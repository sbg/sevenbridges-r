Permission <- setRefClass("Permission", contains = "Item",
                          fields = list(
                              write = "logicalORNULL",
                              copy_permission = "logicalORNULL", #cannot use copy
                              execute = "logicalORNULL",
                              admin = "logicalORNULL",
                          read = "logicalORNULL"),
                          methods = list(
                              initialize = function(write = NULL,
                                  copy_permission = NULL,
                                  execute = NULL,
                                  admin = NULL,
                                  read = NULL, ...){

                                  write <<- write
                                  copy_permission <<- copy_permission
                                  execute <<- execute
                                  admin <<- admin
                                  read <<- read

                                  callSuper(...)
                              },
                              show = function(){
                                  .showFields(.self, "-- Permission --",
                                           c("read", "write", "copy_permission", "execute", "admin"))
                              }
                          ))

Member <- setRefClass("Member", contains = "Item",
                      fields = list(
                          pid = "characterORNULL",
                          id = "characterORNULL",
                          username = "characterORNULL",
                          invitation_pending = "logicalORNULL",
                          permissions = "Permission"
                      ),
                      methods = list(
                          update = function(write = NULL,
                              copy = NULL,
                              execute = NULL,
                              admin = NULL, read = NULL, ...){
                              
                              if(is.null(pid)){
                                  stop("cannot find project id")
                              }
                              body = list('write' = write,
                                  'copy' = copy,
                                  'execute' = execute,
                                  'read' = read, 
                                  'admin' = admin)

                              body <- body[!sapply(body, is.null)]
                              
                              if(length(body) == 0)
                                  stop("please provide updated information")


                             


                              req <- api(token = auth$token,
                                  base_url = auth$url, 
                                  path = paste0('projects/', pid, '/members/', username, '/permissions'),
                                  body = body, method = 'PATCH', ...)
                              
                              res <- status_check(req)

                              ## check new updated info
                              
                              
                              ## update self
                              lst <- res
                              names(lst)[names(lst) == "copy"] <- "copy_permission"
                              nms <- names(lst)

                              ## update object
                              for(nm in nms){
                                  .self$permissions$field(nm, lst[[nm]])
                              }
                              
                              .self
                          },
                          delete = function(...){
                              stopifnot(!is.null(auth$version))
                              
                              req <- api(token = auth$token,
                                         base_url = auth$url,
                                         path = paste0('projects/', pid, '/members/', username),
                                         method = 'DELETE', ...)
                              res <- status_check(req)

                          },
                          show = function(){
                              .showFields(.self, "== Member ==",
                                          values = c("id", "username",
                                                     "invitation_pending"))
                              .self$permissions$show()
                          }
                      ))


## this Project object should support both version
Project <- setRefClass("Project", contains = "Item",
                       fields = list(id = "characterORNULL",
                           name = "characterORNULL",
                           billing_group_id = "characterORNULL", 
                           description = "characterORNULL",
                           type = "characterORNULL", 
                          ##  my_permission = "Permission",
                           owner = "characterORNULL",
                           tags = "listORNULL"),
                       methods = list(
                           initialize = function(id = NULL, name = NULL,
                               billing_group_id = NULL, 
                               description = "",
                               type = "",
                           ##    my_permission = Permission(),
                               owner = NULL,
                               tags = list(), ...){

                               if(is.null(id))
                                   stop("id is required")

                               ## fixme in the future
                               if(length(tags)){
                                   if(tags != "tcga"){
                                       stop("tags has to be empty list() (default) or 'tcga' for now")
                                   }
                               }

                               id <<- id
                               name <<- name
                               description <<- description
                               ## my_permission <<- my_permission
                               type <<- type
                               owner <<- owner
                               tags <<- tags
                               billing_group_id <<- billing_group_id
                               
                               callSuper(...)
                           },
                           update = function(name = NULL, description = NULL, billing_group_id = NULL, ... ){
                               'update name/description/billing group for a project'


                               body = list('name' = name,
                                   'description' = description,
                                   'billing_group' = billing_group_id)

                               body <- body[!sapply(body, is.null)]
                               if(length(body) == 0)
                                   stop("please provide updated information")

                               nms <- names(body)

                               ## update project itself
                               for(nm in nms){
                                .self$field(nm, body[[nm]])
                               }

                               req <- api(token = auth$token,
                                   base_url = auth$url, 
                                   path = paste0('projects/', id),
                                   body = body, method = 'PATCH', ...)

                               res <- status_check(req)
                               res <- .asProject(res)
                               res$auth <- .self$auth
                               res
                           },
                           member = function(username = NULL,
                               name = username,
                               ignore.case = TRUE,
                               exact = FALSE, ...){

                               if(is.null(id))
                                   stop("id must be provided")
                               ## depends on owner information to decide which version we use
                               if(ptype(id) == "1.1"){
                                   ## use V1.1
                                   res <- project_members(auth$token, id)
                                   ms <- .asMemberList(res[[1]])                                   
                               }
                               if(ptype(id) == "v2"){
                                   ## use v2
                                   req = api(token = auth$token,
                                       base_url = auth$url, 
                                       path = paste0('projects/', id, '/members'),
                                       method = 'GET', ...)
                                   res <- status_check(req)
                                   ms <- .asMemberList(res, pid = id)
                                   ms <- setAuth(ms, .self$auth, "Member")
                               }
                               
                               if(is.null(name)){
                                   return(ms)
                               }else{
                                   m <- m.match(ms, name = name,
                                                .name = "username", exact = exact)
                                   return(m)
                               }

                           },
                           member_add = function(
                               username = NULL,
                               name = username,
                               copy = FALSE,
                               write = FALSE,
                               execute = FALSE,
                               admin = FALSE,
                               read = FALSE,
                               ...){

                                   body <- list('username' = name,
                                                'permissions' = list(
                                                    'copy' = copy, 'write' = write,
                                                    'read' = read,
                                                    'execute' = execute, 'admin' = admin))

                                   req = api(token = auth$token,
                                       base_url = auth$url,
                                       path = paste0('projects/', id, '/members'),
                                       body = body, method = 'POST', ...)
                                   
                                   res <- status_check(req)
                                   .asMember(res)

                           },
                           file = function(name = NULL, id = NULL, exact = FALSE, detail = FALSE, ...){
                               res <- auth$file(name = name,
                                                id = id,
                                                project = .self$id,
                                                exact = exact,
                                                detail = detail, ...)
                               res
                           },
                           upload = function(filename = NULL, 
                                             name = NULL,
                                             metadata = list(),
                                             overwrite = FALSE, ...){
                               ## if filename is a list
                               if(length(filename) > 1){
                 
                                   for(fl in filename){
                                       message(fl)
                                       if(file.info(fl)$size > 0){
                                           upload(fl, metadata = metadata, 
                                                  overwrite = overwrite,
                                                  ...)
                                       }else{
                                           warning("skip uploading: empty file")
                                       }
                                      
                                   }
                                   return()
                               }
                               ## if filename is a folder
                               if(!is.na(file.info(filename)$isdir) && file.info(filename)$isdir){
                                   message("Upload all files in the folder: ", filename)
                                   fls <- list.files(filename, recursive = TRUE, full.names = TRUE)
                                   upload(fls, metadata = metadata, 
                                          overwrite = overwrite,
                                          ...)
                               }
                               
                               ## check 
                               if(!file.exists(filename)){
                                   stop("file not found")
                               }
                              
                              
                               u <- Upload(auth = auth,
                                           file = filename,
                                           name = name, 
                                           project_id = id,
                                           metadata = metadata, 
                                           ...)
                               
                               u$upload_file(metadata = metadata, overwrite = overwrite)
                          
                           },
                           ## app
                           app = function(...){
                               auth$app(project = id, ...)
                           },
                           app_add = function(short_name = NULL, filename  = NULL, revision = NULL, ...){
                               
                               if(is.null(filename)){
                                   stop("file (cwl json) need to be provided")
                               }
                               
                               if(is.null(short_name)){
                                   stop("app short name has to be provided (alphanumeric character with no spaces)")
                               }else{
                                   if(grepl("[[:space:]]+", short_name)){
                                       stop("id cannot have white space")
                                   }
                               }
                               
                               if(is(filename, "Tool") || is(filename, "Workflow")){
                                   if(is(filename, "Workflow")){
                                       ## push apps and update run
                                     
                                       steplst <- filename$steps
                                       isSBGApp <- function(x){
                                           length(x$"sbg:id")
                                       }
                                       lst <- lapply(steplst, function(x){
                                           if(!isSBGApp(x$run)){
                                               ## if not exists on sbg platform, need to 
                                               ## add it first
                                               .name <- gsub("#", "",x$run$id)
                                               message(.name)
                                               new.app <- app_add(short_name = .name,
                                                                  filename = x$run)
                                               new.app
                                           }else{
                                               ## SBG id doesn't need to add
                                               ## but need to copy?
                                               x
                                           }
                                       })
                                       ## No need to do this here, should not edit 
                                       ## should assume link exists.
                                      #  slst <- lst[[1]]
                                      #  for(i in 1:(length(lst) -1)){
                                      #      slst <- slst + lst[[i + 1]]
                                      #  }
                                      #  ## udpate steplist
                                      # filename$steps <- slst
                                   }


                                   fl <- tempfile(fileext = ".json")
                                   con <- base::file(fl, raw = TRUE)
                                   writeLines(filename$toJSON(), con = con)
                                   filename <- fl
                                   close(con)
                                   
                               }

                               if(is.null(revision)){
                                   ## latest check revision first
                                   .id <- paste0(id, "/", short_name)
                                   msg <- try(.r <- as.integer(app(id = .id, detail = TRUE)$revision),
                                              silent = TRUE)
                                   if(!inherits(msg, "try-error") && is.integer(.r)){
                                       .r <- .r + 1
                                       message("create new revision ", .r)
                                       res <- auth$api(path = paste0("apps/", id, "/", short_name, "/", .r, "/raw"),
                                                       method = "POST",
                                                       body = upload_file(filename),  ...)
                                   }else{
                                       res <- auth$api(path = paste0("apps/", id, "/", short_name, "/raw"),
                                                       method = "POST",
                                                       body = upload_file(filename), ...)            
                                   }
                                   

                         
            
                               }else{
                                   ## latest check revision first
                                   .id <- paste0(id, "/", short_name)
                                   .r <- as.integer(app(id = .id, detail = TRUE)$revision)
                                   if(revision != .r + 1)
                                       stop("latest revision is: ", .r, ", you have to bump to: ", .r + 1)
                                   res <- auth$api(path = paste0("apps/", id, "/", short_name, "/", revision, "/raw"),
                                                   method = "POST",
                                                   body = upload_file(filename),  ...)
                               }

                               ## file.remove(filename)
                               .id <- res[["sbg:id"]]
                               res <- app(id = .id)
                               ## check error message
                               validateApp(response(res))
                               res

                           },
                           ## task
                           task = function(...){
                               auth$task(project = id, ...)
                           },
                           task_add = function(
                               name = NULL,
                               description = NULL,
                               batch = NULL,
                               app = NULL,
                               inputs = NULL, 
                               input_check = getOption("sevenbridges")$input_check, ...){

                              
                            if(input_check){
                                message("checking inputs ...")
                                message("API: getting app input information ...")
                                apps = auth$app(id = app)
                                message("checking ...")
                                inputs = apps$input_check(inputs)
                            }
                            message("Task drafting ...")
                            
                            if(is.null(inputs)){
                                .i = inputs
                            }else{
                                .i = lapply(inputs, asTaskInput)
                            }
                            
                            body = list(name = name,
                                description = description,
                                project = id,
                                app = app,
                                inputs = .i)
                            
                            if(!is.null(batch)){
                                
                                body = c(batch, body)
      
                            }
                            
                            res <- auth$api(path = "tasks", body = body, method = "POST", ...)
                            res <- .asTask(res)
                            if(length(res$errors)){
                                message("Errors found: please fix it in your script or in the UI")
                                .showList(res$errors)
                            }
                            setAuth(res, .self$auth, "Task")
                               
                           },
                           task_run = function(...){
                               task = Task(auth = .self$auth,
                                           project_id = id, ...)

                               task$run()
                           },
                           delete = function(...){

                                   req <- auth$api(path = paste0('projects/', id), method = 'DELETE', ...)
                                   req


                           },
                           show = function(){
                               .showFields(.self, "== Project ==",
                                           c("id", "name", "description", "billing_group_id", "type",
                                             "owner", "tags"))
                           }
                       ))


.asProject <- function(x){
   ## if(is.null(x$my_permission)){
        Project(id = x$id,
                href = x$href,
                name = x$name,
                type = x$type,
                owner = x$owner,
                tags = x$tags,
                description = x$description, ## v1 only entry
                billing_group_id = x$billing_group, 
                response = response(x))
        
  # ##  }else{
  #       Project(id = x$id,
  #               href = x$href,
  #               name = x$name,
  #               type = x$type,
  #               owner = x$owner,
  #               tags = x$tags,
  #               description = x$description, ## v1 only entry
  #               billing_group_id = x$billing_group,                 
  #               my_permission = do.call(Permission, x$my_permission), ## v1 only entry
  #               response = response(x))
  #       
  #   }
}

ProjectList <- setListClass("Project", contains = "Item0")

.asProjectList <- function(x){
    obj <- ProjectList(lapply(x$items, .asProject))
    obj@href <- x$href
    obj@response <- response(x)
    obj
}

.asMember <- function(x, pid = NULL){
    Member(id = x$id,
           pid = pid, 
           username = x$username,
           invitation_pending = x$invitation_pending,
           permissions = do.call(Permission, x$permissions),
           response = response(x))
}

MemberList <- setListClass("Member", contains = "Item0")

.asMemberList <- function(x, pid = NULL){
    obj <- MemberList(lapply(x$items, .asMember, pid = pid))
    obj@href <- x$href
    obj@response <- response(x)
    obj
}

