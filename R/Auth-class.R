##' Class Auth
##'
##' Auth token object
##'
##' Every object could be requested from this Auth object and any action
##' could start from this object using cascading style. Please check vignette
##' 'easy-api' for more information.
##'
##' @field token [character] your auth token.
##' @field url [character] basic url used for API, by default
##' it's \url{https://api.sbgenomics.com/1.1/}
##' @field version url version number
##' @field platform which platform you are using, by default it is cgc platform, it tries
##' to search your .sbg.auth.yml file to parse platform id your deinfed there.
##' @field fs FS object, for mount and unmount file system.
##'
##'
##' @importFrom stringr str_match 
##'
##' @export Auth
##' @exportClass Auth
##' @examples
##' ## replace it with real token
##' token <- "aef7e9e3f6c54fb1b338ac4ecddf1a56"
##' a <- Auth(token)
Auth <- setRefClass("Auth", fields = list(token = "character",
                                url = "character",
                                version = "character",
                                platform = "characterORNULL",
                                fs = "FSORNULL"),
                    methods = list(
                        initialize = function(
                            token = NULL,
                            url = NULL,
                            platform = NULL,
                            username = NULL, 
                            fs = NULL,
                            ...){

                            ## get API URL first
                            ## logic:
                            ## no url, guess from platform, no platform, retrieve first entry, nothing, error.

                            .default.url <- "https://cgc-api.sbgenomics.com/v2/"

                            platform <<- platform
                            fs <<- fs
                            if(is.null(url)){
                                if(is.null(platform)){
                                    ## try to get token from config and option
                                    .p <- getToken(platform = platform)
                                    if(is.null(.p)){
                                        # get nothing preset
                                        if(is.null(token)){
                                            stop("please provide url and token")
                                        }else{
                                            message("url not provied, use: ", .default.url)
                                            url <<- .default.url
                                        }
                                            
                                    }else{
                                        .url <- .p[[1]]$url
                                        if(is.null(.url)){
                                            stop("you config file is wrong, don't have url")
                                        }else{
                                            platform <<- names(.p)[1]
                                            url <<-  .url
                                        }
                                    }
                                }else{
                                    .p <- getToken(platform = platform)
                                    if(is.null(.p)){
                                        ## try match default public platform
                                        if(platform %in% c("cgc", "us", "china", "gcp")){
                                            url <<- switch(platform,
                                                           'us' = "https://api.sbgenomics.com/v2/",
                                                           'cgc'= "https://cgc-api.sbgenomics.com/v2/",
                                                           'gcp' = "https://gcp-api.sbgenomics.com/v2/",
                                                           'china' = "https://cn-api.sbgenomics.com/v2/")
                                        }else{
                                            stop("platform doesn't exist, please setup config or provide url")
                                        }
                                    }else{
                                        ## exist in config file
                                        .url <- .p$url
                                        if(is.null(.url)){
                                            stop("you config file is wrong, don't have url")
                                        }else{
                                            url <<-  .url
                                        }
                                    }
                                }
                            }else{
                                url <<- url
                            }
                            url <<- normalizeUrl(.self$url)
                            ## we should know platform or ulr at least
                            ## get token
                            .token <- NULL
                            if(is.null(token)){
                                ## try to read from config first
                                if(!is.null(username)){
                                    .token <- getToken(platform = platform, username = username)
                                    token <<- .token
                                }else{
                                    ## use the first token and user
                                    .p <- getToken(platform = platform)
                                    if(!is.null(.p[[1]])){
                                        ## use the first one
                                        message("use username: ", names(.p$user)[1])
                                        .token <- .p$user[[1]]$token
                                    }else{
                                        stop("cannot find any existing authentification information.")
                                    }
                                    if(is.null(.token)){
                                        stop("cannot set token")
                                    }else{
                                        token <<- .token
                                    }
                                }
                            }else{
                                token <<- token                                
                            }

                        },
                        project_owner = function(owner = NULL, ...){
                            'List the projects owned by and accessible to a particular user.
                             Each project\'s ID and URL will be returned.'
                            
                            if(is.null(owner)){
                                stop("owner must be provided. For example, Nate. ")
                            }

                            req <- api(token = token,
                                          base_url = url, 
                                          path = paste0('projects/', owner),
                                          method = 'GET', ...)
                            res <- status_check(req)
                            if(hasItems(res)){
                                rp <- parseItem(res)
                                obj <- .asProjectList(rp)
                            }else{
                                message("not found")
                                obj <- res
                            }
                             obj <- setAuth(obj, .self, "Project")
                            
                        }, 
                        project_new = function(name = NULL,
                            billing_group_id = NULL,                            
                            description = name,
                            tags = list(), type = "v2", ...){
                            
                            'Create new projects, required parameters: name, billing_group_id,
optional parameteres: tags and description, type. '

                            if (is.null(name) || is.null(billing_group_id))
                                stop('name, description, and billing_group_id must be provided')
                            
                            ## check tags
                            if(is.character(tags)){
                                tags <- as.list(tags)
                            }

                            body = list('name' = name,
                                'type' = type, 
                                'description' = description,
                                'tags' = tags,
                                'billing_group' = billing_group_id)


                            res <- api(path = 'projects', body = body,
                                       method = 'POST', ...)

                            res <- .asProject(res)
                            res <- setAuth(res, .self, "Project")
                        },
                        ## Project call                        
                        project = function(name = NULL, id = NULL,
                            index = NULL, ignore.case = TRUE,
                            exact = FALSE, owner = NULL, detail = FALSE, ...){
                            
                            'If no id or name provided, this call returns a list of all projects you are a member of. Each project\'s project_id and URL on the CGC will be returned. If name or id provided, we did a match search the list'

                            if(!is.null(id)){
                                req <- api(path = paste0("projects/", id), method = "GET",  ...)
                                res <- .asProject(req)
                                res <- setAuth(res, .self, "Project")
                                return(res)                                
                            }


                            ## check owner
                            if(is.null(owner)){
                                ## show all projects
                                req <- api(path = "projects", method = "GET", ...)
                                res <- .asProjectList(req)
                            }else{
                                message("Owner: ", owner)
                                req <- api(path = paste0("projects/", owner),
                                           method = "GET", ...)
                                res <- .asProjectList(req)
                            }

                            
                            res <- m.match(res, id = id, name = name, exact = exact,
                                           ignore.case = ignore.case)

                            if(!length(res)) return(NULL)
                            
                            ## if(length(res) == 1){
                           ##     .id <- res$id
                            ##     req <- api(path = paste0("projects/", .id), method = "GET",  ...)
                            ##     res <- .asProject(req)
                            ##     res <- setAuth(res, .self, "Project")
                            ##     return(res)                                
                            ## }

                            
                            if(detail && length(res)){
                                if(is(res, "SimpleList")){
                                    ids <- sapply(res, function(x){ x$id })
                                }else{
                                    ids <- res$id
                                }

                                lst <- lapply(ids, function(id){
                                    req <- api(path = paste0("projects/", id), method = "GET", ...)
                                    .asProject(req)
                                })
                                res <- ProjectList(lst)
                            }
                            
                            ## double check
                            if(length(res) == 1 && is(res, "SimpleList")){
                                res <- res[[1]]
                            }
                            res <- setAuth(res, .self, "Project")
                            res
                               
                        },
                        billing = function(id = NULL, breakdown = FALSE, ...){
                            'if no id provided, This call returns a list of paths used to access billing
information via the API. else, This call lists all your billing groups, including groups that are pending or have been disabled.
if breakdown = TRUE, This call returns a breakdown of spending per-project for the billing group specified by billing_group. For each project that the billing group is associated with, information is shown on the tasks run, including their initiating user (the runner), start and end times, and cost.
'
                            if(is.null(id)){
                                ## show api
                                req <- api(path = 'billing/groups', method = 'GET', ...)
                                req <- .asBillingList(req)
                                if(length(req) == 1 && is(req, "SimpleList")){
                                    req <- req[[1]]
                                }
                                return(req)
                                
                            }else{
                                

                                if(breakdown){
                                    req = api(path = paste0('billing/groups/', id, "/breakdown"),
                                        method = 'GET', ...)
                                }else{
                                    req = api(path = paste0('billing/groups/', id), method = 'GET', ...)

                                }
                                req <- .asBilling(req)
                                
                                return(req)
                            }
                        },
                        invoice = function(id = NULL, ...){
                            'no id provided, This call returns a list of invoices, with information about each, including whether or not the invoice is pending and the billing period it covers.

The call returns information about all your available invoices, unless you use the query parameter bg_id to specify the ID of a particular billing group, in which case it will return the invoice incurred by that billing group only.

if id provided, This call retrieves information about a selected invoice, including the costs for analysis and storage, and the invoice period.
'

                            if(is.null(id)){
                                req = api(path = 'billing/invoices', method = 'GET', ...)    
                            }else{
                                req = api(path = paste0('billing/invoices/', id), method = 'GET', ...)    
                            }
                            req

                        },
                        api = function(..., limit = getOption("sevenbridges")$limit,
                            offset = getOption("sevenbridges")$offset,
                            complete = FALSE){
                            'This call returns all API paths, and pass arguments to api() function and input token and url automatically'
                            req <- sevenbridges::api(token, base_url = url, limit = limit, offset = offset, ...)
                            req <- status_check(req)
                           
                            if(complete){
                                N <- as.numeric(headers(response(req))[["x-total-matching-query"]])
                                if(length(N)){
                                    .item <- length(req$items)

                                }
                                if(.item < N){
                                    pb <- txtProgressBar(min = 1, max = N%/%100 + 1, style = 3)
                                    res <- NULL
                         
                                    
                                    for(i in 1:(N%/%100 + 1)){
                                        .limit = 100 
                                        .offset = (i-1) * 100
                                        req <- sevenbridges::api(token, base_url = url,
                                                                 limit = .limit, offset = .offset, ...)
                                        req <- status_check(req)
                                        res$items <- c(res$items, req$items)
                                        setTxtProgressBar(pb, i)
                                    }
                                    cat("\n")
                                    res$href = NULL
                                }else{
                                    return(req)
                                }
                                return(res)
                            }else{
                                return(req)
                            }
                        },
                        show = function(){
                            .showFields(.self, "== Auth ==",
                                        values = c("token", "url"))
                        },
                        ## v2 only feature
                        rate_limit = function(...){
                            'This call returns information about your current 
                            rate limit. This is the number of API calls you can 
                            make in one hour.'
                            
                          
                            
                            req <- api(path = "rate_limit", method = "GET", ...)
                                                  
                            .asRate(req)
                        
                            
                        },
                        user = function(username = NULL, ...){
                            'This call returns a list of the resources, such as projects, 
                            billing groups, and organizations, that are accessible to you.
                            
                            If you are not an administrator, this call will only return a 
                            successful response if {username} is replaced with your own 
                            username. If you are an administrator, you can replace 
                            {username} with the username of any CGC user, to return 
                            information on their resources.'
                            
                            
                            if(is.null(username)){
                                req <- api(token = token, 
                                       path = "user/",
                                       method = "GET", ...)
                                message("username is not provided, show run user information instead")
                            }else{
                                
                                req <- api(token = token, 
                                           path = paste0("users/", username), 
                                           method = "GET", ...)
                            }

                            .asUser(req)
                        },
                        ## File API
                        file = function(name = NULL, id = NULL, project = NULL,
                            exact = FALSE, detail = FALSE,  
                            metadata = list(), 
                            origin.task = NULL, 
                            complete = FALSE, 
                            search.engine = c("server", "brute"), ...){
                            'This call returns a list of all files in a specified project that you can access. For each file, the call returns: 1) Its ID 2) Its filename The project is specified as a query parameter in the call.'

                            search.engine = match.arg(search.engine)
                            
                            if(is.null(id)){
                                if(is.null(project)){
                                    stop("When file id is not provided, project id need to be provided.")
                                }                                
                            }else{
                                if(length(id) > 1){
                                    res <- iterId(id, .self$file, exact = exact, ...)
                                    return(res)
                                }
                                req <- api(path = paste0("files/", id), method = "GET", ...)
                                res <- .asFiles(req)
                                res <- setAuth(res, .self, "Files")
                                return(res)                                
                            }

                            .query <- list(project = project)
                            if(length(metadata)){
                                new.meta <- unlist(metadata)
                                names(new.meta) <- sapply(names(new.meta), 
                                                          function(nm) paste("metadata", nm, sep = "."))
                                
                                .query <- c(.query, as.list(new.meta))
                            }

                            if(!is.null(origin.task)){
                                .query <- c(.query, list(origin.task = origin.task))
                            }
                         
                            
                          
                            
                            if(is.null(name)){
                                ## if no id, no name, list all 
                                req <- api(path = 'files',  method = 'GET', 
                                           query = .query, complete = complete, ...)
                                
                                res <- .asFilesList(req)
                                
                                
                                res <- setAuth(res, .self, "Files")  
                                if(length(res) == 1){
                                    return(res[[1]]) 
                                }else{
                                    return(res)
                                }
                                
                            }

                            ## search now by name
                            ## get all files
                            switch(search.engine, 
                                   server = {
                                       if(exact){
                                           .query = c(list(name = name), .query)
                                           
                                           req <- api(path = 'files',  method = 'GET', 
                                                      query = .query, complete = FALSE, ...)
                                           
                                           res <- .asFilesList(req)[[1]] 
                                       }else{
                                           ## message("using 'brute' for name pattern matching, please use exact = TRUE if that's full exact name.")
                                           req <- api(path = 'files',  method = 'GET', 
                                                      query = .query, complete = complete, ...)
                                           
                                           res <- .asFilesList(req)
                                           res <- m.match(res, id = id, name = name, exact = exact) 
                                       }
                                       
                                   },
                                   brute = {
                                       req <- api(path = 'files',  method = 'GET', 
                                                  query = .query, complete = complete, ...)
                                       
                                       res <- .asFilesList(req)
                                       res <- m.match(res, id = id, name = name, exact = exact)
                                   })
                            
                            
                            
                            
                            
                            if(length(res)){
                                if(detail){
                                    if(is(res, "FilesList")){
                                        ids <- sapply(res, function(x){ x$id })
                                    }else{
                                        ids <- res$id
                                    }
                                    lst <- lapply(ids, function(id){
                                        req <- api(path = paste0("files/", id), method = "GET", ...)
                                        .asFiles(req)
                                    })
                                    res <- FilesList(lst)
                                }
                            }else{
                                return(NULL)
                            }

                            
                            res <- setAuth(res, .self, "Files")
                            res
                        },
                        public_file = function(...){
                            file(project = "admin/sbg-public-data", ...)
                        },
                        copyFile = function(id, project = NULL, name = ""){
                            if(is.null(project))
                                stop("project ID need to be provided, to which the file is copied to")


                            ## iteratively
                            if(length(id) > 1){
                                ids <- as.character(id)
                                for(i in ids){
                                    message("copying: ", i)
                                    copyFile(i, project = project, name = name)
                                }
                            }else{
                                
                                body = list(project = project,
                                    name = name)
                                
                                res <- api(path = paste0("files/", id, "/actions/copy"),
                                           body = body, method = "POST")

                                res <- .asFiles(res)
                                setAuth(res, .self, "Files")
                            }
                        },
                        copy_file = function(...){
                            copyFile(...)
                        },
                        ## App API
                        app = function(name = NULL,
                            id = NULL,
                            exact = FALSE,
                            ignore.case = TRUE,
                            detail = FALSE,
                            project = NULL,
                            query = NULL,
                            visibility = c("project", "public"),
                            revision = NULL,
                            complete = FALSE, 
                            ...){

                            visibility <- match.arg(visibility)

                            if(visibility == "public"){
                                
                                query <- c(query, list(visibility = "public"))
                            }

                            ## if id specified, doesn't have to list all
                            
                            if(!is.null(id)){
                                req <- api(path = paste0("apps/", .update_revision(id, revision)),
                                           method = "GET", query = query, ...)
                           
                                return(setAuth(.asApp(req), .self, "App"))
                            }


                            ## list all apps first
                            if(is.null(project)){
                                req <- api(path = "apps", method = "GET",
                                           query = query, complete = complete, ...)
                                # browser()
                                # if(complete){
                                #     
                                #     res <- lapply(req$it, function(x){
                                #         as.list(.asAppList(x))
                                #     })
                                #     res <- do.call(c, res)
                                #     res <- do.call(AppList, res)
                                # }else{
                                # res <- .asAppList(req)
                                # }
                            }else{
                                req <- api(path = "apps", method = "GET",
                                           query = c(list(project = project), query),
                                           complete = complete,
                                           ...)
                                # if(complete){
                                #     res <- lapply(req, function(x){
                                #         as.list(.asAppList(x))
                                #     })
                                #     res <- do.call(c, res)
                                #     res <- do.call(AppList, res)
                                # }else{
                                    # res <- .asAppList(req)
                                #}
                            }
                            res <- .asAppList(req)
                            ## match
                            res <- m.match(res, id = id, name = name, exact = exact,
                                           ignore.case = ignore.case)

                            if(length(res) == 1){
                                .id <- res$id
                                req <- api(path = paste0("apps/", .update_revision(.id, revision)),
                                           method = "GET", query = query, ...)
                                res <- .asApp(req)
                                return(setAuth(res, .self, "App"))
                            }
                                                            
                            if(detail && length(res)){
                                if(is(res, "AppList")){
                                    ids <- sapply(res, function(x){ x$id })
                                }else{
                                    ids <- res$id
                                }

                                lst <- lapply(ids, function(id){
                                    if(is.null(project)){
                                        req <- api(path = paste0("apps/", id), 
                                                   query = query,
                                                   method = "GET", ...)
                                    }else{
                                        req <- api(path = paste0("apps/", id), method = "GET", 
                                                   query = c(list(project = project), query),
                                                   ...)                                
                                    }

                                    .asApp(req)
                                })
                                res <- AppList(lst)
                            }


                            if(!length(res)) return(NULL)
                            
                            setAuth(res, .self, "App")
                        },
                        public_app = function(...){
                            app(visibility = "public", ...)
                        },
                        copyApp = function(id, project = NULL, name = ""){
                            if(is.null(project))
                                stop("project ID need to be provided, to which the file is copied to")


                            ## iteratively
                            if(length(id) > 1){
                                ids <- as.character(id)
                                for(i in ids){
                                    message("copying: ", i)
                                    copyApp(i, project = project, name = name)
                                }
                            }else{
                                
                                body = list(project = project,
                                    name = name)
                                
                                res <- api(path = paste0("apps/", id, "/actions/copy"),
                                           body = body, method = "POST")

                                res <- .asApp(res)
                                setAuth(res, .self, "App")
                            }
                        },
                        task = function(name = NULL,
                            id = NULL, project = NULL,
                            exact = FALSE, detail = FALSE,
                            status = c("all", "queued", "draft", "running", "completed", "aborted", "failed"),...){

                            status <- match.arg(status)

                            if(!is.null(id)){
                                req <- api(path = paste0("tasks/", id), method = "GET",  ...)
                                res <- .asTask(req)
                                res <- setAuth(res, .self, "Task")
                                return(res)                                
                            }

                            if(is.null(project)){
                                ## list all files
                                if(status == "all"){
                                    req <- api(path = 'tasks',  method = 'GET', ...)
                                }else{
                                    req <- api(path = 'tasks',  method = 'GET', query = list(status = status), ...)
                                }
                            }else{
                                ## list all files
                                if(status == "all"){
                                    req <- api(path = paste0("projects/", project, "/tasks"),
                                               method = 'GET', , ...)
                                    ## req <- api(path = 'tasks',  method = 'GET', query = list(project = project), ...)
                                }else{
                                    req <- api(path = paste0("projects/", project, "/tasks"),
                                               method = 'GET',
                                               query = list(status = status), ...)

                                }
                            }

                            res <- .asTaskList(req)

                            ## matching
                            res <- m.match(res, id = id, name = name, exact = exact)

                            ## if(length(res) == 1){
                            ##     .id <- res$id
                            ##     req <- api(path = paste0("tasks/", .id), method = "GET",  ...)
                            ##     res <- .asTask(req)
                            ##     res <- setAuth(res, .self, "Task")
                            ##     return(res)                                
                            ## }

                            if(length(res)){
                                if(detail){
                                    if(is(res, "TaskList")){
                                        ids <- sapply(res, function(x){ x$id })
                                    }else{
                                        ids <- res$id
                                    }
                                    lst <- lapply(ids, function(id){
                                        req <- api(path = paste0("taskss/", id), method = "GET", ...)
                                        .asTask(req)
                                    })
                                    res <- TasksList(lst)
                                }
                            }else{
                                return(NULL)
                            }

                            
                            res <- setAuth(res, .self, "Task")
                            res
                        },
                        mount = function(mountPoint = NULL, 
                                         projectId = NULL, 
                                         ignore.stdout = TRUE, 
                                         sudo = TRUE, ...){
                            fs <<- FS(authToken = token, ...)
                            fs$mount(mountPoint = mountPoint, 
                                     projectId = projectId, 
                                     ignore.stdout = ignore.stdout, 
                                     sudo = sudo)
                        },
                        unmount = function(...){
                            fs$unmount(...)
                        },
                        get_id_from_path = function(p){
                            ids <- a$api(path = 'action/files/get_ids', 
                                         method = "POST", 
                                         body = as.list(p))
                            idx <- unlist(lapply(ids, is.null))
                            if(sum(idx)){
                                message("no id for following file: \n", paste(df.path[idx], collapse = "\n"))
                            }
                            if(sum(!idx)){
                                id.valid <- unlist(ids[!idx])
                            }else{
                                id.valid <- NULL
                            }
                            id.valid
                        }, 
                        add_volume = function(name = NULL, 
                                              type = c("s3", "gcs"), 
                                              root_url = NULL, 
                                              bucket = NULL, 
                                              prefix = "",
                                              access_key_id = NULL,
                                              secret_access_key = NULL, 
                                              client_email = NULL, 
                                              private_key = NULL, 
                                              sse_algorithm = "AES256", 
                                              aws_canned_acl = NULL, 
                                              access_mode = c("RW", "RO")){
                            
                            if(is.null(name)){
                                stop("Please provide name, the name of the volume. It must be unique from all other volumes for this user.")
                            }
                            
                            type = match.arg(type)
                            access_mode = match.arg(access_mode)
                            
                            if(is.null(root_url)){
                                root_url = switch(type, 
                                                  s3 = "https://s3.amazonaws.com",
                                                  gcs = "https://www.googleapis.com/")
                            }
                            
                            
                            if(type == "s3" && !is.null(access_key_id) && !is.null(secret_access_key)){
                                credentials = list(
                                    access_key_id = access_key_id,
                                    secret_access_key = secret_access_key
                                )
                            }else if(type == "gcs" && !is.null(client_email) && !is.null(private_key)){
                                credentials = list(
                                    client_email = client_email,
                                    private_key = private_key
                                )
                            }else{
                                stop("credials are needed")
                            }
                            
                            body = list(
                                name = name,
                                service = list(
                                    type = type,
                                    bucket = bucket,
                                    prefix = prefix,
                                    credentials = credentials,
                                    properties = list(
                                        sse_algorithm = sse_algorithm
                                    )
                                ),
                                access_mode = access_mode
                            )
                            
                            res = api(path = "storage/volumes", body = body, method = "POST")    
                            res = .asVolume(res)
                            res = setAuth(res, .self, "Volume")
                            res
                        }, 
                        volume = function(name = NULL, id = NULL,
                                           index = NULL, ignore.case = TRUE,
                                           exact = FALSE, detail = FALSE, ...){
                            
                            'If no id or name provided, this call returns a list of all volumes you are a member of. If name or id provided, we did a match search the list'
                            
                            if(!is.null(id)){
                                req <- api(path = paste0("storage/volumes/", id), method = "GET",  ...)
                                res <- .asVolume(req)
                                res <- setAuth(res, .self, "Volume")
                                return(res)                                
                            }
                            
                            ## list "all"
                            req <- api(path = "storage/volumes", method = "GET", ...)
                            res <- .asVolumeList(req)
                            if(is.null(name)){
                                res <- setAuth(res, .self, "Volume")
                                return(res)
                            }
                            
                            
                            res <- m.match(res, id = id, name = name, exact = exact,
                                           ignore.case = ignore.case)
                            
                            if(!length(res)) return(NULL)

                            
                            if(detail && length(res)){
                                if(is(res, "SimpleList")){
                                    ids <- sapply(res, function(x){ x$id })
                                }else{
                                    ids <- res$id
                                }
                                
                                lst <- lapply(ids, function(id){
                                    req <- api(path = paste0("storage/volumes/", id), method = "GET", ...)
                                    .asVolume(req)
                                })
                                res <- VolumeList(lst)
                            }
                            
                            ## double check
                            if(length(res) == 1 && is(res, "SimpleList")){
                                res <- res[[1]]
                            }
                            res <- setAuth(res, .self, "Volume")
                            res
                       
                        }
                        
                    ))


setClassUnion("AuthORNULL", c("Auth", "NULL"))

                                
                               
#' get Token
#'
#' get Token from config files and option list
#'
#' Current config file is set on home directory with the name .sbg.auth.yml
#' @param platform which platform you are using, by default it is cgc platform, it tries
#' to search your .sbg.auth.yml file to parse platform id your deinfed there.
#' @param username username defined in .sbg.auth.yml file
#' 
#' @rdname Auth-class
#' @aliases getToken
#' @return a token string.
#' @export getToken 
getToken <- function(platform = NULL, username = NULL){
    o <- options("sevenbridges")$sevenbridges$auth
    if(is.null(o)){
 
        o <- .parseToken()
    }
    if(is.null(platform)){
        return(o)
    }else{
        if(is.null(username)){
            o <- options("sevenbridges")$sevenbridges$auth[[platform]]
        }else{
            o <- options("sevenbridges")$sevenbridges$auth[[platform]]$user[[username]]$token            
        }
    }
    o
}

.parseToken <- function(f = ".sbg.auth.yml", p = path.expand("~")){
    fl <- file.path(p, f)
    if(file.exists(fl)){
        res <- try(yaml.load_file(fl), silent = TRUE)  
        if(inherits(res, "try-error")){
            warning("something wrong with your auth config file")
            message("try debug with yaml.load_file")
            res <- NULL
        }
    }else{
        message("configuration file: ", fl, " not found")
        res <- NULL
    }
    res
}



setAuth <- function(res, auth, className = NULL){
    stopifnot(!is.null(className))
    rps <- response(res)
    if(is(res, className)){
        res$auth <- auth
    }else if(is(res, "SimpleList")){
        res <- endoapply(res, function(x){
            x$auth <- auth
            x
        })
    }
    response(res) <- rps
    res
}


#' Read Auth config file to options 
#' 
#' @param config file location, default is ".sbg.auth.yml" at home folder. 
#' @rdname Auth-class
#' @aliases updateAuthList
#' @export updateAuthList
updateAuthList <- function(config = ".sbg.auth.yml"){
    lst <- getOption("sevenbridges")
    lst$auth <- suppressMessages(.parseToken(config))
    cat(as.yaml(lst$auth))
    options(sevenbridges = lst)
}


