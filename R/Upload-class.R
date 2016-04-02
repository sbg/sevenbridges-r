## Upload: Tuesday
## Kind of complex, think about how to make it easier?
Part <- setRefClass("Part", contains = "Item",
                    fields = list(
                        part_number = "numericORNULL",
                        part_size = "numericORNULL",
                        url = "characterORNULL",
                        expires = "characterORNULL",
                        success_codes = "listORNULL",
                        report = "listORNULL",
                        etag = "characterORNULL"),
                    methods = list(
                        initialize = function(part_number = NULL,
                                              part_size = NULL,
                                              url = NULL,
                                              expries = NULL,
                                              success_codes = NULL,
                                              report = NULL,
                                              etag = NULL, ...){

                            .part_number <- as.integer(as.character(part_number))
                            .part_size <- as.integer(as.character(part_size))
                            if(.part_number >  10000 | .part_number <1){
                                stop("par_number has to be a number in the range 1-10000.")
                            }
                            url <<- url
                            part_number <<- .part_number
                            part_size <<- .part_size
                            etag <<- etag
                            expires <<- expires
                            success_codes <<- success_codes
                            report <<- report

                            callSuper(...)
                        },
                        show = function(){
                            .showFields(.self, "== Part ==",
                                        c("part_number", "url"))

                        }
                    ))



Upload <- setRefClass("Upload", contains = "Item",
                      fields = list(
                    
                          file = "characterORNULL",
                          project_id = "characterORNULL",
                          name = "characterORNULL",
                          size = "numericORNULL",
                          part_size = "numericORNULL",
                          upload_id = "characterORNULL",
                          part = "list",
                          part_length = "integer",
                          part_finished = "integer",
                          initialized = "logical",
                          parallel_uploads = "logicalORNULL",
                          metadata = "Metadata"
                      ),
                      methods = list(
                          initialize = function(
                              file = NULL,
                              project_id = NULL,
                              name = NULL,
                              size = NULL,
                              part_size = NULL,
                              part_finished = 0L,
                              initialized = FALSE,
                              part_length = NULL,
                              parallel_uploads = NULL, 
                              metadata = list(), ...){

                              metadata <<- normalizeMeta(metadata)

                              parallel_uploads <<-  parallel_uploads
                              initialized <<- initialized
                              part_finished <<- part_finished
                              ## validation
                              stopifnot_provided(!is.null(file))

                              file <<- normalizePath(file)

                              if(!file.exists(file)){
                                  stop("file doesn't exist, please provide relative or aboslution path to the file")
                              }

                              if(is.null(name)){
                                  name <<- basename(file)
                              }else{
                                  name <<- name
                              }


                              if(is.null(size)){
                                  ## file.zie is R 3.2
                                  ## to be compatible
                                  ## size <<- file.size(file)
                                  size <<- file.info(file)$size
                              }else{
                                  size <<- size
                              }

                              stopifnot_provided(!is.null(project_id))


                              if(is.numeric(.self$size)){
                                  if(!(.self$size <= 5497558138880 &
                                           .self$size >= 0))
                                      stop("size must be between 0 - 5497558138880, inclusive")
                              }else{
                                  stop("size must be between 0 - 5497558138880, inclusive")
                              }


                              project_id <<- project_id
                              ## fixme: try manual part-size
                              if(is.null(part_size))
                                  if(is.null(part_length)){
                                      if(is.null(part_size)){
                                          part_size <<- as.integer(5 * 1024^2)
                                      }
                                      part_length <<- as.integer(ceiling(.self$size/.self$part_size))
                                  }else{
                                      ## go with priority part_length
                                      ## let's reuire integer here
                                      part_size <<- as.integer(ceiling(.self$size/part_length))
                                      ## round the length number
                                      part_length <<- as.integer(ceiling(.self$size/.self$part_size))

                                  }

                              .part_size <- rep(.self$part_size, .self$part_length)
                              ## last part
                              .part_size[.self$part_length] <- .self$size -
                                  .self$part_size * (.self$part_length - 1)

                              part <<- vector("list", .self$part_length)

                              part <<- lapply(1:.self$part_length, function(idx){
                                  Part(part_number = idx,
                                       part_size = .part_size[idx])
                              })
                              if(.self$part_length == 1){
                                  .self$part_size <<- .self$size
                              }
                              callSuper(...)
                          },
                          upload_init = function(overwrite = FALSE, ...){

                              body = list('project' = project_id, 
                                              'name' = name, 
                                              'size' = size,
                                              'part_size' = part_size)
                              
                              res <- auth$api(path = "upload/multipart", 
                                              query = list(overwrite = overwrite), 
                                              body = body, method = 'POST', ...)
                          
                              upload_id <<- res$upload_id
                       
                              initialized <<- TRUE
                              part_size <<- res$part_size
                              parallel_uploads <<-  res$parallel_uploads
                              message("Initialized")
                              invisible(res)
                          },
                          upload_info = function(list_parts = TRUE, ...){
                              if(is.null(upload_id)){
                                  stop("Upload is not initialized yet")
                              }
                              res <- auth$api(path = paste0("upload/multipart/",
                                                     upload_id), 
                                              query = list(list_parts = list_parts),
                                              method = "GET")
                              
                           
                              # show()
                              # invisible(res)
                          },
                          upload_info_part = function(part_number = NULL, ...){
                              stopifnot_provided(!is.null(part_number))
                              if(part_number >  10000 | part_number <1){
                                  stop("part_number has to be a number in the range 1- 10000.")
                              }

                              ## cl <- c("Content-Length" = as.character(part[[part_number]]$part_size))
                              res <- auth$api(path = paste0("upload/multipart/",
                                                     upload_id, "/part/", part_number), 
                                              method = "GET")

                              part[[part_number]]$url <<- res$url
                              part[[part_number]]$etag <<- res$etag
                              part[[part_number]]$response <<- response(res)
                              part[[part_number]]$expires <<- res$expries
                              part[[part_number]]$success_codes <<- res$success_codes
                              part[[part_number]]$report <<- res$report
                              res
                          },
                          upload_file = function(metadata = list(), overwrite = FALSE){
                            
                              ## make this one easy to use
                              N <- part_length
                              res <- upload_init(overwrite = overwrite)
                          
                              pb <- txtProgressBar(min = 0, max = N, style = 3)
                        
                              con <- file(file, "rb")
                             
                              for(i in 1:N){
                           
                                  p <- upload_info_part(i)
                                  url <- p$url
                                  b <- readBin(con, "raw", part_size)
                                  res <- PUT(url, body = b)
                                  rm(b)

                     
                                  etag <- headers(res)$etag
                                  
                                  part[[i]]$etag <<- etag
                                  upload_complete_part(i, etag)
                                  # part_finished <<- as.integer(i)
                                  setTxtProgressBar(pb, i)
                              }
                              close(pb)
                              res <- upload_complete_all()
                              close(con)
                              message("file uploading complete")

                              ## when we complete we could add meta
                              # meta <- .self$metadata$asList()
                              fl.id <- res$id
                              fl.meta <- paste0(file, ".meta")
                              if(length(metadata)){
                                  if(file.exists(fl.meta)){
                                      message("Ignore meta file: ", fl.meta)
                                     
                                  }
                                  message("Adding metadata ...")
                                  auth$file(id = fl.id)$setMeta(metadata)
                                  message("Metadata complete")
                                
                                  metadata <<- normalizeMeta(metadata)
                         
                              }else{
                                 
                                  if(file.exists(fl.meta)){
                                      message("loading meta from: ", fl.meta)
                                      metalist <- jsonlite::fromJSON(fl.meta)
                                      auth$file(id = fl.id)$setMeta(metalist)
                                      # browser()
                                      # metalist
                                      # do.call(Metadata, metalist)
                                      # 
                                      # metadata <<- do.call(Metadata, metalist)
                                      metadata <<- normalizeMeta(metalist)
                                  }
                              }
                              res <- .asFiles(res)
                              invisible(res)
                          },
                          upload_complete_part = function(part_number = NULL,
                                                          etag = NULL){
                              
                              body = list(
                                  part_number = unbox(part_number),
                                  response = list(headers = list(ETag = unbox(etag)))
                              )
                              
                              res <- auth$api(path = paste0("upload/multipart/",
                                                             upload_id, "/part"),
                                              body = body,
                                              method = "POST")

                          },
                          upload_complete_all = function(...){
                              ## fixme:
                              pl <- lapply(part, function(p){
                                  list(part_number = unbox(p$part_number),
                                       response = list(headers = list(ETag = unbox(p$etag))))
                              })
                              body = list(parts = pl)
                 
                              res <- auth$api(path = paste0("upload/multipart/",
                                                          upload_id, "/complete"),
                                              body = body,
                                            method = "POST", ...)
                          },
                          upload_delete = function(){

                              auth$api(path = paste0("/upload/multipart/", upload_id),
                                       method = "DELETE")

                          },
                          show = function(){
                              .showFields(.self, "== Upload ==",
                                          c("initialized", "part_length",
                                            "part_finished",
                                            "project_id", "name",
                                            "size", "part_size", "upload_id"))
                          }
                      ))
## define alias
um <- Upload@generator$def@refMethods
Upload$methods(list(
    init = um$upload_init,
    info = um$upload_info,
    info_part = um$upload_info_part,
    delete = um$upload_delete,
    upload = um$upload_file
))


.asUpload <- function(x){
    Upload(
        ## auth = Auth(x$token),
        project_id = x$project_id,
        name = x$name,
        size = x$size,
        part_size = x$part_size,
        response = response(x))
}
.asUploadList <- function(x){
    lapply(x, .asUpload)
}
