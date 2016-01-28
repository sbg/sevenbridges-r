## Upload: Tuesday
## Kind of complex, think about how to make it easier?
Part <- setRefClass("Part", contains = "Item",
                    fields = list(
                        part_number = "numericORNULL",
                        part_size = "numericORNULL",
                        uri = "characterORNULL",
                        etag = "characterORNULL"),
                    methods = list(
                        initialize = function(part_number = NULL,
                                              part_size = NULL,
                                              uri = NULL,
                                              etag = NULL, ...){

                            .part_number <- as.integer(as.character(part_number))
                            .part_size <- as.integer(as.character(part_size))
                            if(.part_number >  10000 | .part_number <1){
                                stop("par_number has to be a number in the range 1-10000.")
                            }
                            uri <<- uri
                            part_number <<- .part_number
                            part_size <<- .part_size
                            etag <<- etag

                            callSuper(...)
                        },
                        show = function(){
                            .showFields(.self, "== Part ==",
                                        c("part_number", "uri"))

                        }
                    ))



Upload <- setRefClass("Upload", contains = "Item",
                      fields = list(
                          ## filepath = "characterORNULL",
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
                              metadata = list(), ...){

                              metadata <<- normalizeMeta(metadata)

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
                          upload_init = function(){
                              res <- sevenbridges::upload_init(token = auth$token,
                                                       project_id = project_id,
                                                       name = name,
                                                       size = size,
                                                       base_url = auth$url)

                              upload_id <<- res$upload_id
                              initialized <<- TRUE
                              message("Initialized")
                              invisible(res)
                          },
                          upload_info = function(){
                              if(is.null(upload_id)){
                                  stop("Upload is not initialized yet")
                              }
                              res <- sevenbridges::upload_info(auth$token, upload_id,
                                                       base_url = auth$url)
                              show()
                              invisible(res)
                          },
                          upload_info_part = function(part_number = NULL){
                              stopifnot_provided(!is.null(part_number))
                              if(part_number >  10000 | part_number <1){
                                  stop("par_number has to be a number in the range 1- 10000.")
                              }
                              cl <- c("Content-Length" = as.character(part[[part_number]]$part_size))
                              res <- status_check(api(auth$token,
                                                         base_url = auth$url,
                                                         path = paste0("upload/multipart/",
                                                                       upload_id, "/", part_number),
                                                         method = "GET"))


                              ## update that part
                              part[[part_number]]$uri <<- res$uri
                              part[[part_number]]$etag <<- res$etag
                              part[[part_number]]$response <<- response(res)
                              part[[part_number]]
                          },
                          upload_file = function(metadata = list()){
                              if(length(metadata)){
                                  metadata <<- list(metadata)
                                  names(metadata) <<- "metadata"
                              }
                              ## make this one easy to use
                              N <- part_length
                              res <- upload_init()
                              pb <- txtProgressBar(min = 0, max = N, style = 3)
                              con <- file(file, "rb")
                              for(i in 1:N){
                                  p <- upload_info_part(i)
                                  ## hack
                                  uri <- p$uri
                                  b <- readBin(con, "raw", part_size)
                                  res <- PUT(uri, body = b)
                                  rm(b)

                                  
                                  etag <- headers(res)$etag
                                  part[[i]]$etag <<- etag
                                  upload_complete_part(i, etag)
                                  part_finished <<- as.integer(i)
                                  setTxtProgressBar(pb, i)
                              }
                              close(pb)
                              res <- upload_complete_all()
                              close(con)
                              message("file uploading complete")

                              ## when we complete we could add meta
                              meta <- .self$metadata$asList()
                              if(length(meta)){
                                  message("Adding metadata ...")
                                  req <- api(token = auth$token,
                                                base_url = auth$url,
                                                path = paste0('project/',
                                                              project_id,
                                                              '/file/', res$id),
                                                body = meta,
                                                method = 'PUT')
                                  res <- status_check(req)
                                  message("Metadata complete")
                              }
                              res <- .asFile(res)
                              res
                          },
                          upload_complete_part = function(part_number = NULL,
                                                          etag = NULL){
                              res <- sevenbridges::upload_complete_part(auth$token,
                                                                upload_id,
                                                                part_number,
                                                                etag, base_url = auth$url)

                          },
                          upload_complete_all = function(){
                              ## fixme:
                              req <- api(token = auth$token,
                                            base_url = auth$url,
                                            path = paste0("upload/multipart/",
                                                          upload_id, "/complete"),
                                            method = "POST")
                              status_check(req)

                          },
                          upload_delete = function(){
                              sevenbridges::upload_delete(auth$token, upload_id,
                                                  base_url = auth$url)
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
