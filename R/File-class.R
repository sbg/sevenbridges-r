.response_files <- c("id", "name",  "size", "project", "created_on",
                     "modified_on", "origin", "metadata")
## Files: let's extend File class from CWL package
Files <- setRefClass("Files", contains = c("Item", "File"),
                     fields = list(id = "characterORNULL",
                         name = "characterORNULL",
                         metadata = "listORNULL",
                         project = "characterORNULL",
                         url = "characterORNULL",
                         created_on = "characterORNULL",
                         modified_on = "characterORNULL",
                         origin = "listORNULL"),
                     methods = list(
                         initialize = function(id = NULL,
                             name = NULL,
                             metadata = NULL,
                             project = NULL,
                             url = NULL,
                             created_on = NULL,
                             modified_on = NULL,
                             origin = list(), ...){


                             id <<- id
                             name <<- name
                             metadata <<- metadata
                             project <<- project
                             url <<- url
                             created_on <<- created_on
                             modified_on <<- modified_on
                             origin <<- origin

                             callSuper(...)
                         },
                         delete = function(){
                                 auth$api(path = paste0("files/", id),
                                          method = "DELETE")
                        },
                         download_url = function(){
                                 auth$api(path = paste0("files/", id, "/download_info"),
                                          method = "GET")
                        },
                         download = function(destfile, ..., method = "curl"){
                             'see help(download.file) for more options'
                             if(is.null(url))
                                 url <<- download_url()$url
                             ## for compatible reason, R 3.1 doesn't have dir.exists
                             ##
                             .dir.exists <- function(d) {
                                 de <- file.info(d)$isdir
                                 ifelse(is.na(de), FALSE, de)
                             }
                             if(.dir.exists(destfile)){
                                 ## is directory
                                 if(!is.null(name))
                                     destfile <- file.path(destfile, name)
                             }
                             download.file(url, destfile, ..., method = method)
                         },
                         copyTo = function(project = NULL, name = NULL){
                             auth$copyFile(id, project = project, name = name)
                         },
                         meta = function(){
                             'get metadata from a file'
                             req <- auth$api(path = paste0('files/', id, '/metadata'),
                                             methods = "GET")
                             ## update
                             metadata <<- req
                             req
                         },
                         setMeta = function(..., overwrite = FALSE){

                             o <- .self$metadata

                             md <- .dotargsAsList(...)

                             if(length(md)){

                                 if(!overwrite){
                                     req <- auth$api(path = paste0('files/', id, '/metadata'),
                                                     body = md,
                                                     method = 'PATCH')
                                 }else{
                                     req <- auth$api(path = paste0('files/', id, '/metadata'),
                                                     body = md,
                                                     method = 'PUT')
                                 }
                              

                                 
                             }else{
                                 if(overwrite){
                                     ## overwrite!
                                     message("reset meta")
                                     req <- auth$api(path = paste0('files/', id, '/metadata'),
                                                     method = 'PUT')
                                 }else{
                                     message("Nothing to add")
                                     req <- NULL
                                 }
                             }
                             
                             ## only when successful update, we edit the object
                             metadata <<- req
                             req
                         },
                         update  = function(name = NULL, metadata = NULL){
                             body <- list(name = name, metadata = metadata)
                             body <- body[!sapply(body, is.null)]
                             if(length(body)){
                                 req <- auth$api(path = paste0('files/', id),
                                                 body = body,
                                                 method = 'PATCH')
                                 res <- .asFiles(req)
                             }else{
                                 req <- auth$api(path = paste0('files/', id),
                                                 method = 'GET')
                                 res <- .asFiles(req)
                             }
                             ## update fields
                             for(fld in .response_files){
                                 .self$field(fld,res[[fld]])
                             }
                             res
                         },
                         show = function(){
                            .showFields(.self, "== File ==", .response_files)
                        }

                    ))

.asFiles <- function(x){
    Files(id = x$id,
          name = x$name,
          size = as.integer(x$size),
          metadata = x$metadata,
          project = x$project,
          created_on = x$created_on,
          modified_on = x$modified_on,
          origin = x$origin, 
          response = response(x))
}

FilesList <- setListClass("Files", contains = "Item0")

.asFilesList <- function(x){
    obj <- FilesList(lapply(x$items, .asFiles))
    obj@href <- x$href
    obj@response <- response(x)
    obj
}


#' Delete file or files
#'
#' Delete file
#'
#' @param obj single File or FileList
#'
#' @export
#' @docType methods
#' @rdname delete-methods
#' @return system message
#' @examples
#' \dontrun{
#' a$project("demo")$file("omni")$delete()
#' ## or
#' delete(a$project("demo")$file("omni"))
#' }
setGeneric("delete", function(obj) standardGeneric("delete"))

#' @rdname delete-methods
#' @aliases delete,SimpleList-method
setMethod("delete", "SimpleList", function(obj){
    lapply(obj, function(x) x$delete())
})

#' @rdname delete-methods
#' @aliases delete,Files-method
setMethod("delete", "Files", function(obj){
    obj$delete()
})




#' Download file or files
#'
#' Download file
#'
#' @param obj single File or FileList
#' @param ... passed to download() 
#'
#' @export
#' @docType methods
#' @rdname download-methods
#' @return system message
#' @examples
#' \dontrun{
#' a$project("demo")$file("omni")$download()
#' ## or
#' download(a$project("demo")$file("omni"))
#' }
setGeneric("download", function(obj, ...) standardGeneric("download"))

#' @rdname download-methods
#' @aliases download,FilesList-method
setMethod("download", "FilesList", function(obj, ...){
    lapply(obj, function(x) x$download(...))
})

#' @rdname download-methods
#' @aliases download,Files-method
setMethod("download", "Files", function(obj, ...){
    obj$download(...)
})

