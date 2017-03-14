.response_files = c(
    "id", "name", "size", "project",
    "created_on", "modified_on", "storage",
    "origin", "tags", "metadata", "url")

#' Class Files
#'
#' Class Files
#'
#' \code{Files} (with "\code{s}") class is usally returned by the API call
#' which returns Files. A group of \code{Files} is defined as \code{FilesList}.
#' Users do not usually need to construct \code{Files} or \code{FilesList}
#' manually, they are generated from a API call most of the time.
#'
#' @field id character used as file id
#' @field name string used as file name
#' @field size file size
#' @field project project id if any, when returned by a API call,
#' it usually return the project id and stored with the object.
#' @field created_on date created on
#' @field modified_on date modified on
#' @field storage list as storage type
#' @field origin list as origin
#' @field tags list as tags
#' @field metadata a list for metadata associated with the file
#' @field url file download url
#'
#' @note In sevenbridges package version <= 1.5.4, the \code{Files} class
#' inherits from the \code{File} class defined in CWL. To avoid confusion,
#' in the current implementation, they are defined separately and not
#' coupled anymore.
#'
#' @return Files object
#' @export Files
#' @exportClass Files
#' @examples
#' Files(id = "test_id", name = "test.bam")
Files <- setRefClass(

    "Files",

    contains = c("Item"),

    fields = list(
        id          = "characterORNULL",
        name        = "characterORNULL",
        size        = "numericORNULL",
        project     = "characterORNULL",
        created_on  = "characterORNULL",
        modified_on = "characterORNULL",
        storage     = "listORNULL",
        origin      = "listORNULL",
        tags        = "listORNULL",
        metadata    = "listORNULL",
        url         = "characterORNULL"),

    methods = list(

        initialize = function(
            id          = NULL,
            name        = NULL,
            size        = NULL,
            project     = NULL,
            created_on  = NULL,
            modified_on = NULL,
            storage     = list(),
            origin      = list(),
            tags        = list(),
            metadata    = list(),
            url         = NULL, ...) {

            id          <<- id
            name        <<- name
            size        <<- size
            project     <<- project
            created_on  <<- created_on
            modified_on <<- modified_on
            storage     <<- storage
            origin      <<- origin
            tags        <<- tags
            metadata    <<- metadata
            url         <<- url

            callSuper(...)

        },

        delete = function() {
            auth$api(path = paste0("files/", id),
                     method = "DELETE")
        },

        download_url = function() {
            auth$api(path = paste0("files/", id,
                                   "/download_info"),
                     method = "GET")
        },

        download = function(destfile, ..., method = "curl") {
            'see `help(download.file)` for more options'

            if (is.null(url))
                url <<- download_url()$url

            # For backward compatibility:
            # R 3.1 does not have `dir.exists()`
            .dir.exists = function(d) {
                dirinfo = file.info(d)$isdir
                ifelse(is.na(dirinfo), FALSE, dirinfo)
            }

            if (.dir.exists(destfile)) {
                # is directory
                if (!is.null(name))
                    destfile = file.path(destfile, name)
            } else {
                stop("Destination directory does not exist")
            }

            download.file(url, destfile, ..., method = method)

        },

        copyTo = function(project = NULL, name = NULL) {
            auth$copyFile(id, project = project, name = name)
        },

        copy_to = function(project = NULL, name = NULL) {
            'copy a file to a project (id) with new name'

            copyTo(project = project, name = name)

        },

        meta = function() {
            'get metadata from a file'

            req = auth$api(path = paste0('files/', id, '/metadata'),
                           methods = "GET")
            # update
            metadata <<- req
            req

        },

        setMeta = function(..., overwrite = FALSE) {
            '
            set metadata with provided list, when overwrite
            is set to TRUE, it overwrites the metadata'

            o = .self$metadata

            md = .dotargsAsList(...)

            if (length(md)) {
                if (!overwrite) {
                    req = auth$api(path = paste0('files/', id, '/metadata'),
                                   body = md,
                                   method = 'PATCH')
                } else {
                    req = auth$api(path = paste0('files/', id, '/metadata'),
                                   body = md,
                                   method = 'PUT')
                }
            } else {
                if (overwrite) {
                    # overwrite!
                    message("reset meta")
                    req = auth$api(path = paste0('files/', id, '/metadata'),
                                   method = 'PUT')
                } else {
                    message("Nothing to add")
                    req = NULL
                }
            }

            # edit the object only when update is successful
            metadata <<- req
            req

        },

        set_meta = function(..., overwrite = FALSE) {
            '
            set metadata with provided list, when overwrite
            is set to TRUE, it overwrites the metadata'
            setMeta(..., overwrite = overwrite)
        },

        tag = function() {
            'get tag from a file'
            update()
            .self$tags
        },

        set_tag = function(x = NULL, overwrite = TRUE, ...) {
            'set a tag for a file, your tag need to be a list or vector'
            if (is.null(x)) stop("please provided your tags")
            if (is.character(x)) x = as.list(x)
            if (overwrite) {
                auth$api(path = paste0("files/", id, "/tags"),
                         method = "PUT",
                         body = x, ...)
                tags <<- x
            } else {
                .tags = tag()
                .tags = c(.tags, x)
                auth$api(path = paste0("files/", id, "/tags"),
                         method = "PUT",
                         body = .tags, ...)
                tags <<- .tags
            }

            tags

        },

        add_tag = function(x, ...) {
            'add new tags while keeping old tags'
            set_tag(x, overwrite = FALSE, ...)
        },

        update  = function(name = NULL, metadata = NULL,
                           tags = NULL) {
            '
            This call updates the name, the full set metadata,
            and tags for a specified file.'

            body = list(name = name, metadata = metadata, tags = tags)
            body = body[!sapply(body, is.null)]
            if (length(body)) {
                req = auth$api(path = paste0('files/', id),
                               body = body,
                               method = 'PATCH')
                res = .asFiles(req)
            } else {
                req = auth$api(path = paste0('files/', id),
                               method = 'GET')
                res = .asFiles(req)
            }

            # update fields
            for (fld in .response_files) .self$field(fld,res[[fld]])

            res

        },

        show = function() {
            .showFields(.self, "== Files ==", .response_files)
        }

    ))

.asFiles <- function(x) {
    Files(id          = x$id,
          name        = x$name,
          size        = as.numeric(x$size),
          project     = x$project,
          created_on  = x$created_on,
          modified_on = x$modified_on,
          storage     = x$storage,
          origin      = x$origin,
          tags        = x$tags,
          metadata    = x$metadata,
          url         = x$url,
          response    = response(x))
}

#' @rdname Files-class
#' @export FilesList
#' @aliases FilesList-class
#' @param ... one or more \code{Files} objects
#' @exportClass FilesList
FilesList <- setListClass("Files", contains = "Item0")

.asFilesList <- function(x) {
    obj          = FilesList(lapply(x$items, .asFiles))
    obj@href     = x$href
    obj@response = response(x)
    obj
}

#' Delete files
#'
#' Delete files
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
#' # or
#' delete(a$project("demo")$file("omni"))}
setGeneric("delete", function(obj) standardGeneric("delete"))

#' @rdname delete-methods
#' @aliases delete,SimpleList-method
setMethod("delete", "SimpleList", function(obj) {
    lapply(obj, function(x) x$delete())
})

#' @rdname delete-methods
#' @aliases delete,Files-method
setMethod("delete", "Files", function(obj) {
    obj$delete()
})

#' Download files
#'
#' Download files
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
#' # or
#' download(a$project("demo")$file("omni"))}
setGeneric("download", function(obj, ...) standardGeneric("download"))

#' @rdname download-methods
#' @aliases download,FilesList-method
setMethod("download", "FilesList", function(obj, ...) {
    for (i in 1:length(obj)) obj[[i]]$download(...)
})

#' @rdname download-methods
#' @aliases download,Files-method
setMethod("download", "Files", function(obj, ...) {
    obj$download(...)
})

#' Set file tags
#'
#' Set file tags
#'
#' @param obj single File or FileList
#' @param ... passed to obj$set_tag() or obj$add_tag()
#'
#' @export
#' @docType methods
#' @rdname tag-methods
#' @return tag list
#' @examples
#' \dontrun{
#' fl = a$project("demo")$file("omni")
#' set_tag(fl, "new tag")
#' set_tag(fl, list("new tag", "new tag 2"))}
setGeneric("set_tag", function(obj, ...) standardGeneric("set_tag"))

#' @rdname tag-methods
#' @aliases set_tag,FilesList-method
setMethod("set_tag", "FilesList", function(obj, ...) {
    for (i in 1:length(obj)) obj[[i]]$set_tag(...)
})

#' @rdname tag-methods
#' @aliases set_tag,Files-method
setMethod("set_tag", "Files", function(obj, ...) {
    obj$set_tag(...)
})

#' Add new file tags
#'
#' Add new file tags and keep the old tags
#'
#' @export
#' @docType methods
#' @rdname tag-methods
#' @return tag list
#' @examples
#' \dontrun{
#' fl = a$project("demo")$file("omni")
#' add_tag(fl, "new tag")
#' add_tag(fl, list("new tag", "new tag 2"))}
setGeneric("add_tag", function(obj, ...) standardGeneric("add_tag"))

#' @rdname tag-methods
#' @aliases add_tag,FilesList-method
setMethod("add_tag", "FilesList", function(obj, ...) {
    for (i in 1:length(obj)) obj[[i]]$add_tag(...)
})

#' @rdname tag-methods
#' @aliases add_tag,Files-method
setMethod("add_tag", "Files", function(obj, ...) {
    obj$add_tag(...)
})
