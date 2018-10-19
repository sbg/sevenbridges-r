.response_folders <- c(
  "id", "name", "project", "parent",
  "type", "created_on", "modified_on"
)

#' Class Folders
#'
#' Class Folders
#'
#' \code{Folders} class is usally returned by the API call which returns
#' Folders. It allows you to create and organize folders on platform.
#' Folders are created inside projects, and can be used to contain
#' files within the project. Users do not usually need to construct
#' \code{Folders} manually, they are generated from a API call most
#' of the time.
#'
#' @field id character string used as folder ID
#' @field name character string used as folder name
#' @field project project name the folder belongs to
#' @field parent parent folder ID, usually returned by
#' the project details (\code{root_folder}) or listed
#' folder contents
#' @field type file type: \code{FILE} or \code{FOLDER}
#' @field created_on date the folder was created
#' @field modified_on date the folder was modified
#'
#' @return Folders object
#' @export Folders
#' @exportClass Folders
#' @examples
#' Folders(id = "test_id", name = "test.bam")
Folders <- setRefClass(

  "Folders",
  contains = c("Item"),

  fields = list(
    id = "characterORNULL",
    name = "characterORNULL",
    project = "characterORNULL",
    parent = "characterORNULL",
    type = "characterORNULL",
    created_on = "characterORNULL",
    modified_on = "characterORNULL"
  ),

  methods = list(
    initialize = function(id = NULL,
                              name = NULL,
                              project = NULL,
                              parent = NULL,
                              type = NULL,
                              created_on = NULL,
                              modified_on = NULL, ...) {
      id <<- id
      name <<- name
      project <<- project
      parent <<- parent
      type <<- type
      created_on <<- created_on
      modified_on <<- modified_on

      callSuper(...)
    },

    create_folder = function(name = NULL, ...) {
      "create a new folder"

      if (is.null(name)) {
        stop("folder `name` must be specified")
      }

      if (identical(substr(name, 1L, 2L), "__")) {
        stop("folder name should not start with `__`")
      }

      body <- list(
        "name" = name,
        "parent" = id,
        "type" = "FOLDER"
      )

      auth$api(
        path = "files", method = "POST",
        body = body, ...
      )
    },

    list_contents = function(...) {
      "list folder contents"

      auth$api(
        path = paste0("files/", id, "/list"),
        method = "GET", ...
      )
    },

    copy_file_to_folder = function(...) {
      "copy a file between folders"
      NULL
    },

    move_file_to_folder = function(...) {
      "move a file between folders"
      NULL
    },

    delete = function(...) {
      "delete the folder"

      auth$api(
        path = paste0("files/", id),
        method = "DELETE", ...
      )
    },

    show = function() {
      .showFields(.self, "== Folders ==", .response_folders)
    }
  )
)

.asFolders <- function(x) {
  Folders(
    id = x$id,
    name = x$name,
    project = x$project,
    parent = x$parent,
    type = x$type,
    created_on = x$created_on,
    modified_on = x$modified_on,
    response = response(x)
  )
}

FoldersList <- setListClass("Folders", contains = "Item0")

.asFoldersList <- function(x) {
  obj <- FoldersList(lapply(x$items, .asFolders))
  obj@href <- x$href
  obj@response <- response(x)
  obj
}
