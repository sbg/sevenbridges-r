Volume <- setRefClass(
  "Volume",
  contains = "Item",

  fields = list(
    id = "characterORNULL",
    name = "characterORNULL",
    description = "characterORNULL",
    created_on = "characterORNULL",
    modified_on = "characterORNULL",
    active = "logicalORNULL",
    service = "listORNULL",
    import_job = "listORNULL", # add on
    export_job = "listORNULL"
  ), # add on

  methods = list(
    initialize = function(id = NULL, name = NULL, description = NULL,
                              created_on = NULL, modified_on = NULL,
                              active = NULL, service = NULL, ...) {
      id <<- id
      name <<- name
      description <<- description
      created_on <<- created_on
      modified_on <<- modified_on
      active <<- active
      service <<- service

      callSuper(...)
    },

    update = function(description = NULL, service = NULL, ...) {
      body <- list(description = description, service = service)
      res <- auth$api(
        path = paste0("storage/volumes/", id),
        body = body,
        method = "PATCH", ...
      )
      description <<- description
      service <<- service

      # update import jobs

      # update export jobs

      .asVolume(res)
    },

    detail = function(...) {
      res <- auth$api(path = paste0("storage/volumes/", id), ...)
      .asVolume(res)
    },

    delete = function() {
      auth$api(
        path = paste0("storage/volumes/", id),
        method = "DELETE"
      )
    },

    import = function(location = NULL, project = NULL, name = NULL, overwrite = FALSE, ...) {
      body <- list(
        "source" = list(
          "volume" = id,
          "location" = location
        ),

        "destination" = list(
          "project" = project,
          "name" = ifelse(is.null(name), location, name)
        ),

        "overwrite" = overwrite
      )

      res <- auth$api(
        path = "storage/imports",
        body = body, method = "POST", ...
      )
      import_job <<- c(import_job, list(res))

      res
    },

    export = function(file = NULL, volume = NULL, location = NULL, sse_algorithm = "AES256", ...) {
      body <- list(
        "source" = list(
          "file" = file
        ),
        "destination" = list(
          "volume" = volume,
          "location" = location
        ),
        "properties" = list(
          "sse_algorithm" = sse_algorithm
        )
      )

      res <- auth$api(
        path = "storage/exports",
        body = body, method = "POST"
      )
      export_job <<- c(export_job, list(res))

      res
    },

    get_import_job = function(job_id = NULL) {
      if (is.null(job_id)) {
        message("no job_id provided, show existing ones")
        return(import_job)
      }

      res <- auth$api(
        path = paste0("storage/imports/", job_id),
        body = body, method = "GET"
      )

      # insert
      if (length(import_job)) {
        idx <- which(job_id == sapply(
          import_job,
          function(x) x$id
        ))
        if (length(idx)) {
          import_job[[idx]] <<- res
        } else {
          import_job <<- c(import_job, list(res))
        }
      } else {
        import_job <<- list(res)
      }
      res
    },

    get_export_job = function(job_id = NULL) {
      if (is.null(job_id)) {
        message("no job_id provided, show existing ones")
        return(export_job)
      }
      res <- auth$api(
        path = paste0("storage/exports/", job_id),
        body = body, method = "GET"
      )
      # insert
      if (length(export_job)) {
        idx <- which(job_id == sapply(
          export_job,
          function(x) x$id
        ))
        if (length(idx)) {
          export_job[[idx]] <<- res
        } else {
          export_job <<- c(export_job, list(res))
        }
      } else {
        export_job <<- list(res)
      }

      res
    },

    show = function() {
      .showFields(
        .self, "== Volume ==",
        c(
          "id", "name", "description",
          "created_on", "modified_on",
          "active", "service",
          "import_job", "export_job"
        )
      )
    }
  )
)

VolumeList <- setListClass("Volume", contains = "Item0")

.asVolumeList <- function(x) {
  obj <- VolumeList(lapply(x$items, .asVolume))
  obj@href <- x$href
  obj@response <- response(x)
  obj
}

.asVolume <- function(x) {
  Volume(
    id = x$id,
    name = x$name,
    description = x$description,
    created_on = x$created_on,
    modified_on = x$modified_on,
    active = x$active,
    service = x$service
  )
}
