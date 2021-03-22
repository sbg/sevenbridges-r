# Project class ----------------------------------------------------------------
Project <- setRefClass(
  "Project",
  contains = "Item",

  fields = list(
    id = "characterORNULL",
    name = "characterORNULL",
    billing_group_id = "characterORNULL",
    description = "characterORNULL",
    type = "characterORNULL",
    owner = "characterORNULL",
    tags = "listORNULL",
    settings = "listORNULL",
    root_folder = "characterORNULL",
    created_by = "characterORNULL",
    created_on = "characterORNULL",
    modified_on = "characterORNULL"
  ),

  methods = list(

    # initialize ---------------------------------------------------------------
    initialize = function(id = NULL, name = NULL, billing_group_id = NULL,
                          description = "", type = "", owner = NULL,
                          tags = list(), settings = list(),
                          root_folder = "", created_by = "",
                          created_on = "", modified_on = "", ...) {
      if (is.null(id)) stop("id is required")
      id <<- id
      name <<- name
      billing_group_id <<- billing_group_id
      description <<- description
      type <<- type
      owner <<- owner
      tags <<- tags
      settings <<- settings
      root_folder <<- root_folder
      created_by <<- created_by
      created_on <<- created_on
      modified_on <<- modified_on

      callSuper(...)
    },

    # update -------------------------------------------------------------------
    update = function(name = NULL, description = NULL, billing_group_id = NULL, ...) {
      "update name/description/billing group for a project"

      body <- list(
        "name" = name,
        "description" = description,
        "billing_group" = billing_group_id
      )

      body <- body[!sapply(body, is.null)]
      if (length(body) == 0) {
        stop("please provide updated information")
      }

      nms <- names(body)

      # update project itself
      for (nm in nms) .self$field(nm, body[[nm]])

      req <- api(
        token = auth$token,
        base_url = auth$url,
        path = paste0("projects/", id),
        body = body, method = "PATCH",
        authorization = auth$authorization, ...
      )

      res <- status_check(req)
      res <- .asProject(res)
      res$auth <- .self$auth

      res
    },

    # member -------------------------------------------------------------------
    member = function(username = NULL, name = username,
                      ignore.case = TRUE, exact = FALSE, ...) {
      if (is.null(id)) stop("id must be provided")

      req <- api(
        token = auth$token,
        base_url = auth$url,
        path = paste0("projects/", id, "/members"),
        method = "GET",
        authorization = auth$authorization, ...
      )
      res <- status_check(req)
      ms <- .asMemberList(res, pid = id)
      ms <- setAuth(ms, .self$auth, "Member")

      if (is.null(name)) {
        return(ms)
      } else {
        m <- m.match(ms,
                     name = name,
                     .name = "username",
                     exact = exact
        )
        return(m)
      }
    },

    member_add = function(username = NULL, name = username,
                          copy = FALSE, write = FALSE, execute = FALSE,
                          admin = FALSE, read = FALSE, ...) {
      body <- list(
        "username" = name,
        "permissions" = list(
          "copy" = copy,
          "write" = write,
          "read" = read,
          "execute" = execute,
          "admin" = admin
        )
      )

      req <- api(
        token = auth$token,
        base_url = auth$url,
        path = paste0("projects/", id, "/members"),
        body = body, method = "POST",
        authorization = auth$authorization, ...
      )

      res <- status_check(req)
      .asMember(res)
    },

    # file ---------------------------------------------------------------------
    file = function(name = NULL,
                    id = NULL,
                    exact = FALSE,
                    detail = FALSE, ...) {
      res <- auth$file(
        name = name,
        id = id,
        project = .self$id,
        exact = exact,
        detail = detail, ...
      )
      res
    },

    upload = function(filename = NULL, name = NULL, metadata = list(),
                      overwrite = FALSE, manifest_file = NULL,
                      manifest_metadata = TRUE, subset, select,
                      verbal = NULL, ...) {

      # upload via a manifest
      if (!is.null(manifest_file)) {
        if (!file.exists(manifest_file)) {
          stop("manifest file not found")
        }

        # importing
        manf <- read.csv(manifest_file, stringsAsFactors = FALSE)

        # subseting
        # revision on subset.data.frame to hack on missing -> NULL
        # browser()

        r <- if (missing(subset)) {
          rep_len(TRUE, nrow(manf))
        } else {
          e <- substitute(subset)
          r <- eval(e, manf, parent.frame())
          if (!is.logical(r)) {
            stop("'subset' must be logical")
          }
          r & !is.na(r)
        }

        vars <- if (missing(select)) {
          TRUE
        } else {
          nl <- as.list(seq_along(manf))
          names(nl) <- names(manf)
          eval(substitute(select), nl, parent.frame())
        }

        manf.sub <- manf[r, vars, drop = FALSE]
        if (!missing(subset) || !missing(select)) {
          message(nrow(manf.sub), " out of ", nrow(manf), " item subsetted.")
        }

        # if (!missing(subset) || !missing(select)) {
        #
        #     manf.sub = subset(manf, subset = parse(subset),
        #                       select = parse(select))
        #     message(nrow(manf.sub)," out of ", nrow(manf), " item subsetted.")
        # } else {
        #     manf.sub = manf
        # }

        # formalize data frame to right type
        manf.sub <- formalizeMetaDataFrame(manf.sub)


        # validation: first column of data.frame has to be file path and it exists
        fc <- sapply(manf.sub[, 1], function(x) {
          is.character(x)
        })

        if (!all(fc)) {
          message("Following rows are invalid: ", paste(which(!fc), collapse = " "))
          stop("The first column of manifest file has to be character to represent file path")
        }

        fe <- sapply(manf.sub[, 1], function(x) {
          file.exists(x)
        })

        if (!all(fe)) {
          message("Following rows are invalid (not exists): ", paste(which(!fe), collapse = " "))
          stop("The first colunn of manifest file has to be valid file path")
        }

        if (is.null(verbal)) verbal <- FALSE

        # if verbal = TRUE, print file uploading progress info for each file
        # if verbal = FALSE, print all files uploading progress in single bar
        if (!verbal) {
          message("files uploading progress:")
          pb <- txtProgressBar(min = 0, max = nrow(manf.sub), style = 3)
        }

        for (i in 1:nrow(manf.sub)) {
          x <- manf.sub[i, ]


          if (manifest_metadata) {
            .m <- as.list(x)[-1]
          } else {
            .m <- list()
          }
          if (verbal) {
            upload(x[, 1], metadata = .m, overwrite = overwrite, verbal = verbal, ...)
          } else {
            suppressMessages(upload(x[, 1],
                                    metadata = .m, overwrite = overwrite,
                                    verbal = verbal, ...
            ))
            setTxtProgressBar(pb, i)
          }
        }
        if (!verbal) {
          close(pb)
        }

        return(invisible())
      }

      # if filename is a list
      if (length(filename) > 1) {
        if (is.null(verbal)) verbal <- FALSE
        # if verbal = TRUE, print file uploading progress info for each file
        # if verbal = FALSE, print all files uploading progress in single bar
        if (!verbal) {
          message("files uploading progress:")
          pb <- txtProgressBar(min = 0, max = length(filename), style = 3)
        }
        for (i in 1:length(filename)) {
          fl <- filename[i]
          if (verbal) {
            message(fl)
            if (file.info(fl)$size > 0) {
              upload(fl,
                     metadata = metadata,
                     overwrite = overwrite, verbal = verbal, ...
              )
            } else {
              warning("skip uploading: empty file")
            }
          } else {
            if (file.info(fl)$size > 0) {
              upload(fl,
                     metadata = metadata,
                     overwrite = overwrite, verbal = verbal, ...
              )
              setTxtProgressBar(pb, i)
            }
          }
        }
        if (!verbal) {
          close(pb)
        }
        return(invisible())
      }

      # if filename is a folder
      if (!is.na(file.info(filename)$isdir) && file.info(filename)$isdir) {
        message("Upload all files in the folder: ", filename)
        fls <- list.files(filename, recursive = TRUE, full.names = TRUE)
        upload(fls,
               metadata = metadata,
               overwrite = overwrite, verbal = verbal, ...
        )
        return(invisible())
      }

      # check
      if (!file.exists(filename)) stop("file not found")

      u <- Upload(
        auth = auth,
        file = filename,
        name = name,
        project_id = id,
        metadata = metadata, ...
      )

      if (is.null(verbal)) {
        verbal <- TRUE
      }
      u$upload_file(
        metadata = metadata,
        overwrite = overwrite,
        verbal = verbal
      )
    },

    # apps ---------------------------------------------------------------------
    app = function(...) {
      auth$app(project = id, ...)
    },

    app_add = function(short_name = NULL, filename = NULL, revision = NULL, keep_test = FALSE, ...) {
      if (is.null(filename)) {
        stop("file (cwl json) need to be provided")
      }

      if (is.null(short_name)) {
        stop("app short name has to be provided (alphanumeric character with no spaces)")
      } else {
        if (grepl("[[:space:]]+", short_name)) {
          stop("id cannot have white space")
        }
      }

      if (is(filename, "Tool") || is(filename, "Workflow")) {
        if (is(filename, "Workflow")) {
          # push apps and update run
          steplst <- filename$steps
          isSBGApp <- function(x) length(x$"sbg:id")
          lst <- lapply(steplst, function(x) {
            if (!isSBGApp(x$run)) {
              # if not exists on sbg platform,
              # need to add it first
              .name <- gsub("#", "", x$run$id)
              message(.name)
              new.app <- app_add(short_name = .name, filename = x$run)
              new.app
            } else {
              # SBG id does not need to add
              # but need to copy?
              x
            }
          })
          # # No need to do this here, should not edit
          # # should assume link exists.
          # slst = lst[[1]]
          # for (i in 1:(length(lst) -1)) {
          #     slst = slst + lst[[i + 1]]
          # }
          # # udpate steplist
          # filename$steps = slst
        }

        ## works for Tool now
        if (is(filename, "Tool") && keep_test) {
          ## keep old revision job test info
          .app.id <- paste0(id, "/", short_name)
          .sbg.job <- auth$app(id = .app.id)$cwl()$"sbg:job"
          if (!is.null(filename$"sbg:job")) {
            stop("Using the new passed test info")
          } else {
            message("keeping the previous revision test info ('sbg:job')")
            filename$"sbg:job" <- .sbg.job
          }
        }

        fl <- tempfile(fileext = ".json")
        con <- base::file(fl, raw = TRUE)
        writeLines(filename$toJSON(), con = con)
        filename <- fl
        close(con)
      }

      if (is.null(revision)) {
        # latest check revision first
        .id <- paste0(id, "/", short_name)
        msg <- try(.r <- as.integer(app(id = .id, detail = TRUE)$revision), silent = TRUE)
        if (!inherits(msg, "try-error") && is.integer(.r)) {
          .r <- .r + 1
          message("create new revision ", .r)
          res <- auth$api(
            path = paste0("apps/", id, "/", short_name, "/", .r, "/raw"),
            method = "POST",
            body = upload_file(filename), ...
          )
        } else {
          res <- auth$api(
            path = paste0("apps/", id, "/", short_name, "/raw"),
            method = "POST",
            body = upload_file(filename), ...
          )
        }
      } else {
        # latest check revision first
        .id <- paste0(id, "/", short_name)
        .r <- as.integer(app(id = .id, detail = TRUE)$revision)
        if (revision != .r + 1) {
          stop("latest revision is: ", .r, ", you have to bump to: ", .r + 1)
        }
        res <- auth$api(
          path = paste0("apps/", id, "/", short_name, "/", revision, "/raw"),
          method = "POST",
          body = upload_file(filename), ...
        )
      }

      # file.remove(filename)
      .id <- res[["sbg:id"]]
      res <- app(id = .id)
      # check error message
      validateApp(response(res))
      res
    },

    # tasks --------------------------------------------------------------------
    task = function(...) {
      auth$task(project = id, ...)
    },

    task_add = function(name = NULL, description = NULL, batch = NULL,
                        app = NULL, inputs = NULL,
                        input_check = getOption("sevenbridges")$input_check,
                        use_interruptible_instances = NULL,
                        execution_settings = NULL, ...) {

      # spot instance logic:
      # if it's NULL, then follow the project settings (if project set it to TRUE, then use TRUE, and vice versa)
      # if it's not NULL, then use the specified value
      if (is.null(use_interruptible_instances)) use_interruptible_instances <- .self$settings$use_interruptible_instances
      if (!is.null(use_interruptible_instances)) use_interruptible_instances <- as.logical(use_interruptible_instances)

      if (input_check) {
        message("Checking inputs...")
        apps <- auth$app(id = app)
        inputs <- apps$input_check(inputs, batch, .self)
      }

      message("Drafting task...")

      if (is.null(inputs)) {
        .i <- inputs
      } else {
        .i <- lapply(inputs, asTaskInput)
      }

      body <- list(
        name = name,
        description = description,
        project = id,
        app = app,
        use_interruptible_instances = use_interruptible_instances,
        execution_settings = execution_settings,
        inputs = .i
      )

      if (!is.null(batch)) body <- c(batch, body)

      res <- auth$api(
        path = "tasks", body = body,
        method = "POST", ...
      )

      message("Done")

      res <- .asTask(res)
      if (length(res$errors)) {
        message("Errors found: please fix it in your script or in the GUI")
        .showList(res$errors)
      }

      setAuth(res, .self$auth, "Task")
    },

    task_run = function(...) {
      task <- Task(auth = .self$auth, project_id = id, ...)
      task$run()
    },

    delete = function(...) {
      req <- auth$api(path = paste0("projects/", id), method = "DELETE", ...)
      req
    },

    # folders ------------------------------------------------------------------
    get_root_folder_id = function() {
      "Get the project root folder ID"
      root_folder
    },

    get_root_folder = function() {
      "Get the project root folder object."
      .self$file(id = root_folder)
    },

    # show ---------------------------------------------------------------------
    show = function() {
      .showFields(
        .self, "== Project ==",
        c(
          "id", "name", "description",
          "billing_group_id", "type",
          "owner", "tags"
        )
      )
    }
  )
)

# .asProject -------------------------------------------------------------------
.asProject <- function(x) {
  Project(
    id = x$id,
    href = x$href,
    name = x$name,
    type = x$type,
    owner = x$owner,
    tags = x$tags,
    description = x$description,
    billing_group_id = x$billing_group,
    settings = x$settings,
    root_folder = x$root_folder,
    created_by = x$created_by,
    created_on = x$created_on,
    modified_on = x$modified_on,
    response = response(x)
  )
}

# ProjectList class ------------------------------------------------------------
ProjectList <- setListClass("Project", contains = "Item0")

# .asProjectList ---------------------------------------------------------------
.asProjectList <- function(x) {
  obj <- ProjectList(lapply(x$items, .asProject))
  obj@href <- x$href
  obj@response <- response(x)
  obj
}
