#' Class Auth
#'
#' Auth object
#'
#' Every object could be requested from this Auth object and any action
#' could start from this object using cascading style. Please check
#' \code{vignette("api")} for more information.
#'
#' @field from [character] Authentication method. Could be \code{"direct"}
#' (pass the credential information to the arguments directly),
#' \code{"env"} (read from pre-set system environment variables),
#' or \code{"file"} (read configurations from a credentials file).
#' Default is \code{"direct"}.
#' @field platform [character] The platform to use.
#' If \code{platform} and \code{url} are both not specified,
#' the default is \code{"cgc"} (Cancer Genomics Cloud).
#' Other possible values include
#' \code{"aws-us"} (Seven Bridges Platform - US),
#' \code{"aws-eu"} (Seven Bridges Platform - EU),
#' \code{"ali-cn"} (Seven Bridges Platform - China),
#' \code{"cavatica"} (Cavatica), and
#' \code{"f4c"} (BioData Catalyst Powered by Seven Bridges).
#' @field url [character] Base URL for API. Please only use this when you
#' want to specify a platform that is not in the \code{platform} list
#' above, and also leaving \code{platform} unspecified.
#' @field token [character] Your authentication token.
#' @field sysenv_url Name of the system environment variable storing
#' the API base URL. By default: \code{"SB_API_ENDPOINT"}.
#' @field sysenv_token Name of the system environment variable storing
#' the auth token. By default: \code{"SB_AUTH_TOKEN"}.
#' @field config_file [character] Location of the user configuration file.
#' By default: \code{"~/.sevenbridges/credentials"}.
#' @field profile_name [character] Profile name in the user configuration file.
#' The default value is \code{"default"}.
#' @field fs FS object, for mount and unmount file system.
#' @field authorization Logical. Is the \code{token} an API
#' auth token (\code{FALSE}) or an access token from the
#' Seven Bridges single sign-on (\code{TRUE})?
#'
#' @importFrom stringr str_match
#'
#' @export Auth
#' @exportClass Auth
#' @examples
#' # Direct authentication (default)
#' # replace with your auth token
#' token <- "your_token"
#' a <- Auth(platform = "cgc", token = token)
#' \dontrun{
#' # Authentication with environment variables
#' # This will read system environments variables
#' # `SB_API_ENDPOINT` and `SB_AUTH_TOKEN` by default
#' a <- Auth(from = "env")
#'
#' # Authentication with user configuration file
#' # This will load profile `default` from config
#' # file `~/.sevenbridges/credentials` by default
#' a <- Auth(from = "file")
#' }
Auth <- setRefClass(
  "Auth",
  fields = list(
    from = "character",
    platform = "characterORNULL",
    url = "character",
    token = "character",
    sysenv_url = "characterORNULL",
    sysenv_token = "characterORNULL",
    config_file = "characterORNULL",
    profile_name = "characterORNULL",
    fs = "FSORNULL",
    authorization = "logical"
  ),
  methods = list(

    # initialize ---------------------------------------------------------------
    initialize =
      function(from = c("direct", "env", "file"), platform = NULL, url = NULL,
               token = NULL, sysenv_url = NULL, sysenv_token = NULL,
               config_file = NULL, profile_name = NULL, fs = NULL,
               authorization = FALSE, ...) {

        # Authentication Logic
        #
        # 0x01. If `from == "direct"` (default)
        #       then use on-the-fly configuration.
        #
        # Four cases:
        #
        # 1. `platform` and `url` are both provided:
        #    throw error: platform and URL cannot coexist
        # 2. `platform` and `url` are both not provided:
        #    use `.sbg_default_platform` and throw a warning
        # 3. `platform` != NULL, `url`  = NULL:
        #    use platform + token, throw message
        # 4. `platform`  = NULL, `url` != NULL:
        #    use URL + token, throw message
        #
        # 0x02. If `from == "env"`
        #       then read from environment variables.
        #
        # One step:
        #
        # 1. Read environment variables `sysenv_url`
        #    and `sysenv_token`
        #    throw message indicating environment
        #    variable names
        #    use url + token
        #
        # 0x03. If `from == "file"`
        #       then use configuration file.
        #
        # Two steps:
        #
        # 1. Load ini format file at location `config_file`
        #    throw message indicating file location
        # 2. In loaded config list, look for `profile_name`
        #    throw message indicating profile name
        #    get url + token from this profile

        # For backward compatibility only:
        # remove this block when enough time has passed
        auth_call <- as.list(match.call())
        if (!is.null(auth_call[["username"]])) {
          stop("This authentication parameter is deprecated, please refer to: https://sbg.github.io/sevenbridges-r/articles/api.html#create-auth-object for the new authentication methods")
        }

        fs <<- fs
        authorization <<- authorization

        .from <- match.arg(from)
        from <<- .from

        if (.from == "direct") {

          # In this case, `sysenv_url`, `sysenv_token`,
          # `config_file`, and `profile_name`
          # should all be `NULL` even if they
          # are assigned values
          .sysenv_url <- NULL
          .sysenv_token <- NULL
          .config_file <- NULL
          .profile_name <- NULL
          sysenv_url <<- .sysenv_url
          sysenv_token <<- .sysenv_token
          config_file <<- .config_file
          profile_name <<- .profile_name

          # Four cases depending on `platform` and `url`

          # Case 1: platform and url are both provided
          if (!is.null(platform) & !is.null(url)) {
            stop("`platform` and `url` cannot be set simultaneously", call. = FALSE)
          }

          # Case 2: platform and url are both *not* provided
          if (is.null(platform) & is.null(url)) {
            warning("`platform` and `url` are not set, will use the default platform: ",
              .sbg_default_platform,
              call. = FALSE
            )
            .platform <- .sbg_default_platform
            .url <- .sbg_baseurl[[.sbg_default_platform]]

            platform <<- .platform
            url <<- .url
          }

          # Case 3: platform is provided, url is not provided
          if (!is.null(platform) & is.null(url)) {

            # platform name sanity check
            .platform <- platform
            if (.platform %in% names(.sbg_baseurl)) {
              .url <- .sbg_baseurl[[.platform]]
            } else {
              stop("Platform does not exist, please check its spelling (case-sensitive)", call. = FALSE)
            }
            message("Using platform: ", .platform)

            platform <<- .platform
            url <<- .url
          }

          # Case 4: platform is not provided, url is provided
          if (is.null(platform) & !is.null(url)) {
            .url <- normalize_url(url)
            # lookup an accurate platform name
            .platform <- sbg_platform_lookup(.url)

            platform <<- .platform
            url <<- .url
          }

          if (is.null(token)) {
            stop('`token` must be set when `from = "direct"`', call. = FALSE)
          }
          token <<- token
        }

        if (.from == "env") {

          # In this case, `config_file` and `profile_name`
          # should be `NULL` even if they
          # are assigned values
          .config_file <- NULL
          .profile_name <- NULL
          config_file <<- .config_file
          profile_name <<- .profile_name

          # get system environment variables
          if (is.null(sysenv_url)) {
            .sysenv_url <- .sbg_default_sysenv_url
          } else {
            .sysenv_url <- sysenv_url
          }
          if (is.null(sysenv_token)) {
            .sysenv_token <- .sbg_default_sysenv_token
          } else {
            .sysenv_token <- sysenv_token
          }
          message(
            "Authenticating with system environment variables: ",
            .sysenv_url, " and ", .sysenv_token
          )

          sysenv_url <<- .sysenv_url
          sysenv_token <<- .sysenv_token

          # extract url + token from environment variables
          .url <- normalize_url(sbg_get_env(.sysenv_url))
          .token <- sbg_get_env(.sysenv_token)

          url <<- .url
          token <<- .token

          # lookup an accurate platform name instead of simply `NULL`
          .platform <- sbg_platform_lookup(.url)
          platform <<- .platform
        }

        if (.from == "file") {

          # In this case, `sysenv_url`, `sysenv_token`,
          # should be `NULL` even if they
          # are assigned values
          .sysenv_url <- NULL
          .sysenv_token <- NULL
          sysenv_url <<- .sysenv_url
          sysenv_token <<- .sysenv_token

          # parse user config file
          if (is.null(config_file)) {
            .config_file <- .sbg_default_config_file
          } else {
            .config_file <- config_file
          }
          config_list <- sbg_parse_config(.config_file)
          message("Authenticating with user configuration file: ", .config_file)

          config_file <<- .config_file

          # locate user profile with url + token
          if (is.null(profile_name)) {
            .profile_name <- .sbg_default_profile_name
          } else {
            .profile_name <- profile_name
          }
          # extract url + token from profile
          .url <- normalize_url(config_list[[.profile_name]][["api_endpoint"]])
          .token <- config_list[[.profile_name]][["auth_token"]]
          if (is.null(.url) | is.null(.token)) {
            stop("`The field api_endpoint` or `auth_token` is missing in profile:",
              .profile_name,
              call. = FALSE
            )
          }
          message("Authenticating with user profile: ", .profile_name)

          profile_name <<- .profile_name
          url <<- .url
          token <<- .token

          # lookup an accurate platform name instead of simply `NULL`
          .platform <- sbg_platform_lookup(.url)
          platform <<- .platform
        }
      },

    # api paths ----------------------------------------------------------------
    api = function(..., limit = getOption("sevenbridges")$"limit",
                   offset = getOption("sevenbridges")$"offset",
                   fields = NULL, complete = FALSE) {
      "This call returns all API paths, and pass arguments to api() function with input token and url automatically"

      req <- sevenbridges::api(
        token,
        base_url = url, limit = limit,
        offset = offset, fields = fields, authorization = authorization, ...
      )
      req <- status_check(req)

      if (complete) {
        N <- as.numeric(headers(response(req))[["x-total-matching-query"]])
        if (length(N)) .item <- length(req$items)
        if (.item < N) {
          pb <- txtProgressBar(min = 1, max = N %/% 100 + 1, style = 3)
          res <- NULL

          for (i in 1:(N %/% 100 + 1)) {
            .limit <- 100
            .offset <- (i - 1) * 100
            req <- sevenbridges::api(
              token,
              base_url = url,
              limit = .limit, offset = .offset,
              fields = fields, authorization = authorization, ...
            )
            req <- status_check(req)
            res$items <- c(res$items, req$items)
            setTxtProgressBar(pb, i)
          }
          cat("\n")
          res$href <- NULL
        } else {
          return(req)
        }
        return(res)
      } else {
        return(req)
      }
    },

    # user ---------------------------------------------------------------------
    user = function(username = NULL, ...) {
      "This call returns information about the authenticated user."

      if (is.null(username)) {
        req <- api(
          token = token,
          path = "user/",
          method = "GET", ...
        )
        message("username not provided, showing the currently authenticated user information")
      } else {
        req <- api(
          token = token,
          path = paste0("users/", username),
          method = "GET", ...
        )
      }

      .asUser(req)
    },

    # billing ------------------------------------------------------------------
    billing = function(id = NULL, breakdown = FALSE, ...) {
      "If no id provided, This call returns a list of paths used to access billing information via the API. else, This call lists all your billing groups, including groups that are pending or have been disabled. If breakdown = TRUE, This call returns a breakdown of spending per-project for the billing group specified by billing_group. For each project that the billing group is associated with, information is shown on the tasks run, including their initiating user (the runner), start and end times, and cost."

      if (is.null(id)) {
        # show api
        req <- api(path = "billing/groups", method = "GET", ...)
        req <- .asBillingList(req)
        if (length(req) == 1 && is(req, "SimpleList")) {
          req <- req[[1]]
        }

        return(req)
      } else {
        if (breakdown) {
          req <- api(
            path = paste0("billing/groups/", id, "/breakdown"),
            method = "GET", ...
          )
        } else {
          req <- api(path = paste0("billing/groups/", id), method = "GET", ...)
        }
        req <- .asBilling(req)

        return(req)
      }
    },
    invoice = function(id = NULL, ...) {
      "If no id provided, This call returns a list of invoices, with information about each, including whether or not the invoice is pending and the billing period it covers. The call returns information about all your available invoices, unless you use the query parameter bg_id to specify the ID of a particular billing group, in which case it will return the invoice incurred by that billing group only. If id was provided, This call retrieves information about a selected invoice, including the costs for analysis and storage, and the invoice period."

      if (is.null(id)) {
        req <- api(path = "billing/invoices", method = "GET", ...)
      } else {
        req <- api(path = paste0("billing/invoices/", id), method = "GET", ...)
      }

      req
    },

    # projects -----------------------------------------------------------------
    project_owner = function(owner = NULL, ...) {
      "List the projects owned by and accessible to a particular user. Each project's ID and URL will be returned."

      if (is.null(owner)) {
        stop("owner must be provided. For example, Nate.")
      }

      req <- api(
        token = token,
        base_url = url,
        path = paste0("projects/", owner),
        method = "GET", ...
      )

      res <- status_check(req)
      if (hasItems(res)) {
        rp <- parseItem(res)
        obj <- .asProjectList(rp)
      } else {
        message("not found")
        obj <- res
      }

      obj <- setAuth(obj, .self, "Project")
    },
    project_new = function(name = NULL, billing_group_id = NULL,
                           description = name, tags = list(),
                           type = "v2", locked = FALSE,
                           use_interruptible_instances = FALSE, ...) {
      "Create new projects, required parameters: name, billing_group_id, optional parameteres: tags, description, type, and settings."

      if (is.null(name) || is.null(billing_group_id)) {
        stop("name, description, and billing_group_id must be provided")
      }

      # check tags
      if (is.character(tags)) tags <- as.list(tags)

      body <- list(
        "name" = name,
        "type" = type,
        "description" = description,
        "tags" = tags,
        "billing_group" = billing_group_id,
        "settings" = list("locked" = locked, "use_interruptible_instances" = use_interruptible_instances)
      )

      res <- api(
        path = "projects", body = body,
        method = "POST", ...
      )

      res <- .asProject(res)
      res <- setAuth(res, .self, "Project")
    },

    # Project call
    project = function(name = NULL, id = NULL, index = NULL,
                       ignore.case = TRUE, exact = FALSE,
                       owner = NULL, detail = FALSE, ...) {
      "If no id or name provided, this call returns a list of all projects you are a member of. Each project's project_id and URL on the platform will be returned. If name or id provided, we do a match search the list."

      if (!is.null(id)) {
        req <- api(path = paste0("projects/", id), method = "GET", ...)
        res <- .asProject(req)
        res <- setAuth(res, .self, "Project")
        return(res)
      }

      # check owner
      if (is.null(owner)) {
        # show all projects
        req <- api(path = "projects", method = "GET", ...)
        res <- .asProjectList(req)
      } else {
        message("Owner: ", owner)
        req <- api(
          path = paste0("projects/", owner),
          method = "GET", ...
        )
        res <- .asProjectList(req)
      }

      res <- m.match(res,
        id = id, name = name,
        exact = exact,
        ignore.case = ignore.case
      )

      if (!length(res)) {
        return(NULL)
      }

      # if (length(res) == 1) {
      #   .id = res$id
      #   req = api(path = paste0("projects/", .id), method = "GET",  ...)
      #   res = .asProject(req)
      #   res = setAuth(res, .self, "Project")
      #   return(res)
      # }

      if (detail && length(res)) {
        if (is(res, "SimpleList")) {
          ids <- sapply(res, function(x) {
            x$id
          })
        } else {
          ids <- res$id
        }

        lst <- lapply(ids, function(id) {
          req <- api(path = paste0("projects/", id), method = "GET", ...)
          .asProject(req)
        })
        res <- ProjectList(lst)
      }

      # double check
      if (length(res) == 1 && is(res, "SimpleList")) {
        res <- res[[1]]
      }
      res <- setAuth(res, .self, "Project")
      res
    },

    # files --------------------------------------------------------------------
    file = function(name = NULL, id = NULL, project = NULL, exact = FALSE,
                    detail = FALSE, metadata = list(), origin.task = NULL,
                    tag = NULL, complete = FALSE,
                    search.engine = c("server", "brute"), ...) {
      "This call returns a list of all files in a specified project that you can access. For each file, the call returns: 1) Its ID 2) Its filename The project is specified as a query parameter in the call."

      search.engine <- match.arg(search.engine)

      if (is.null(id)) {
        if (is.null(project)) {
          stop("When file id is not provided, project id need to be provided.")
        }
      } else {
        if (length(id) > 1) {
          res <- iterId(id, .self$file, exact = exact, ...)
          return(res)
        }
        req <- api(path = paste0("files/", id), method = "GET", ...)
        res <- .asFiles(req)
        res <- setAuth(res, .self, "Files")
        return(res)
      }

      # build query
      .query <- list(project = project)

      if (length(metadata)) {
        new.meta <- unlist(metadata)
        names(new.meta) <- sapply(
          names(new.meta),
          function(nm) paste("metadata", nm, sep = ".")
        )
        .query <- c(.query, as.list(new.meta))
      }

      if (!is.null(origin.task)) {
        .query <- c(.query, list(origin.task = origin.task))
      }

      if (detail) {
        .query <- c(.query, list(fields = "_all"))
      }

      .split_item <- function(x, list_name = NULL) {
        if (length(x) > 1) {
          names(x) <- rep(list_name, length(x))
          x
        } else {
          if (is.list(x)) {
            x <- x[[1]]
          }
          res <- list(x)
          names(res) <- list_name
          res
        }
      }

      if (!is.null(tag)) {
        .new_tag <- .split_item(tag, "tag")
        # encode the tag for cases like "#1"
        .new_tag <- lapply(.new_tag, URLencode, TRUE)
        .query <- c(.query, .new_tag)
      }



      if (is.null(name)) {
        # if no id, no name, list all

        if (length(metadata) || length(origin.task) || length(tag)) {
          complete <- FALSE
        }

        req <- api(
          path = "files", method = "GET",
          query = .query, complete = complete, ...
        )

        res <- .asFilesList(req)

        res <- setAuth(res, .self, "Files")
        if (length(res) == 1) {
          return(res[[1]])
        } else {
          return(res)
        }
      }

      # search now by name or multiple names
      # get all files
      switch(search.engine,
        server = {
          if (exact) {
            .query <- c(.split_item(name, "name"), .query)
            req <- api(
              path = "files", method = "GET",
              query = .query, complete = FALSE, ...
            )
            res <- .asFilesList(req)
            if (length(res) == 1) res <- res[[1]]
          } else {
            # use brute
            req <- api(
              path = "files", method = "GET",
              query = .query, complete = complete, ...
            )
            res <- .asFilesList(req)
            res <- m.match(res, id = id, name = name, exact = exact)
          }
        },
        brute = {
          req <- api(
            path = "files", method = "GET",
            query = .query, complete = complete, ...
          )
          res <- .asFilesList(req)
          res <- m.match(res, id = id, name = name, exact = exact)
        }
      )

      if (!length(res)) {
        return(NULL)
      }
      res <- setAuth(res, .self, "Files")
      res
    },
    public_file = function(...) {
      file(project = "admin/sbg-public-data", ...)
    },
    copyFile = function(id, project = NULL, name = "") {
      if (is.null(project)) {
        stop("project ID need to be provided, to which the file is copied to")
      }

      # iteratively
      if (length(id) > 1) {
        ids <- as.character(id)
        for (i in ids) {
          message("copying: ", i)
          copyFile(i, project = project, name = name)
        }
      } else {
        body <- list(
          project = project,
          name = name
        )
        res <- api(
          path = paste0("files/", id, "/actions/copy"),
          body = body, method = "POST"
        )
        res <- .asFiles(res)
        setAuth(res, .self, "Files")
      }
    },
    copy_file = function(id, project = NULL, name = "") {
      copyFile(id = id, project = project, name = name)
    },

    # apps ---------------------------------------------------------------------
    app = function(name = NULL, id = NULL, exact = FALSE, ignore.case = TRUE,
                   detail = FALSE, project = NULL, query = NULL,
                   visibility = c("project", "public"),
                   revision = NULL, complete = FALSE, ...) {
      visibility <- match.arg(visibility)

      if (visibility == "public") {
        query <- c(query, list(visibility = "public"))
      }

      # if id specified, does not have to list all

      if (!is.null(id)) {
        req <- api(
          path = paste0("apps/", .update_revision(id, revision)),
          method = "GET", query = query, ...
        )
        return(setAuth(.asApp(req), .self, "App"))
      }

      # list all apps first
      if (is.null(project)) {
        req <- api(
          path = "apps", method = "GET",
          query = query, complete = complete, ...
        )
        # browser()
        # if (complete) {
        #     res = lapply(req$it, function(x) {
        #         as.list(.asAppList(x))
        #     })
        #     res = do.call(c, res)
        #     res = do.call(AppList, res)
        # } else {
        # res = .asAppList(req)
        # }
      } else {
        req <- api(
          path = "apps", method = "GET",
          query = c(list(project = project), query),
          complete = complete, ...
        )
        # if (complete) {
        #     res = lapply(req, function(x) {
        #         as.list(.asAppList(x))
        #     })
        #     res = do.call(c, res)
        #     res = do.call(AppList, res)
        # } else {
        # res = .asAppList(req)
        # }
      }
      res <- .asAppList(req)
      # match
      res <- m.match(res,
        id = id, name = name, exact = exact,
        ignore.case = ignore.case
      )

      if (length(res) == 1) {
        .id <- res$id
        req <- api(
          path = paste0("apps/", .update_revision(.id, revision)),
          method = "GET", query = query, ...
        )
        res <- .asApp(req)
        return(setAuth(res, .self, "App"))
      }

      if (detail && length(res)) {
        if (is(res, "AppList")) {
          ids <- sapply(res, function(x) {
            x$id
          })
        } else {
          ids <- res$id
        }

        lst <- lapply(ids, function(id) {
          if (is.null(project)) {
            req <- api(
              path = paste0("apps/", id),
              query = query,
              method = "GET", ...
            )
          } else {
            req <- api(
              path = paste0("apps/", id), method = "GET",
              query = c(list(project = project), query), ...
            )
          }
          .asApp(req)
        })
        res <- AppList(lst)
      }

      if (!length(res)) {
        return(NULL)
      }

      setAuth(res, .self, "App")
    },
    public_app = function(...) {
      app(visibility = "public", ...)
    },
    copyApp = function(id, project = NULL, name = "") {
      if (is.null(project)) {
        stop("project ID need to be provided, to which the file is copied to")
      }

      # iteratively
      if (length(id) > 1) {
        ids <- as.character(id)
        for (i in ids) {
          message("copying: ", i)
          copyApp(i, project = project, name = name)
        }
      } else {
        body <- list(
          project = project,
          name = name
        )
        res <- api(
          path = paste0("apps/", id, "/actions/copy"),
          body = body, method = "POST"
        )
        res <- .asApp(res)
        setAuth(res, .self, "App")
      }
    },
    copy_app = function(id, project = NULL, name = "") {
      copyApp(id = id, project = project, name = name)
    },

    # tasks --------------------------------------------------------------------
    task = function(name = NULL, id = NULL, project = NULL, parent = NULL,
                    exact = FALSE, detail = FALSE,
                    status = c("all", "queued", "draft", "running", "completed", "aborted", "failed"), ...) {
      status <- match.arg(status)

      if (!is.null(id)) {
        req <- api(path = paste0("tasks/", id), method = "GET", ...)
        res <- .asTask(req)
        res <- setAuth(res, .self, "Task")
        return(res)
      }

      if (!is.null(parent)) {
        if (status == "all") {
          req <- api(path = "tasks", method = "GET", query = list(parent = parent), ...)
        } else {
          req <- api(
            path = "tasks", method = "GET",
            query = list(status = status, parent = parent), ...
          )
        }
      } else {
        if (is.null(project)) {
          # list all files
          if (status == "all") {
            req <- api(path = "tasks", method = "GET", ...)
          } else {
            req <- api(path = "tasks", method = "GET", query = list(status = status), ...)
          }
        } else {
          # list all files
          if (status == "all") {
            req <- api(
              path = paste0("projects/", project, "/tasks"),
              method = "GET", ...
            )
            # req = api(path = "tasks",  method = "GET", query = list(project = project), ...)
          } else {
            req <- api(
              path = paste0("projects/", project, "/tasks"),
              method = "GET",
              query = list(status = status), ...
            )
          }
        }
      }

      res <- .asTaskList(req)

      # matching
      res <- m.match(res, id = id, name = name, exact = exact)

      # if (length(res) == 1) {
      #     .id = res$id
      #     req = api(path = paste0("tasks/", .id), method = "GET",  ...)
      #     res = .asTask(req)
      #     res = setAuth(res, .self, "Task")
      #     return(res)
      # }

      if (length(res)) {
        if (detail) {
          if (is(res, "TaskList")) {
            ids <- sapply(res, function(x) {
              x$id
            })
          } else {
            ids <- res$id
          }

          lst <- lapply(ids, function(id) {
            req <- api(path = paste0("tasks/", id), method = "GET", ...)
            .asTask(req)
          })
          res <- TaskList(lst)
        }
      } else {
        return(NULL)
      }

      res <- setAuth(res, .self, "Task")
      res
    },

    # volumes ------------------------------------------------------------------
    mount = function(mountPoint = NULL, projectId = NULL,
                     ignore.stdout = TRUE, sudo = TRUE, ...) {
      fs <<- FS(authToken = token, ...)
      fs$mount(
        mountPoint = mountPoint,
        projectId = projectId,
        ignore.stdout = ignore.stdout,
        sudo = sudo
      )
    },
    unmount = function(...) {
      fs$unmount(...)
    },
    get_id_from_path = function(p) {
      ids <- a$api(
        path = "action/files/get_ids",
        method = "POST",
        body = as.list(p)
      )
      idx <- unlist(lapply(ids, is.null))
      if (sum(idx)) {
        message("no id for following file: \n", paste(df.path[idx], collapse = "\n"))
      }
      if (sum(!idx)) {
        id.valid <- unlist(ids[!idx])
      } else {
        id.valid <- NULL
      }
      id.valid
    },
    add_volume = function(name = NULL, type = c("s3", "gcs"), root_url = NULL,
                          bucket = NULL, prefix = "", access_key_id = NULL,
                          secret_access_key = NULL, client_email = NULL,
                          private_key = NULL, sse_algorithm = "AES256",
                          aws_canned_acl = NULL, access_mode = c("RW", "RO")) {
      if (is.null(name)) {
        stop("Please provide name, the name of the volume. It must be unique from all other volumes for this user.")
      }

      type <- match.arg(type)
      access_mode <- match.arg(access_mode)

      if (is.null(root_url)) {
        root_url <- switch(type,
          s3 = "https://s3.amazonaws.com",
          gcs = "https://www.googleapis.com/"
        )
      }

      if (type == "s3" && !is.null(access_key_id) && !is.null(secret_access_key)) {
        credentials <- list(
          access_key_id = access_key_id,
          secret_access_key = secret_access_key
        )
      } else if (type == "gcs" && !is.null(client_email) && !is.null(private_key)) {
        credentials <- list(
          client_email = client_email,
          private_key = private_key
        )
      } else {
        stop("credentials are needed")
      }

      body <- list(
        name = name,
        service = list(
          type = type,
          bucket = bucket,
          root_url = root_url,
          prefix = prefix,
          credentials = credentials,
          properties = list(sse_algorithm = sse_algorithm)
        ),
        access_mode = access_mode
      )

      res <- api(path = "storage/volumes", body = body, method = "POST")
      res <- .asVolume(res)
      res <- setAuth(res, .self, "Volume")
      res
    },
    volume = function(name = NULL, id = NULL, index = NULL, ignore.case = TRUE,
                      exact = FALSE, detail = FALSE, ...) {
      "If no id or name provided, this call returns a list of all volumes you are a member of. If name or id provided, we did a match search the list."

      if (!is.null(id)) {
        req <- api(path = paste0("storage/volumes/", id), method = "GET", ...)
        res <- .asVolume(req)
        res <- setAuth(res, .self, "Volume")
        return(res)
      }

      # list "all"
      req <- api(path = "storage/volumes", method = "GET", ...)
      res <- .asVolumeList(req)
      if (is.null(name)) {
        res <- setAuth(res, .self, "Volume")
        return(res)
      }

      res <- m.match(res,
        id = id, name = name, exact = exact,
        ignore.case = ignore.case
      )

      if (!length(res)) {
        return(NULL)
      }

      if (detail && length(res)) {
        if (is(res, "SimpleList")) {
          ids <- sapply(res, function(x) {
            x$id
          })
        } else {
          ids <- res$id
        }

        lst <- lapply(ids, function(id) {
          req <- api(path = paste0("storage/volumes/", id), method = "GET", ...)
          .asVolume(req)
        })
        res <- VolumeList(lst)
      }

      # double check
      if (length(res) == 1 && is(res, "SimpleList")) {
        res <- res[[1]]
      }
      res <- setAuth(res, .self, "Volume")
      res
    },

    # actions ------------------------------------------------------------------
    bulk_file_copy = function(file_ids, project, ...) {
      "Copy files between projects in a batch."
      api(
        path = "action/files/copy",
        body = list("project" = project, "file_ids" = file_ids),
        method = "POST", ...
      )
    },
    send_feedback = function(text, type = c("idea", "thought", "problem"), referrer = NULL, ...) {
      "Send feedback to Seven Bridges."
      text <- paste0(as.character(text), collapse = " ")
      type <- match.arg(type)
      if (is.null(referrer)) {
        username <- suppressMessages(user()$"username")
        referrer <- paste(username, "sent from sevenbridges-r")
      }
      api(
        path = "action/notifications/feedback",
        body = list("text" = text, "type" = type, "referrer" = referrer),
        method = "POST", ...
      )
    },

    # enterprise ---------------------------------------------------------------
    division = function(id = NULL, ...) {
      "List all divisions or get details of a division."
      if (is.null(id)) {
        req <- api(path = "divisions/", method = "GET", ...)
      } else {
        req <- api(path = paste0("divisions/", id), method = "GET", ...)
      }

      # only one division
      if (is.null(req$items) & !is.null(req$id)) {
        res <- .asDivision(req)
        res$auth <- .self
      }

      # multiple divisions
      if (!is.null(req$items)) {
        res <- .asDivisionList(req)
        setAuth(res, .self, "Division")
      }

      res
    },

    # rate limit ---------------------------------------------------------------
    rate_limit = function(...) {
      "This call returns information about your current rate limit. This is the number of API calls you can make in one hour."

      req <- api(path = "rate_limit", method = "GET", ...)

      .asRate(req)
    },

    # bulk ---------------------------------------------------------------------
    bulk_file_get = function(file_ids, ...) {
      "Get details of multiple files."
      if (length(file_ids) <= 100L) {
        req <- api(
          path = "bulk/files/get",
          body = list("file_ids" = file_ids),
          method = "POST", ...
        )
        req_noname <- sapply(req$items, unname)
      } else {
        # if more than 100 files, split into 100-sized chunks
        file_ids_lst <- split(file_ids, ceiling(seq_along(file_ids) / 100L))
        # loop over
        req <- vector("list", length(file_ids_lst))
        for (i in 1L:length(file_ids_lst)) {
          req[[i]] <- api(
            path = "bulk/files/get",
            body = list("file_ids" = file_ids_lst[[i]]),
            method = "POST", ...
          )
        }
        # merge all
        req_noname <- sapply(unname(unlist(unlist(req, recursive = FALSE), recursive = FALSE)), unname)
      }

      req <- list("items" = req_noname)

      res <- .asFilesList(req)
      setAuth(res, .self, "Files")

      res
    },
    bulk_file_edit = function(...) {
      "Edit details of multiple files (preserving the omitted fields)."
      NULL
    },
    bulk_file_update = function(...) {
      "Update details of multiple files (removing the omitted fields)."
      NULL
    },
    bulk_file_delete = function(file_ids, ...) {
      "Delete multiple files."
      if (length(file_ids) <= 100L) {
        req <- api(
          path = "bulk/files/delete",
          body = list("file_ids" = file_ids),
          method = "POST", ...
        )
        req_noname <- sapply(req$items, unname)
      } else {
        # if more than 100 files, split into 100-sized chunks
        file_ids_lst <- split(file_ids, ceiling(seq_along(file_ids) / 100L))
        # loop over
        req <- vector("list", length(file_ids_lst))
        for (i in 1L:length(file_ids_lst)) {
          req[[i]] <- api(
            path = "bulk/files/delete",
            body = list("file_ids" = file_ids_lst[[i]]),
            method = "POST", ...
          )
        }
        # merge all
        req_noname <- sapply(unname(unlist(unlist(req, recursive = FALSE), recursive = FALSE)), unname)
      }

      req <- list("items" = req_noname)

      res <- .asFilesList(req)
      setAuth(res, .self, "Files")

      res
    },
    bulk_task_get = function(task_ids, ...) {
      "Get details of multiple tasks."
      if (length(task_ids) <= 100L) {
        req <- api(
          path = "bulk/tasks/get",
          body = list("task_ids" = task_ids),
          method = "POST", ...
        )
        req_noname <- sapply(req$items, unname)
      } else {
        # if more than 100 tasks, split into 100-sized chunks
        task_ids_lst <- split(task_ids, ceiling(seq_along(task_ids) / 100L))
        # loop over
        req <- vector("list", length(task_ids_lst))
        for (i in 1L:length(task_ids_lst)) {
          req[[i]] <- api(
            path = "bulk/tasks/get",
            body = list("task_ids" = task_ids_lst[[i]]),
            method = "POST", ...
          )
        }
        # merge all
        req_noname <- sapply(unname(unlist(unlist(req, recursive = FALSE), recursive = FALSE)), unname)
      }

      req <- list("items" = req_noname)

      res <- .asTaskList(req)
      setAuth(res, .self, "Task")

      res
    },
    bulk_volume_import = function(...) {
      "Bulk import from volumes."
      NULL
    },
    bulk_volume_export = function(...) {
      "Bulk export to volumes."
      NULL
    },
    bulk_volume_get_import = function(...) {
      "Get details of a bulk import job."
      NULL
    },
    bulk_volume_get_export = function(...) {
      "Get details of a bulk export job."
      NULL
    },

    # show ---------------------------------------------------------------------
    show = function() {
      .showFields(.self, "== Auth ==", values = c("url", "token"))
    }
  )
)

setClassUnion("AuthORNULL", c("Auth", "NULL"))
