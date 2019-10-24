#' Core HTTP logic for Seven Bridges API
#'
#' Core HTTP logic for Seven Bridges API
#'
#' Used for advanced users and the core method for higher level API
#' in this package, please refer to the easy api vignette and
#' additional vignettes pages for more convenient usage.
#'
#' @param token API auth token or \code{access_token} for
#' Seven Bridges single sign-on.
#' @param version API version number, default is \code{v2}.
#' @param path path connected with \code{base_url}.
#' @param method one of \code{"GET"}, \code{"POST"},
#' \code{"PUT"}, or \code{"Delete"}.
#' @param query Passed to httr package GET/POST call.
#' @param body Passed to httr package GET/POST/PUT/DELETE call.
#' @param encode If the body is a named list, how should it be
#' encoded? Can be one of \code{"json"} (application/json),
#' \code{"form"} (application/x-www-form-urlencoded),
#' or \code{"multipart"} (multipart/form-data).
#' Default is \code{"json"}.
#' For \code{"multipart"}, list elements can be strings
#' or objects created by \code{\link[httr]{upload_file}}.
#' For "form", elements are coerced to strings and escaped,
#' use \code{I()} to prevent double-escaping.
#' For \code{"json"}, parameters are automatically "unboxed"
#' (i.e. length 1 vectors are converted to scalars). To preserve
#' a length 1 vector as a vector, wrap in \code{I()}.
#' @param limit How many results to return
#' @param offset The point at which to start displaying them
#' @param advance_access Enable advance access features?
#' Default is \code{FALSE}.
#' @param authorization Logical. Is the \code{token} an API
#' auth token (\code{FALSE}) or an access token from the
#' Seven Bridges single sign-on (\code{TRUE})?
#' @param fields All API calls take the optional query parameter fields.
#' This parameter enables you to specify the fields you want to be returned
#' when listing resources (e.g. all your projects) or getting details of a
#' specific resource (e.g. a given project). For example, fields="id,name,size"
#' to return the fields id, name and size for files. More details please check
#' \url{https://docs.sevenbridges.com/docs/the-api#section-general-api-information}
#' @param base_url defeault is \code{"https://api.sbgenomics.com/v2"}
#' @param ... passed to GET/POST/PUT/DELETE/PATCH call.
#'
#' @return returned request list of httr
#'
#' @references
#' \url{https://docs.sevenbridges.com/v1.0/page/api}
#'
#' @export api
#' @examples
#' token <- "your_token"
#' # list projects
#' \dontrun{
#' api(token = token, path = "projects", method = "GET")}
api <- function(token = NULL, version = "v2", path = NULL,
                method = c("GET", "POST", "PUT", "DELETE", "PATCH"),
                query = NULL, body = list(),
                encode = c("json", "form", "multipart"),
                limit = getOption("sevenbridges")$limit,
                offset = getOption("sevenbridges")$offset,
                advance_access = getOption("sevenbridges")$advance_access,
                authorization = FALSE,
                fields = NULL,
                base_url = paste0("https://api.sbgenomics.com/", version, "/"),
                ...) {
  if (is.null(token)) stop("token must be provided")

  method <- match.arg(method)
  encode <- match.arg(encode)

  if (authorization) {
    headers <- c("Authorization" = paste("Bearer", token, sep = " "))
  } else {
    headers <- c(
      "X-SBG-Auth-Token" = token
      # "Accept" = "application/json",
      # "Content-type" = "application/json"
    )
  }

  # add optional advance access flag
  if (advance_access) headers <- c(headers, "X-SBG-advance-access" = "advance")

  # setup query
  query <- c(query, list(limit = limit, offset = offset, fields = fields))
  idx <- !sapply(query, is.null)
  if (any(idx)) {
    query <- query[idx]
  } else {
    query <- NULL
  }

  switch(method,
    GET = {
      GET2(paste0(base_url, path),
        add_headers(.headers = headers),
        query = query, ...
      )
    },
    POST = {
      # stopifnot(is.list(body))
      # body_json = toJSON(body, auto_unbox = TRUE)
      POST2(paste0(base_url, path),
        add_headers(.headers = headers),
        query = query,
        body = body, encode = encode, ...
      )
    },
    PUT = {
      # stopifnot(is.list(body))
      # body_json = toJSON(body, auto_unbox = TRUE)
      PUT(paste0(base_url, path),
        add_headers(.headers = headers),
        body = body, encode = encode, ...
      )
    },
    DELETE = {
      DELETE(
        paste0(base_url, path),
        add_headers(.headers = headers), ...
      )
    },
    PATCH = {
      # stopifnot(is.list(body))
      # body_json = toJSON(body, auto_unbox = TRUE)
      PATCH(paste0(base_url, path),
        add_headers(.headers = headers),
        body = body,
        encode = encode, ...
      )
    }
  )
}

#' Check request status
#'
#' Check request status
#'
#' @return request content or the message
#'
#' @keywords internal
status_check <- function(req, as = "parsed", ...) {
  if (status_code(req) %in% c("200", "201", "202", "204")) {
    res <- content(req, as = as, ...)
    if (!is.null(res)) {
      attr(res, "response") <- req
    }
    return(res)
  } else if (status_code(req) %in% c("401", "403", "404", "503")) {
    msg <- content(req, as = as, ...)$message
    stop(paste0("HTTP Status ", status_code(req), ": ", msg), call. = FALSE)
  } else {
    if ("message" %in% names(content(req, as = as, ...))) {
      msg <- content(req, as = as, ...)$message
    } else {
      msg <- NULL
    }

    if (is.null(msg)) {
      if (status_code(req) %in% names(.codes)) {
        msg <- .codes[[status_code(req)]]
      }
      if (is.null(msg)) {
        print(content(req, as = as, ...))
        stop(paste("Error of unknown type occured", status_code(req)))
      } else {
        stop(paste0("HTTP Status ", status_code(req), ": ", msg), call. = FALSE)
      }
    } else {
      stop(paste0("HTTP Status ", status_code(req), ": ", msg), call. = FALSE)
    }
  }
}

# Status codes are from API v2 specification
# https://docs.sevenbridges.com/reference#api-status-codes

.codes <- list(

  # 0xxx: Platform maintenance errors
  "0" = "The Platform is currently under maintenance.",

  # 1xxx: General errors
  "1000" = "Rate limit exceeded! Check response headers.",

  # 2xxx: User errors
  "2000" = "User service is currently unavailable.",
  "2001" = "Not enough privileges to access requested user info.",
  "2002" = "Requested user does not exist.",
  "2003" = "Requested user already exists.",

  # 3xxx: Projects errors
  "3000" = "Project service is currently unavailable.",
  "3001" = "Not enough privileges to access the requested project/member.",
  "3002" = "Requested project or member does not exist.",
  "3003" = "Requested project/member already exists.",
  "3004" = "Owner's username must not be null or empty string.",
  "3005" = "Member username must not be null or empty string.",
  "3006" = "Project id must not be null or an empty string.",
  "3007" = "Project name must not be null or empty string.",
  "3008" = "Billing group id must not be null or empty string.",
  "3009" = "Project type must not be null or empty string.",
  "3010" = "Project type can be either v2 for standard projects or v1 for LEGACY projects.",
  "3011" = "Project permissions must not null or an empty value.",
  "3012" = "Malformed project id. Expecting `owner/project`.",
  "3013" = "Please provide all permissions data.",

  # 4xxx: Billing errors
  "4000" = "Billing service is currently unavailable.",
  "4001" = "Insufficient privileges to access the requested billing group/invoice.",
  "4002" = "Requested billing group/invoice does not exist.",
  "4003" = "Requested billing group/invoice already exist.",
  "4004" = "Billing group id must not be null or an empty string.",
  "4005" = "Billing group id must be a valid UUID.",
  "4006" = "You are not a member of this billing group.",
  "4007" = "Invoice id must not be null or an empty string.",

  # 5xxx: Files errors
  "5000" = "File service is currently unavailable.",
  "5001" = "Insufficient privileges to access the requested file.",
  "5002" = "Requested file does not exist.",
  "5003" = "Requested file already exists.",
  "5004" = "File id must not be null or an empty string.",
  "5005" = "Malformed project query parameter. Expecting `?project=owner/project`",
  "5006" = "Metadata validation failed.",
  "5007" = "File copy failed.",
  "5008" = "File renaming not allowed.",
  "5009" = "Modifying metadata is not allowed.",
  "5010" = "Metadata service is currently unavailable.",
  "5011" = "Modifying file tags is not allowed.",
  "5012" = "Invalid `type` supplied. Allowed values: [folder].",
  "5014" = "Insufficient privileges to copy the requested file.",
  "5015" = "Moving files between projects is not supported.",
  "5017" = "Downloading folders is not supported.",
  "5018" = "Copying folders is not supported.",
  "5019" = "Archiving folders is not supported.",
  "5020" = "Restoring folders is not supported.",
  "5021" = "Deleting non-empty folders is not supported.",
  "5022" = "The parent specified is not a folder.",
  "5023" = "Updating folder details is not supported.",
  "5024" = "Invalid name parameter. Check the documentation.",
  "5025" = "Updating folder metadata is not supported.",
  "5026" = "Destination folder is not found.",
  "5027" = "Updating folder tags is not supported.",
  "5029" = "Missing `parent` or `project` field. These fields must be included together.",
  "5030" = "Requested folder already exists.",
  "5031" = "Providing both `parent` and `project` is not allowed.",
  "5032" = "Insufficient privileges to move the requested file.",
  "5033" = "Invalid request please check the documentation.",

  # 6xxx: Apps errors
  "6000" = "App service is currently unavailable.",
  "6001" = "Insufficient privileges to access the requested app/revision.",
  "6002" = "Requested app/revision does not exist.",
  "6003" = "Requested app/revision already exists.",
  "6004" = "App name must not be null or an empty string.",
  "6006" = "Project owner must not be null or an empty string.",
  "6007" = "Project must not be null or an empty string.",
  "6008" = "App revision must not be null or an empty string.",
  "6009" = "Destination project must not be null or an empty string.",
  "6010" = "Source app must not be null or an empty string.",
  "6011" = "Malformed app id. Expecting `owner/project/app_name/revision`.",
  "6012" = "Invalid visibility query parameter. Allowed values: [PUBLIC, PRIVATE].",

  # 7xxx: Tasks errors
  "7000" = "Task service is currently unavailable.",
  "7001" = "Insufficient privileges to access the requested task.",
  "7002" = "Requested task does not exist.",
  "7003" = "Requested task already exists.",
  "7004" = "Task ID must not be empty or null or an empty string.",
  "7005" = "Task ID must be a valid UUID.",
  "7006" = "Invalid task status. Allowed values: [QUEUED, DRAFT, RUNNING, COMPLETED, ABORTED, ABORTING, FAILED]",
  "7007" = "This action is only available for DRAFT tasks.",
  "7008" = "This action is only available for RUNNING tasks.",
  "7009" = "Invalid task action. Action can be performed only on DRAFT or RUNNING tasks.",
  "7010" = "Invalid task action. Action can be performed on DRAFT tasks.",
  "7011" = "Invalid task action. Action can be performed on tasks in the states: CREATING, RUNNING or QUEUED.",
  "7012" = "Missing inputs.",
  "7013" = "Invalid task action.",
  "7014" = "Action parameter must not be null or an empty string.",
  "7015" = "App Id must not be null or an empty string.",
  "7016" = "Invalid app url.",
  "7017" = "Only Common Workflow Language (CWL) tasks are supported.",
  "7018" = "Batch input property should reference input identifier or omitted. Empty value is not allowed.",
  "7019" = "Missing batch criteria.",
  "7020" = "Invalid batch type supplied. Allowed values: [criteria, item].",
  "7021" = "Batching can only be disabled if the task is submitted for execution.",
  "7022" = "Disabling batching action is only available for BATCH tasks.",
  "7023" = "Missing batch_by or batch_input fields. These fields must be included together.",
  "7024" = "Task can not be started due to validation errors.",
  "7026" = "Editing is available only for tasks which are in DRAFT status. Tasks which are in RUNNING and COMPLETED states can only be renamed.",

  # 8xxx: Upload errors
  "8000" = "Upload service is currently unavailable.",
  "8001" = "Insufficient privileges to access the requested upload.",
  "8002" = "Insufficient privileges to access the requested file.",
  "8003" = "Requested upload does not exist.",
  "8004" = "Requested file already exists.",
  "8005" = "Requested file does not exist.",
  "8006" = "Requested upload already exists.",
  "8007" = "Failed to complete upload.",
  "8008" = "Failed to reserve part for upload. Try again.",
  "8009" = "Failed to abort upload.",
  "8010" = "Malformed project id. Expecting `owner/project`.",
  "8011" = "Upload id must not be null or an empty string.",
  "8012" = "Part number is missing or invalid.",
  "8013" = "Invalid `init` request.",
  "8014" = "Invalid `part` report.",
  "8015" = "Invalid list of parts. Expecting an object with `parts`: [ array of part reports ].",

  # 9xxx: Volumes errors
  "9000" = "There was an error communicating with the service.",
  "9001" = "Could not obtain read access on the service.",
  "9002" = "Could not obtain cross-write access on the service.",
  "9003" = "Insufficient privileges to access the requested project.",
  "9004" = "Insufficient privileges to access the requested file.",
  "9005" = "Insufficient privileges to access the requested job.",
  "9006" = "Requested file cannot be exported.",
  "9007" = "Requested volume does not exist.",
  "9008" = "Requested job does not exist.",
  "9009" = "Requested file does not exist.",
  "9010" = "Requested volume name already exists.",
  "9011" = "Invalid request syntax.",
  "9012" = "Requested project or member does not exist.",
  "9013" = "Volume name must not be null or an empty string.",
  "9014" = "Volume name must consist of up to 32 English letters, numbers and underscores.",
  "9015" = "`access_mode` must be provided (either `RO` or `RW`).",
  "9016" = "`service` object must be provided.",
  "9017" = "`service` object is invalid. Check the documentation.",
  "9018" = "Cannot infer file name and none given.",
  "9019" = "`service` object is invalid. Check the documentation.",
  "9020" = "Insufficient privileges to access the requested volume.",
  "9021" = "Invalid time format. Check the documentation.",
  "9022" = "Invalid canned ACL selected (`aws_canned_acl`). Check the documentation.",
  "9032" = "Invalid server-side encryption selected (`sse_algorithm`). Check the documentation.",
  "9024" = "Invalid S3 storage class selected (`aws_storage_class`). Check the documentation.",
  "9025" = "Invalid private key given (`private_key`). Check the documentation.",
  "9026" = "The volume is not configured for writing access (`access mode` is not set to `RW`).",
  "9027" = "Exporting files across different cloud services is not yet supported.",
  "9028" = "Exporting files across different cloud services is not yet supported.",
  "9030" = "Volume name must consist of up to 32 English letters, numbers, and underscores.",
  "9057" = "This environment only supports `RO` buckets of type `GCS`.",
  "9058" = "This environment only supports `RO` buckets of type `S3`.",
  "9100" = "There was an error communicating with the service.",
  "9101" = "The volume is not configured for writing (`access mode` is not set to `RW`).",
  "9102" = "Insufficient privileges to access the requested project.",
  "9103" = "Location on volume is not accessible as configured.",
  "9104" = "Requested volume does not exist.",
  "9105" = "Location on volume not found.",
  "9106" = "Requested file does not exist.",
  "9107" = "Location on volume already contains a file.",
  "9108" = "Requested file already exists.",
  "9109" = "Requested file does not exist or not accessible.",

  # 9xxxx: General validation errors
  "90000" = "Bad request.",
  "90001" = "Unauthorized.",
  "90002" = "Forbidden.",
  "90003" = "Not found.",
  "90004" = "Unexpected error happened.",
  "90005" = "Service unavailable.",
  "90006" = "Method not allowed.",
  "90007" = "Conflict.",
  "90008" = "Unsupported Media Type.",
  "90009" = "An error occurred during the decoding of the request content."
)

# customize underlying http logic
# (handle_url2, build_url2, GET2, POST2)

handle_url2 <- function(handle = NULL, url = NULL, ...) {
  if (is.null(url) && is.null(handle)) {
    stop("Must specify at least one of url or handle")
  }
  if (is.null(handle)) handle <- handle_find(url)
  if (is.null(url)) url <- handle$url
  # workaround to bypass `:::` checks
  new <- eval(parse(text = "httr:::named(list(...))"))
  if (length(new) > 0 || eval(parse(text = "httr:::is.url(url)"))) {
    old <- httr::parse_url(url)
    url <- build_url2(modifyList(old, new))
  }

  list(handle = handle, url = url)
}

build_url2 <- function(url) {
  stopifnot(eval(parse(text = "httr:::is.url(url)")))
  scheme <- url$scheme
  hostname <- url$hostname
  if (!is.null(url$port)) {
    port <- paste0(":", url$port)
  }
  else {
    port <- NULL
  }
  path <- url$path
  if (!is.null(url$params)) {
    params <- paste0(";", url$params)
  } else {
    params <- NULL
  }
  if (is.list(url$query)) {
    url$query <- eval(parse(text = "httr:::compact(url$query)"))
    names <- curl_escape(names(url$query))
    values <- as.character(url$query)
    query <- paste0(names, "=", values, collapse = "&")
  } else {
    query <- url$query
  }
  if (!is.null(query)) {
    stopifnot(is.character(query), length(query) == 1)
    query <- paste0("?", query)
  }
  if (is.null(url$username) && !is.null(url$password)) {
    stop("Cannot set password without username")
  }

  paste0(scheme, "://", url$username, if (!is.null(url$password)) {
    ":"
  }, url$password, if (!is.null(url$username)) {
    "@"
  }, hostname, port, "/", path, params, query, if (!is.null(url$fragment)) {
    "#"
  }, url$fragment)
}

GET2 <- function(url = NULL, config = list(), ..., handle = NULL) {
  hu <- handle_url2(handle, url, ...)
  req <- eval(parse(text = 'httr:::request_build("GET", hu$url, config, ...)'))

  return(eval(parse(text = "httr:::request_perform(req, hu$handle$handle)")))
}

POST2 <- function(url = NULL, config = list(), ...,
                  body = NULL, encode = c("json", "form", "multipart"),
                  multipart = TRUE, handle = NULL) {
  if (!missing(multipart)) {
    warning("multipart is deprecated, please use encode argument instead",
      call. = FALSE
    )
    encode <- ifelse(multipart, "multipart", "form")
  }

  encode <- match.arg(encode)
  hu <- handle_url2(handle, url, ...)
  req <- eval(parse(text = 'httr:::request_build("POST", hu$url, httr:::body_config(body, encode), config, ...)'))

  return(eval(parse(text = "httr:::request_perform(req, hu$handle$handle)")))
}
