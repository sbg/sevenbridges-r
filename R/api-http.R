#' Core HTTP logic for Seven Bridges API
#'
#' Core HTTP logic for Seven Bridges API
#'
#' Used for advanced users and the core method for higher level API
#' in this package, please refer to the easy api vignette and
#' additional vignettes pages for more convenient usage.
#'
#' @param token authenticate token string.
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
#' @param base_url defeault is \code{"https://api.sbgenomics.com/v2"}
#' @param ... passed to GET/POST/PUT/DELETE/PATCH call.
#'
#' @return returned request list of httr
#'
#' @references
#' \url{http://docs.sevenbridges.com/v1.0/page/api}
#'
#' @export api
#' @examples
#' token = "your_token"
#' \donttest{
#' # list projects
#' api(token = token, path = "projects", method = "GET")}
api = function(token = NULL, version = 'v2', path = NULL,
               method = c('GET', 'POST', 'PUT', 'DELETE', 'PATCH'),
               query = NULL, body = list(),
               encode = c("json", "form", "multipart"),
               limit = getOption("sevenbridges")$limit,
               offset = getOption("sevenbridges")$offset,
               base_url = paste0("https://api.sbgenomics.com/", version, "/"),
               ...) {

    if (is.null(token))
        stop('token must be provided')

    method = match.arg(method)
    encode = match.arg(encode)

    headers = c(
        'X-SBG-Auth-Token' = token
        # 'Accept' = 'application/json',
        # 'Content-type' = 'application/json'
    )

    # setup query
    query = c(query, list(limit = limit, offset = offset))
    idx = !sapply(query, is.null)
    if (any(idx)) {
        query <- query[idx]
    } else {
        query <- NULL
    }

    switch(method,
           GET = {
               GET2(paste0(base_url, path),
                    add_headers(.headers = headers), query = query, ...)
           },
           POST = {
               # stopifnot(is.list(body))
               # body_json = toJSON(body, auto_unbox = TRUE)
               POST2(paste0(base_url, path),
                     add_headers(.headers = headers), query = query,
                     body = body, encode = encode,  ...)
           },
           PUT = {
               # stopifnot(is.list(body))
               # body_json = toJSON(body, auto_unbox = TRUE)
               PUT(paste0(base_url, path),
                   add_headers(.headers = headers),
                   body = body, encode = encode, ...)
           },
           DELETE = {
               DELETE(paste0(base_url, path),
                      add_headers(.headers = headers), ...)
           },
           PATCH = {
               # stopifnot(is.list(body))
               # body_json = toJSON(body, auto_unbox = TRUE)
               PATCH(paste0(base_url, path),
                     add_headers(.headers = headers),
                     body = body,
                     encode = encode, ...)
           })

}

#' Check request status
#'
#' Check request status
#'
#' @return request content or the message
#'
#' @keywords internal
status_check = function (req, as = 'parsed', ...) {

    if (status_code(req) %in% c('200', '201', '202', '204')) {

        res <- content(req, as = as, ...)
        if(!is.null(res))
            attr(res, 'response') = req
        return(res)

    } else if (status_code(req) %in% c('401', '403', '404', '503')) {

        msg = content(req, as = as, ...)$message
        stop(paste0('HTTP Status ', status_code(req), ': ', msg), call. = FALSE)

    } else {

        if ("message" %in% names(content(req, as = as, ...))) {

            msg = content(req, as = as, ...)$message

        } else {

            msg = NULL

        }

        if (is.null(msg)) {
            if (status_code(req) %in% names(.codes)) {
                msg <- .codes[[status_code(req)]]
            }
            if (is.null(msg)) {
                print(content(req, as = as, ...))
                stop(paste('Error of unknown type occured', status_code(req)))
            } else {
                stop(paste0('HTTP Status ', status_code(req), ': ', msg), call. = FALSE)
            }
        } else {
            stop(paste0('HTTP Status ', status_code(req), ': ', msg), call. = FALSE)
        }

    }

}

# This code is from specification v2
.codes = list(

    # 0xxx: Platform maintenance errors
    "0" = "Seven Bridges Genomics platform is currently under maintenance.",

    # 1xxx: General errors
    "1000" = "Allowed Rate limit exceeded! Check response headers.",

    # 2xxx: Users
    "2000" = "User service is currently unavailable",
    "2001" = "Not enough privileges to access requested user info.",
    "2002" = "Requested user does not exist.",
    "2003" = "Requested user already exists.",

    # 3xxx: Projects
    "3000" = "Project service is currently unavailable",
    "3001" = "Not enough privileges to access requested project/member",
    "3002" = "Requested project or member does not exist",
    "3003" = "Requested project/member already exists",
    "3004" = "Owner's username must not be null or empty string",
    "3005" = "Member username must not be null or empty string",
    "3006" = "Project id must not be null or an empty string",
    "3007" = "Project name must not be null or empty string",
    "3008" = "Billing group id must not be null or empty string",
    "3009" = "Project type must not be null or empty string",
    "3010" = "Project type can be either CWL for developer projects or LEGACY for old style projects",
    "3011" = "Project permissions must not null or an empty value",
    "3012" = "Malformed project id. Expecting owner/project",

    # 4xxx: Billing
    "4000" = "Billing service is currently unavailable",
    "4001" = "Insufficient privileges to access the requested billing group/invoice",
    "4002" = "Requested billing group/invoice does not exist",
    "4003" = "Requested billing group/invoice already exist",
    "4004" = "Billing group id must not be null or an empty string",
    "4005" = "Billing group id must be a valid UUID",
    "4006" = "You are not a member of this billing group",
    "4007" = "Invoice id must not be null or an empty string",

    # 5xxx: Files
    "5000" = "File service is currently unavailable",
    "5001" = "Insufficient privileges to access the requested file",
    "5002" = "Requested file does not exist",
    "5004" = "Requested file already exists",
    "5005" = "Malformed project query parameter. Expecting ?project=owner/project",
    "5006" = "Metadata validation failed" ,
    "5007" = "File copy failed",
    "5008" = "File renaming not allowed",

    # 6xxx: Apps
    "6000" = "App service is currently unavailable",
    "6001" = "Insufficient privileges to access the requested app/revision",
    "6002" = "Requested app/revision does not exist",
    "6003" = "Requested app/revision already exists",
    "6004" = "App name must not be null or an empty string",
    "6006" = "Project owner must not be null or an empty string",
    "6007" = "Project must not be null or an empty string",
    "6008" = "App revision must not be null or an empty string",
    "6009" = "App revision must be a valid integer",
    "6009" = "Source project must not be null or an empty string",
    "6010" = "Source app must not be null or an empty string.",
    "6011" = "Malformed app id. Expecting owner/project/app_name/revision",

    # 7xxx: Tasks
    "7000" = "Task service is currently unavailable.",
    "7001" = "Insufficient privileges to access the requested task.",
    "7002" = "Requested task does not exist",
    "7003" = "Requested task already exists",
    "7004" = "Task id must not be empty or null or an empty string",
    "7005" = "Task id must be a valid UUID",
    "7006" = "Invalid task status. Allowed values = [QUEUED, DRAFT, RUNNING, COMPLETED, ABORTED, FAILED]",
    "7007" = "This action is only available for DRAFT tasks",
    "7008" = "This action is only available for RUNNING tasks",
    "7009" = "Invalid task action. Action can be performed only on DRAFT or RUNNING tasks.",
    "7010" = "Invalid task action. Action can be performed on DRAFT tasks",
    "7011" = "Invalid task action. Action can be performed on RUNNING tasks",
    "7012" = "Missing inputs.",
    "7013" = "Invalid task action.",
    "7014" = "Action parameter must not be null or an empty string.",
    "7015" = "App Id must not be null or an empty string.",

    # 9xxx: General validation errors
    "9000" = "Bad request.",
    "9001" = "Unauthorized.",
    "9002" = "Forbidden.",
    "9003" = "Not Found.",
    "9004" = "Unexpected error happened.",
    "9005" = "Service Unavailable.",
    "9006" = "Method Not Allowed.",
    "9007" = "Conflict.",
    "9008" = "Unsupported Media Type.",
    "9009" = "An Error occurred during the decoding of the request content."

)

# customize underlying http logic
# (handle_url2, build_url2, GET2, POST2)

handle_url2 = function(handle = NULL, url = NULL, ...) {

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

build_url2 = function(url) {

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

    paste0(scheme, "://", url$username, if (!is.null(url$password))
        ":", url$password, if (!is.null(url$username))
            "@", hostname, port, "/", path, params, query, if (!is.null(url$fragment))
                "#", url$fragment)

}

GET2 = function(url = NULL, config = list(), ..., handle = NULL) {

    hu <- handle_url2(handle, url, ...)
    req <- eval(parse(text = 'httr:::request_build("GET", hu$url, config, ...)'))

    return(eval(parse(text = "httr:::request_perform(req, hu$handle$handle)")))

}

POST2 = function(url = NULL, config = list(), ...,
                 body = NULL, encode = c("json", "form", "multipart"),
                 multipart = TRUE, handle = NULL) {

    if (!missing(multipart)) {
        warning("multipart is deprecated, please use encode argument instead",
                call. = FALSE)
        encode <- ifelse(multipart, "multipart", "form")
    }

    encode <- match.arg(encode)
    hu <- handle_url2(handle, url, ...)
    req <- eval(parse(text = 'httr:::request_build("POST", hu$url, httr:::body_config(body, encode), config, ...)'))

    return(eval(parse(text = "httr:::request_perform(req, hu$handle$handle)")))

}
