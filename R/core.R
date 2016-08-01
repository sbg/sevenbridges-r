#' wrapper of http logic for SBG API
#'
#' wrapper of http logic for SBG API
#'
#' Used for advanced users and the core method for higher level API in
#' this package, please refer to the easy api manual and the two
#' vignettes pages for more convenient usage.
#'
#' @param token authenticate token string.
#' @param version API version number, default 1.1.
#' @param path path connected with base_url.
#' @param method one of 'GET', 'POST', 'PUT', 'Delete'
#' @param query Passed to httr package GET/POST call.
#' @param body Passed to httr package GET/POST/PUT/DELETE call.
#' @param encode If the body is a named list, how should it be
#' encoded? Default here is "json". Can be one of form (application/x-www-form-urlencoded),
#' multipart, (multipart/form-data), or json (application/json).  For
#' "multipart", list elements can be strings or objects created by
#' upload_file. For "form", elements are coerced to strings and escaped,
#' use I() to prevent double-escaping. For "json", parameters are
#' automatically "unboxed" (i.e. length 1 vectors are converted to
#' scalars). To preserve a length 1 vector as a vector, wrap in I().
#' @param limit how many results to return 
#' @param offset the point at which to start displaying them
#' @param base_url defeault is 'https://api.sbgenomics.com/1.1'
#' @param ... passed to GET/PUT/DELETE/PATCH/POST call.
#'
#' @return returned request list of httr
#'
#' @references
#' \url{https://docs.sbgenomics.com/display/developerhub/API}
#'
#' @export api
#' @examples
#' token <- "fake_token"
#' \donttest{
#' ## list projects
#' api(token = token, path = 'project', method = "GET")
#' }
api = function (token = NULL, version = '1.1', path = NULL,
    method = c('GET', 'POST', 'PUT', 'DELETE', 'PATCH'),
    query = NULL, body = list(), encode = "json", 
    limit = getOption("sevenbridges")$limit,
    offset = getOption("sevenbridges")$offset, 
    base_url = paste0("https://api.sbgenomics.com/", version, "/"), ...) {

    if (is.null(token))
        stop('token must be provided')

    method <- match.arg(method)

    headers = c(
        'X-SBG-Auth-Token' = token
        ## 'Accept' = 'application/json',
        ## 'Content-type' = 'application/json'
    )

    ## setup query
    query <- c(query, list(limit = limit, offset = offset))
    idx <- !sapply(query, is.null)
    if(any(idx)){
        query <- query[idx]
    }else{
        query <- NULL
    }


    switch(method,
           GET = {
               GET2(paste0(base_url, path),
                   add_headers(.headers = headers), query = query, ...)
           },
           POST = {
               ## stopifnot(is.list(body))
               ## body_json = toJSON(body, auto_unbox = TRUE)
               POST2(paste0(base_url, path),
                    add_headers(.headers = headers), query = query,
                    body = body, encode = encode,  ...)
           },
           PUT = {
               ## stopifnot(is.list(body))
               ## body_json = toJSON(body, auto_unbox = TRUE)
               PUT(paste0(base_url, path),
                   add_headers(.headers = headers),
                   body = body, encode = encode, ...)
           },
           DELETE = {
               DELETE(paste0(base_url, path),
                      add_headers(.headers = headers), ...)
           },
           PATCH = {
               ## stopifnot(is.list(body))
               ## body_json = toJSON(body, auto_unbox = TRUE)
               PATCH(paste0(base_url, path),
                     add_headers(.headers = headers),
                     body = body,
                     encode = encode, ...)
           })
    
    
}


#' check request status
#'
#' check request status
#'
#' @return request content or the message
#'
#' @keywords internal
status_check = function (req, as = "parsed", ...) {
    if (status_code(req) %in% c('200', '201', '202', '204')) {
        res <- content(req, as = as, ...)
        if(!is.null(res))
            attr(res, "response") <- req
        return(res)
    } else if (status_code(req) %in% c('401', '403', '404', '503')) {

        msg = content(req, as = as, ...)$message       
        stop(paste0('HTTP Status ', status_code(req), ': ', msg), call. = FALSE)            
        

    } else {

        if("message" %in% names(content(req, as = as, ...))){
            msg = content(req, as = as, ...)$message
        }else{
            msg = NULL
        }

        if(is.null(msg)){
            if(status_code(req) %in% names(.codes)){
                msg <- .codes[[status_code(req)]]                
            }
            if(is.null(msg)){
                print(content(req, as = as, ...))
                stop(paste('Error of unknown type occured', status_code(req)))
            }else{
                stop(paste0('HTTP Status ', status_code(req), ': ', msg), call. = FALSE)       
            }
        }else{
            stop(paste0('HTTP Status ', status_code(req), ': ', msg), call. = FALSE)           
        }
        
    }

}



## well this code is from specification v2
.codes = list(
    ## 0xxx: Platform maintenance errors
    "0" = "Seven Bridges Genomics platform is currently under maintenance.",

    ## 1xxx: General errors
    "1000"= "Allowed Rate limit exceeded! Check response headers.",

    ## 2xxx: Users
    "2000" = "User service is currently unavailable", 
    "2001" = "Not enough privileges to access requested user info.", 
    "2002" = "Requested user does not exist.", 
    "2003" = "Requested user already exists.", 

    ## "3xxx" = "Projects
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

    ## 4xxx" = "Billing"
    "4000" = "Billing service is currently unavailable", 
    "4001" = "Insufficient privileges to access the requested billing group/invoice", 
    "4002" = "Requested billing group/invoice does not exist", 
    "4003" = "Requested billing group/invoice already exist", 
    "4004" = "Billing group id must not be null or an empty string", 
    "4005" = "Billing group id must be a valid UUID", 
    "4006" = "You are not a member of this billing group", 
    "4007" = "Invoice id must not be null or an empty string", 

    ## "5xxx" = "Files
    "5000" = "File service is currently unavailable", 
    "5001" = "Insufficient privileges to access the requested file", 
    "5002" = "Requested file does not exist",
    "5004" = "Requested file already exists", 
    "5005" = "Malformed project query parameter. Expecting ?project=owner/project", 
    "5006" = "Metadata validation failed" ,
    "5007" = "File copy failed", 
    "5008" = "File renaming not allowed", 

    ## "6xxx" = "Apps"
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

    ## "7xxx" = "Tasks
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

    ## "9xxx" = "General validation errors
    "9000" = "Bad request.", 
    "9001" = "Unauthorized.", 
    "9002" = "Forbidden.", 
    "9003" = "Not Found.", 
    "9004" = "Unexpected error happened.", 
    "9005" = "Service Unavailable.", 
    "9006" = "Method Not Allowed.", 
    "9007" = "Conflict.", 
    "9008" = "Unsupported Media Type.", 
    "9009" = "An Error occurred during the decoding of the request content.")




# 1. Upload files

#' Returns upload information for the ongoing upload
#'
#' Returns the upload information for the ongoing upload.
#'
#' @param token auth token
#' @param upload_id ID of the upload
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export upload_info
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = upload_info(token,
#'                 upload_id = '8D7sQJxQk14ubsEnKaoeQZlRvV6ouQtMzBWaQNJdxPDLypUC3WogwtJdncevHxnT')}
upload_info = function (token = NULL, upload_id = NULL, ...) {

    if (is.null(upload_id)) stop('upload_id must be provided')

    req = api(token = token,
                 path = paste0('upload/multipart/', upload_id),
                 method = 'GET', ...)

    return(status_check(req))

}

#' Returns AWS S3 signed URL for a part of the file upload
#'
#' Gets the signed URL for the upload of the specified part.
#' Note that URLs are valid for 60 seconds only and that you should initiate
#' upload to the signed URL in this time frame.
#'
#' @param token auth token
#' @param upload_id ID of the upload
#' @param part_number Number of the upload file part that you wish to access
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export upload_info_part
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = upload_info_part(token,
#'                 upload_id = 'aVluXRqSX2bse6va3AFFgVAppOCQ9IABeA8HnyyiEw85j6pNyV989H4xvJpr53xa',
#'                 part_number = 1)}
upload_info_part = function (token = NULL,
                             upload_id = NULL, part_number = NULL, ...) {

    if (is.null(upload_id) || is.null(part_number))
        stop('upload_id and part_number must be both provided')

    req = api(token = token,
                 path = paste0('upload/multipart/', upload_id, '/', part_number),
                 method = 'GET', ...)

    return(status_check(req))

}

#' Initializes the upload of the specified file
#'
#' This is the first operation performed when you wish to upload a file.
#' Operation is initialized by providing file name, project id where you
#' wish the file to be uploaded to (if not specified, defaults to user's stash)
#' and optionally by providing wanted part size. You may wish to set your
#' part size to a low value if you experience problems with uploading large
#' file parts, although default value of 5MB should be good enough for
#' most users.
#'
#' Limits: \itemize{
#' \item Maximum number of parts is 10000
#' \item Maximum file size is 5TB
#' \item Maximum part size is 5GB
#' \item Default part size is 5MB}
#'
#' @param token auth token
#' @param project_id ID of the project you wish to upload to
#' @param name Name of the file you wish to upload
#' @param size Size of the file you wish to upload
#' @param part_size Requested part size. Note that API may reject your
#' requested part size and return proper one in response.
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export upload_init
#' @examples
#' token = '58aeb140-1970-0130-6386-001f5b34aa78'
#' \donttest{req = upload_init(token,
#'                 project_id = 'f0eb447f-3511-4b28-9253-eba96191d432',
#'                 name = 'Sample1_RNASeq_chr20.pe_1.fastq', size = 5242880)}
upload_init = function (token = NULL, project_id = NULL,
                        name = NULL, size = NULL, part_size = NULL, ...) {

    if (is.null(project_id) || is.null(name))
        stop('project_id and name must be both provided')

    body = list('project_id' = project_id, 'name' = name)

    if (!is.null(size)) body$'size' = size
    if (!is.null(part_size)) body$'part_size' = part_size

    req = api(token = token,
                 path = 'upload/multipart', body = body, method = 'POST', ...)

    return(status_check(req))

}

#' Reports the completion of the part upload
#'
#' The ETag is provided for the correctness check upon completion of the
#' whole upload. Value for the ETag is provided by AWS S3 service when
#' uploading the file in the ETag header.
#'
#' @param token auth token
#' @param upload_id ID of the upload
#' @param part_number ID of the part you wish to report as completed
#' @param e_tag Value of the ETag header returned by AWS S3 when uploading
#' part of the file.
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export upload_complete_part
#' @examples
#' token = '58aeb140-1970-0130-6386-001f5b34aa78'
#' \donttest{req = upload_complete_part(token,
#'                 upload_id = '8D7sQJxQk14ubsEnKaoeQZlRvV6ouQtMzBWaQNJdxPDLypUC3WogwtJdncevHxnT',
#'                 part_number = '1',
#'                 e_tag = 'd41d8cd98f00b204e9800998ecf8427e')}
upload_complete_part = function (token = NULL, upload_id = NULL,
                                 part_number = NULL, e_tag = NULL, ...) {

    if (is.null(upload_id) || is.null(part_number) || is.null(e_tag))
        stop('upload_id, part_number and e_tag must be provided')

    body = list('part_number' = as.character(part_number),
                'e_tag' = as.character(e_tag))

    req = api(token = token,
                 path = paste0('upload/multipart/', upload_id),
                 body = body, method = 'POST', ...)

    return(status_check(req))

}

#' Reports the complete file upload
#'
#' If the whole parts are uploaded, and the provided ETags are correct,
#' then the file is assembled and made available on the SBG platform.
#'
#' @param token auth token
#' @param upload_id ID of the upload
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export upload_complete_all
#' @examples
#' token = '58aeb140-1970-0130-6386-001f5b34aa78'
#' \donttest{req = upload_complete_all(token,
#'             upload_id = '8D7sQJxQk14ubsEnKaoeQZlRvV6ouQtMzBWaQNJdxPDLypUC3WogwtJdncevHxnT')}
upload_complete_all = function (token = NULL, upload_id = NULL, ...) {

    if (is.null(upload_id)) stop('upload_id must be provided')

    req = api(token = token,
                 path = paste0('upload/multipart/', upload_id, '/complete'),
                 method = 'POST', ...)

    return(status_check(req))

}

#' Aborts the upload
#'
#' All upload records and the file are deleted.
#'
#' @param token auth token
#' @param upload_id ID of the upload
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export upload_delete
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = upload_delete(token,
#'             upload_id = '8D7sQJxQk14ubsEnKaoeQZlRvV6ouQtMzBWaQNJdxPDLypUC3WogwtJdncevHxnT')}
upload_delete = function (token = NULL, upload_id = NULL, ...) {

    if (is.null(upload_id)) stop('upload_id must be provided')

    req = api(token = token,
                 path = paste0('upload/multipart/', upload_id), method = 'DELETE')

    return(status_check(req))

}
# 2. Projects

#' Returns the details of the project
#'
#' Returns the details of the project.
#'
#' @param token auth token
#' @param project_id ID of a project you want to access.
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export project_details
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = project_details(token,
#'                 project_id = 'b0b3a611-6bb0-47e5-add7-a83402cf7858')}
project_details = function (token = NULL, project_id = NULL, ...) {

    if (is.null(project_id)) stop('project_id must be provided')

    req = api(token = token,
                 path = paste0('project/', project_id), method = 'GET', ...)

    return(status_check(req))

}

#' Returns a list of all users invited to the project and their privileges
#'
#' Returns a list of all users invited to the project and their privileges.
#' Project ID is specified as path parameter. Call returns ID and username
#' of the user with privileges.
#'
#' @param token auth token
#' @param project_id ID of a project you want to access.
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export project_members
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = project_members(token,
#'                 project_id = 'b0b3a611-6bb0-47e5-add7-a83402cf7858')}
project_members = function (token = NULL, project_id = NULL, ...) {

    if (is.null(project_id)) stop('project_id must be provided')

    req = api(token = token,
                 path = paste0('project/', project_id, '/members'),
                 method = 'GET', ...)

    return(status_check(req))

}

