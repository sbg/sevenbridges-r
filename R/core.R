#' wrapper of http logic for SBG API
#'
#' wrapper of http logic for SBG API
#'
#' Used for advanced users and the core method for higher level API in
#' this package, please refer to the easy api manual and the two
#' vignettes pages for more convenient usage.
#'
#' @param auth_token authenticate token string.
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
#' api(auth_token = token, path = 'project', method = "GET")
#' }
api = function (auth_token = NULL, version = '1.1', path = NULL,
    method = c('GET', 'POST', 'PUT', 'DELETE', 'PATCH'),
    query = NULL, body = list(), encode = "json", 
    limit = getOption("limit"), offset = getOption("offset"), 
    base_url = paste0("https://api.sbgenomics.com/", version, "/"), ...) {

    if (is.null(auth_token))
        stop('auth_token must be provided')

    method <- match.arg(method)

    headers = c(
        'X-SBG-Auth-Token' = auth_token
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
               stopifnot(is.list(body))
               ## body_json = toJSON(body, auto_unbox = TRUE)
               POST2(paste0(base_url, path),
                    add_headers(.headers = headers), query = query,
                    body = body, encode = encode,  ...)
           },
           PUT = {
               stopifnot(is.list(body))
               ## body_json = toJSON(body, auto_unbox = TRUE)
               PUT(paste0(base_url, path),
                   add_headers(.headers = headers),
                   body = body, encode = encdoe, ...)
           },
           DELETE = {
               DELETE(paste0(base_url, path),
                      add_headers(.headers = headers), ...)
           },
           PATCH = {
               stopifnot(is.list(body))
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
    if (status_code(req) %in% c('200', '201', '204')) {
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
#' @param auth_token auth token
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
upload_info = function (auth_token = NULL, upload_id = NULL, ...) {

    if (is.null(upload_id)) stop('upload_id must be provided')

    req = api(auth_token = auth_token,
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
#' @param auth_token auth token
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
upload_info_part = function (auth_token = NULL,
                             upload_id = NULL, part_number = NULL, ...) {

    if (is.null(upload_id) || is.null(part_number))
        stop('upload_id and part_number must be both provided')

    req = api(auth_token = auth_token,
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
#' @param auth_token auth token
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
upload_init = function (auth_token = NULL, project_id = NULL,
                        name = NULL, size = NULL, part_size = NULL, ...) {

    if (is.null(project_id) || is.null(name))
        stop('project_id and name must be both provided')

    body = list('project_id' = project_id, 'name' = name)

    if (!is.null(size)) body$'size' = size
    if (!is.null(part_size)) body$'part_size' = part_size

    req = api(auth_token = auth_token,
                 path = 'upload/multipart', body = body, method = 'POST', ...)

    return(status_check(req))

}

#' Reports the completion of the part upload
#'
#' The ETag is provided for the correctness check upon completion of the
#' whole upload. Value for the ETag is provided by AWS S3 service when
#' uploading the file in the ETag header.
#'
#' @param auth_token auth token
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
upload_complete_part = function (auth_token = NULL, upload_id = NULL,
                                 part_number = NULL, e_tag = NULL, ...) {

    if (is.null(upload_id) || is.null(part_number) || is.null(e_tag))
        stop('upload_id, part_number and e_tag must be provided')

    body = list('part_number' = as.character(part_number),
                'e_tag' = as.character(e_tag))

    req = api(auth_token = auth_token,
                 path = paste0('upload/multipart/', upload_id),
                 body = body, method = 'POST', ...)

    return(status_check(req))

}

#' Reports the complete file upload
#'
#' If the whole parts are uploaded, and the provided ETags are correct,
#' then the file is assembled and made available on the SBG platform.
#'
#' @param auth_token auth token
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
upload_complete_all = function (auth_token = NULL, upload_id = NULL, ...) {

    if (is.null(upload_id)) stop('upload_id must be provided')

    req = api(auth_token = auth_token,
                 path = paste0('upload/multipart/', upload_id, '/complete'),
                 method = 'POST', ...)

    return(status_check(req))

}

#' Aborts the upload
#'
#' All upload records and the file are deleted.
#'
#' @param auth_token auth token
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
upload_delete = function (auth_token = NULL, upload_id = NULL, ...) {

    if (is.null(upload_id)) stop('upload_id must be provided')

    req = api(auth_token = auth_token,
                 path = paste0('upload/multipart/', upload_id), method = 'DELETE')

    return(status_check(req))

}
# 2. Projects

#' Returns the details of the project
#'
#' Returns the details of the project.
#'
#' @param auth_token auth token
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
project_details = function (auth_token = NULL, project_id = NULL, ...) {

    if (is.null(project_id)) stop('project_id must be provided')

    req = api(auth_token = auth_token,
                 path = paste0('project/', project_id), method = 'GET', ...)

    return(status_check(req))

}

#' Returns a list of all users invited to the project and their privileges
#'
#' Returns a list of all users invited to the project and their privileges.
#' Project ID is specified as path parameter. Call returns ID and username
#' of the user with privileges.
#'
#' @param auth_token auth token
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
project_members = function (auth_token = NULL, project_id = NULL, ...) {

    if (is.null(project_id)) stop('project_id must be provided')

    req = api(auth_token = auth_token,
                 path = paste0('project/', project_id, '/members'),
                 method = 'GET', ...)

    return(status_check(req))

}

#' Create new project
#'
#' You can use this call to create a project. All details, including
#' project name, description and funding source are specified as part
#' of the JSON, sent as the body of the request. This call returns
#' details of the project.
#'
#' @param auth_token auth token
#' @param name Name of the project you wish to create.
#' @param description Description of the project you wish to create.
#' @param billing_group_id ID of the billing group you wish to use
#' for this project.
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export project_new
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = project_new(token, name = 'Test API project',
#'                 description = 'My first API project',
#'                 billing_group_id = '5b6d5e71-dff8-42fc-8583-500d858f1093')}
project_new = function (auth_token = NULL, name = NULL,
                        description = NULL, billing_group_id = NULL, ...) {

    if (is.null(name) || is.null(description) || is.null(billing_group_id))
        stop('name, description, and billing_group_id must be provided')

    body = list('name' = name,
                'description' = description,
                'billing_group_id' = billing_group_id)

    req = api(auth_token = auth_token,
                 path = 'project', body = body,
                 method = 'POST', ...)

    return(status_check(req))

}

#' Add a user to the project with appropriate permissions
#'
#' You can use this call to add specific users to a project and set their
#' privileges. Note that you need to specify user's SBG platform username
#' when adding to the project.
#'
#' @param auth_token auth token
#' @param project_id Name of the project you wish to add user to.
#' @param username SBG platform username for a user you wish to add to
#' the project.
#' @param copy Logical. Ability to download or copy files.
#' @param write Logical. Ability to create/edit/delete project objects.
#' @param execute Logical. Ability to run tasks.
#' @param admin Logical. User has all rights on the project
#' (including changing).
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export project_member_add
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = project_member_add(token,
#'                 project_id = '88fc89c1-cfcd-46ed-a830-6a2fc110c628',
#'                 username = 'testuser', write = TRUE)}
project_member_add = function (auth_token = NULL, project_id = NULL,
                               username = NULL, copy = FALSE, write = FALSE,
                               execute = FALSE, admin = FALSE, ...) {

    if (is.null(project_id) || is.null(username))
        stop('project_id and username must be both provided')

    body = list('username' = username,
                'permissions' = list(
                    'copy' = copy, 'write' = write,
                    'execute' = execute, 'admin' = admin))

    req = api(auth_token = auth_token,
                 path = paste0('project/', project_id, '/members'),
                 body = body, method = 'POST', ...)

    return(status_check(req))

}

#' Set permissions for a user to a project
#'
#' This call will set project's member privileges.
#' Privileges you do not explicitly set to "true" will be automatically
#' set to "false". Project ID and user ID are specified in path parameters.
#' Note that you must get the user IDs by performing the project_members()
#' call and gathering id of the user with a specific permission.
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param user_id ID of a user whose permissions you with to set
#' @param write Logical. Ability to create/edit/delete project objects.
#' @param copy Logical. Ability to download or copy files.
#' @param execute Logical. Ability to run tasks.
#' @param admin Logical. User has all rights on the project
#' (including changing).
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export project_member_update
#' @examples
#' token = '58aeb140-1970-0130-6386-001f5b34aa78'
#' \donttest{req = project_member_update(token,
#'             project_id = '7f7a72d0-da77-4f51-9416-99f14f7316ab',
#'             user_id = '08890148-6d9e-4a10-b284-924228d3f99a')}
project_member_update = function (auth_token = NULL,
                                  project_id = NULL, user_id = NULL,
                                  write = FALSE, copy = FALSE,
                                  execute = FALSE, admin = FALSE, ...) {

    if (is.null(project_id) || is.null(user_id))
        stop('project_id and user_id must be both provided')

    body = list('write' = write,
                'copy' = copy,
                'execute' = execute,
                'admin' = admin)

    req = api(auth_token = auth_token,
                 path = paste0('project/', project_id, '/members/', user_id),
                 body = body, method = 'PUT', ...)

    return(status_check(req))

}

#' Delete a project
#'
#' Note that this deletes all files, tasks which belong to a project.
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to delete.
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export project_delete
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = project_delete(token,
#'             project_id = '3a21ade8-ef3e-41f8-8ac2-1dc3b434ac77')}
project_delete = function (auth_token = NULL, project_id = NULL, ...) {

    if (is.null(project_id)) stop('project_id must be provided')

    req = api(auth_token = auth_token,
                 path = paste0('project/', project_id), method = 'DELETE', ...)

    return(status_check(req))

}

#' Removes a member from a project
#'
#' Note that user_id parameter is not username, but user ID parameter
#' that you can receive from GET members call.
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param user_id ID of the user you wish to remove.
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export project_member_delete
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = project_member_delete(token,
#'             project_id = '7f7a72d0-da77-4f51-9416-99f14f7316ab',
#'             user_id = '08890148-6d9e-4a10-b284-924228d3f99a')}
project_member_delete = function (auth_token = NULL,
                                  project_id = NULL, user_id = NULL, ...) {

    if (is.null(project_id) || is.null(user_id))
        stop('project_id and user_id must be both provided')

    req = api(auth_token = auth_token,
                 path = paste0('project/', project_id, '/members/', user_id),
                 method = 'DELETE', ...)

    return(status_check(req))

}

# # 4. Files

#' Returns the list of all project files for a project
#'
#' Returns the list of all project files for a project. If user specifies
#' string \code{"public"} as \code{project_id}, this will return a list
#' of public files.
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' Note that specifying \code{"public"} you can list public files.
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export file_list
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = file_list(token,
#'             project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e')}
file_list = function (auth_token = NULL, project_id = NULL, ...) {

    if (is.null(project_id)) stop('project_id must be provided')

    req = api(auth_token = auth_token,
                 path = paste0('project/', project_id, '/file'), method = 'GET', ...)

    return(status_check(req))

}

#' Returns detailed information about a project's files
#'
#' Returns detailed information about a project's files
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param file_id ID of a file you want to access.
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export file_details
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = file_details(token,
#'             project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e',
#'             file_id = '530854e2e4b036506b803c7e')}
file_details = function (auth_token = NULL, project_id = NULL,
    file_id = NULL, ...) {

    if (is.null(project_id) || is.null(file_id))
        stop('project_id and file_id must be both provided')

    req = api(auth_token = auth_token,
                 path = paste0('project/', project_id, '/file/', file_id),
                 method = 'GET', ...)

    return(status_check(req))

}

#' Copy specified file(s) to the specified project
#'
#' Copy specified file(s) to the specified project
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to copy files to.
#' @param file_id Character vector. IDs of the files you wish to copy to
#' the project.
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export file_copy
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = file_copy(token,
#'             project_id = '7f7a72d0-da77-4f51-9416-99f14f7316ab',
#'             file_id = c('5506a44ae4b04a4ab3ae7250',
#'                         '5506a44ae4b04a4ab3ae7254',
#'                         '5506a44ae4b04a4ab3ae7252'))}
file_copy = function (auth_token = NULL, project_id = NULL,
    file_id = NULL, ...) {

    if (is.null(project_id) || is.null(file_id))
        stop('project_id and file_id must be both provided')

    body = list('file_id' = as.character(file_id))

    req = api(auth_token = auth_token,
                 path = paste0('project/', project_id, '/file'),
                 body = body, method = 'POST', ...)

    return(status_check(req))

}

#' Update project's file metadata
#'
#' This function updates project's file metadata. You can also use this call
#' to change filenames if you supply the \code{name} argument.
#'
#' For more information about file metadata, please check the
#' File Metadata Documentation:
#' \url{https://developer.sbgenomics.com/platform/metadata}.
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param file_id ID of a file you want to access.
#' @param name File name.
#' @param file_type File type. This metadata parameter is mandatory
#' for each file.
#' @param qual_scale Quality scale encoding. For FASTQ files, you must
#' either specify the quality score encoding sch which contains the
#' FASTQ quality scale detector wrapper. In that case, you can
#' specify the quality score encoding scheme by setting
#' \code{qual_scale} inside the pipeline. For BAM files, this value
#' should always be \code{'sanger'}.
#' @param seq_tech Sequencing technology. The \code{seq_tech} parameter
#' allows you to specify the sequencing technology used. This metadata
#' parameter is only required by some the tools and pipelines;
#' however, it is strongly recommended that you set it whenever possible,
#' unless you are certain that your pipeline will work without it.
#' @param sample Sample ID. You can use the \code{sample} parameter to specify
#' the sample identifier. The value supplied in this field will be written
#' to the read group tag (\code{@@RG:SM}) in SAM/BAM files generated from reads
#' with the specified Sample ID. AddOrReplaceReadGroups will use this
#' parameter as the value for the read group tag in a SAM/BAM file.
#' @param library Library. You can set the library for the read using the
#' \code{library} parameter. The value supplied in this field will be written
#' to the read group tag (\code{@@RG:LB}) in SAM/BAM files generated from
#' reads with the specified Library ID. AddOrReplaceReadGroups will use
#' this parameter as the value for the read group tag in a SAM/BAM file.
#' @param platform_unit Platform unit. You can set the platform unit
#' (e.g. lane for Illumina, or slide for SOLiD) using the \code{platform_unit}
#' parameter. The value supplied in this field will be written to the read
#' group tag (\code{@@RG:PU}) in SAM/BAM files generated from the reads with
#' the specified Platform Unit. AddOrReplaceReadGroups will use this parameter
#' as the value for the read group tag of a SAM/BAM file.
#' @param paired_end Paired end. With paired-end reads, this parameter
#' indicates if the read file is left end (1) or right end (2).
#' For SOLiD CSFASTA files, paired end files 1 and 2 correspond to R3
#' and F3 files, respectively.
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export file_meta_update
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = file_meta_update(token,
#'             project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e',
#'             file_id = '530854e2e4b036506b803c7e',
#'             name = 'c.elegans_chr2_test.fastq',
#'             file_type = 'fastq', qual_scale = 'illumina13',
#'             seq_tech = 'Illumina')}
file_meta_update = function (auth_token = NULL,
                             project_id = NULL, file_id = NULL,
                             name = NULL,
                             file_type = c('text', 'binary', 'fasta', 'csfasta',
                                           'fastq', 'qual', 'xsq', 'sff', 'bam',
                                           'bam_index', 'illumina_export',
                                           'vcf', 'sam', 'bed', 'archive',
                                           'juncs', 'gtf','gff',
                                           'enlis_genome'),
                             qual_scale = c('sanger', 'illumina13',
                                            'illumina15', 'illumina18',
                                            'solexa'),
                             seq_tech = c('454', 'Helicos', 'Illumina', 'Solid',
                                          'IonTorrent'),
                             sample = NULL, library = NULL,
                             platform_unit = NULL, paired_end = NULL, ...) {

    if (is.null(project_id) || is.null(file_id))
        stop('project_id and file_id must be both provided')

    body = list(list('file_type' = file_type,
                     'qual_scale' = qual_scale,
                     'seq_tech' = seq_tech))
    names(body) = 'metadata'

    if (!is.null(sample)) body$'metadata'$'sample' = as.character(sample)
    if (!is.null(library)) body$'metadata'$'library' = as.character(library)
    if (!is.null(platform_unit)) body$'metadata'$'platform_unit' = as.character(platform_unit)
    if (!is.null(paired_end)) body$'metadata'$'paired_end' = as.character(paired_end)

    if (!is.null(name)) body = c(list('name' = name), body)

    req = api(auth_token = auth_token,
                 path = paste0('project/', project_id, '/file/', file_id),
                 body = body, method = 'PUT', ...)

    return(status_check(req))

}

#' Removes a file from a project
#'
#' Removes a file from a project
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param file_id ID of a file you want to delete.
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export file_delete
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = file_delete(token,
#'             project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e',
#'             file_id = '530854e2e4b036506b803c7e')}
file_delete = function (auth_token = NULL,
                        project_id = NULL, file_id = NULL, ...) {

    if (is.null(project_id) || is.null(file_id))
        stop('project_id and file_id must be both provided')

    req = api(auth_token = auth_token,
                 path = paste0('project/', project_id, '/file/', file_id),
                 method = 'DELETE', ...)

    return(status_check(req))

}

#' Returns a direct download URL for a project's file
#'
#' Returns a direct download URL for a project's file.
#'
#' You can use any HTTP client, or library to access or download
#' the content once you get the URL.
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param file_id ID of a file you want to access.
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export file_download_url
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = file_download_url(token,
#'             project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e',
#'             file_id = '530854e2e4b036506b803c7e')}
file_download_url = function (auth_token = NULL,
                              project_id = NULL, file_id = NULL, ...) {

    if (is.null(project_id) || is.null(file_id))
        stop('project_id and file_id must be both provided')

    req = api(auth_token = auth_token,
                 path = paste0('project/', project_id,
                               '/file/', file_id, '/download'),
                 method = 'GET', ...)

    return(status_check(req))

}
# 5. Pipelines

#' Returns the list of all public pipelines
#'
#' Returns the list of all public pipelines.
#'
#' @param auth_token auth token
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export pipeline_list_pub
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = pipeline_list_pub(token)}
pipeline_list_pub = function (auth_token = NULL, ...) {

    req = api(auth_token = auth_token,
                 path = 'pipeline/public', method = 'GET', ...)

    return(status_check(req))

}

#' Returns the list of pipelines in user's "My Pipelines" section
#'
#' Returns the list of pipelines in user's "My Pipelines" section.
#'
#' @param auth_token auth token
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export pipeline_list_my
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = pipeline_list_my(token)}
pipeline_list_my = function (auth_token = NULL, ...) {

    req = api(auth_token = auth_token, path = 'pipeline/my', method = 'GET', ...)

    return(status_check(req))

}

#' Returns a list of all the pipelines in project
#'
#' Returns a list of all the pipelines in project.
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export pipeline_list_project
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = pipeline_list_project(token,
#'             project_id = 'b0b3a611-6bb0-47e5-add7-a83402cf7858')}
pipeline_list_project = function (auth_token = NULL, project_id = NULL, ...) {

    if (is.null(project_id)) stop('project_id must be provided')

    req = api(auth_token = auth_token,
                 path = paste0('project/', project_id, '/pipeline'),
                 method = 'GET', ...)

    return(status_check(req))

}

#' Returns the details of a pipeline for a project
#'
#' Returns the details of a pipeline (runtime and regular parameters,
#' description etc.) for a project.
#'
#' When using the API to run a task, the user needs to set input files
#' for all input nodes. To facilitate this, some pipeline input nodes
#' may contain field "suggested files", that contains files which may
#' be used as default input (reference genomes, SNP database, etc.).
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param pipeline_id ID of a pipeline you want to access.
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export pipeline_details
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = pipeline_details(token,
#'             project_id = 'b0b3a611-6bb0-47e5-add7-a83402cf7858',
#'             pipeline_id = '55606ad4896a5d524656afd0')}
pipeline_details = function (auth_token = NULL,
                             project_id = NULL, pipeline_id = NULL, ...) {

    if (is.null(project_id) || is.null(pipeline_id))
        stop('project_id and pipeline_id must be both provided')

    req = api(auth_token = auth_token,
                 path = paste0('project/', project_id,
                               '/pipeline/', pipeline_id),
                 method = 'GET', ...)

    return(status_check(req))

}

#' Add a pipeline to a specified project
#'
#' Add a pipeline to a specified project. You can use this function to add
#' a pipeline from your other project or a public pipeline to a project.
#'
#' @param auth_token auth token
#' @param project_id_to ID of a project you to copy pipeline into.
#' @param project_id_from ID of the project you wish to add from.
#' Specify values such as \code{"my"} to specify a pipeline from
#' "My Pipelines" section or omit for a public pipeline, respectively.
#' @param pipeline_id ID of the pipeline you wish to add to project.
#' @param revision Revision of the pipeline you wish to add to the project.
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export pipeline_add
#' @examples
#' token = '58aeb140-1970-0130-6386-001f5b34aa78'
#' \donttest{req = pipeline_add(token,
#'             project_id_to = '7f7a72d0-da77-4f51-9416-99f14f7316ab',
#'             project_id_from = 'f0eb447f-3511-4b28-9253-eba96191d432',
#'             pipeline_id = '53452130d79f0049c0c94441')}
pipeline_add = function (auth_token = NULL, project_id_to = NULL,
                         project_id_from = NULL, pipeline_id = NULL,
                         revision = NULL, ...) {

    if (is.null(project_id_to) || is.null(pipeline_id))
        stop('project_id_to and pipeline_id must be provided')

    if (is.null(project_id_from)) {
        body = list('pipeline_id' = pipeline_id)
    } else {
        body = list('project_id' = project_id_from,
                    'pipeline_id' = pipeline_id)
    }

    if (!is.null(revision)) body = c(body, 'revision' = as.character(revision))

    req = api(auth_token = auth_token,
                 path = paste0('project/', project_id_to, '/pipeline'),
                 body = body, method = 'POST', ...)

    return(status_check(req))

}
# 6. Tasks

#' Returns the list of all the tasks for a project
#'
#' Returns the list of all the tasks for a project.
#'
#' This function returns general information and status of a task,
#' in case you want to get a details, including the inputs, outputs
#' and parameters set for that task, you will have to use task details
#' resource referencing the \code{task_id} of a task that you want to
#' get information about.
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export task_list
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = task_list(token,
#'                 '1c1d06d2-5862-48f6-b595-e0099b20937e')}
task_list = function (auth_token = NULL, project_id = NULL, ...) {

    if (is.null(project_id)) stop('project_id must be both provided')

    req = api(auth_token = auth_token,
        path = paste0('project/', project_id, '/task'),
        method = 'GET', ...)

    return(status_check(req))

}

#' Runs a task as a part of a project
#'
#' Runs a task as a part of a project.
#'
#' All the details, including the pipeline ID and runtime parameters,
#' are specified via a list. See the example for details.
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param task_details A list with the following components:
#' \itemize{
#' \item \code{pipeline_id} - ID of the pipeline you wish to execute
#' \item \code{pipeline_revision} - Revision of the pipeline you wish
#' to execute. If not specified, latest revision is used.
#' \item \code{name} - Name of the task you wish to execute
#' \item \code{description} - Description of the task you wish to execute
#' \item \code{inputs} - Named list containing mappings of pipeline input
#' node ID to file IDs. Note that file IDs always need to be specified as
#' an list, even if empty or with one element.
#' \item \code{parameters} - Named list containing mappings of node IDs
#' to apps specific parameters. Note that parameters are always specified
#' as an list, even if empty or with one element.}
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export task_run
#' @examples
#' token = '58aeb140-1970-0130-6386-001f5b34aa78'
#' details = list(
#'   'name' = 'Test 2 of C. Elegans VC',
#'   'description' = 'Testing Caenorhabditis elegans Exome Variant Calling',
#'   'pipeline_id' = '422',
#'   'inputs' = list('309485' = 13645,
#'                   '317344' = 13646,
#'                   '318662' = 13645,
#'                   '699018' = 13647),
#'   'parameters' = list('393463' = list('read_trimming_qual' = 30,
#'                                       'rg_seq_tech' = 'Illumina'),
#'                       '677492' = list()))
#'
#' \donttest{req = task_run(token,
#'                 project_id = '7f7a72d0-da77-4f51-9416-99f14f7316ab',
#'                 task_details = details)}
task_run = function (auth_token = NULL,
                     project_id = NULL, task_details = NULL, ...) {

    if (is.null(project_id) || is.null(task_details))
        stop('project_id and task_details must be both provided')

    body = task_details

    req = api(auth_token = auth_token,
                 path = paste0('project/', project_id, '/task'),
                 body = body, method = 'POST', ...)

    return(status_check(req))

}

#' Returns information about the task
#'
#' Returns information about the task.
#'
#' Each task has a status and status message, containing the more detailed
#' information about the task status, associated with it. This is a list of
#' all values that task status can have:
#' \itemize{
#' \item \code{active} - Task is currently running.
#' \item \code{completed} - Task has finished successfully.
#' \item \code{aborted} - Task was aborted by user.
#' \item \code{failed} - Task has failed to finish due to
#' either bad inputs and/or parameters, or because of the
#' internal infrastructure failures.}
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param task_id ID of a task you want to access.
#' @param download.url Logical. Return the download URL or not.
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export task_details
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req1 = task_details(token,
#'                  project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e',
#'                  task_id = '22237')
#' req2 = task_details(token,
#'        project_id = '1c1d06d2-5862-48f6-b595-e0099b20937e',
#'        task_id = '22237', download.url = TRUE)}
task_details = function (auth_token = NULL,
                         project_id = NULL, task_id = NULL,
                         download.url = FALSE, ...) {

    if (is.null(project_id) || is.null(task_id))
        stop('project_id and task_id must be both provided')

    if (download.url == FALSE) {
        req = api(auth_token = auth_token,
                     path = paste0('project/', project_id, '/task/', task_id),
                     method = 'GET', ...)
    } else {
        req = api(auth_token = auth_token,
                     path = paste0('project/', project_id, '/task/', task_id),
                     query = list('action' = 'download'), method = 'GET', ...)
    }

    return(status_check(req))

}

#' Performs action on the task
#'
#' Performs action on the task.
#'
#' @param auth_token auth token
#' @param project_id ID of a project you want to access.
#' @param task_id ID of a task you want to access.
#' @param action Character string specifying the action.
#' Currently, only supported action is \code{'abort'}.
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export task_action
#' @examples
#' token = '420b4672ebfc43bab48dc0d18a32fb6f'
#' \donttest{req = task_action(token,
#'                 project_id = '7f7a72d0-da77-4f51-9416-99f14f7316ab',
#'                 task_id = '5506a44ae4b04a4ab3ae7250',
#'                 action = 'abort')}
task_action = function (auth_token = NULL, project_id = NULL,
                        task_id = NULL, action = 'abort', ...) {

    if (is.null(project_id) || is.null(task_id))
        stop('project_id and task_id must be both provided')

    req = api(auth_token = auth_token,
                 path = paste0('project/', project_id, '/task/', task_id),
                 query = list('action' = action), method = 'POST', ...)

    return(status_check(req))

}







