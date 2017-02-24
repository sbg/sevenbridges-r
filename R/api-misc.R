# 1. File Upload

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
#' token = "your_token"
#' \donttest{req = upload_info(token,
#'                 upload_id = "8D7sQJxQk14ubsEnKaoeQZlRvV6ouQtMzBWaQNJdxPDLypUC3WogwtJdncevHxnT")}
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
#' token = "your_token"
#' \donttest{
#' req = upload_info_part(token,
#'       upload_id = "aVluXRqSX2bse6va3AFFgVAppOCQ9IABeA8HnyyiEw85j6pNyV989H4xvJpr53xa",
#'       part_number = 1)}
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
#' token = "your_token"
#' \donttest{
#' req = upload_init(token,
#'       project_id = "f0eb447f-3511-4b28-9253-eba96191d432",
#'       name = "Sample1_RNASeq_chr20.pe_1.fastq", size = 5242880)}
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
#' token = "your_token"
#' \donttest{
#' req = upload_complete_part(token,
#'       upload_id = "8D7sQJxQk14ubsEnKaoeQZlRvV6ouQtMzBWaQNJdxPDLypUC3WogwtJdncevHxnT",
#'       part_number = "1",
#'       e_tag = "d41d8cd98f00b204e9800998ecf8427e")}
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
#' token = "your_token"
#' \donttest{
#' req = upload_complete_all(token,
#'       upload_id = "8D7sQJxQk14ubsEnKaoeQZlRvV6ouQtMzBWaQNJdxPDLypUC3WogwtJdncevHxnT")}
upload_complete_all = function (token = NULL, upload_id = NULL, ...) {

    if (is.null(upload_id)) stop('upload_id must be provided')

    req = api(token = token,
              path = paste0('upload/multipart/', upload_id, '/complete'),
              method = 'POST', ...)

    return(status_check(req))

}

#' Abort the upload
#'
#' Abort the upload; all upload records and the file are deleted.
#'
#' @param token auth token
#' @param upload_id ID of the upload
#' @param ... parameters passed to api function
#'
#' @return parsed list of the returned json
#'
#' @export upload_delete
#' @examples
#' token = "your_token"
#' \donttest{
#' req = upload_delete(token,
#'       upload_id = "8D7sQJxQk14ubsEnKaoeQZlRvV6ouQtMzBWaQNJdxPDLypUC3WogwtJdncevHxnT")}
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
#' token = "your_token"
#' \donttest{
#' req = project_details(token,
#'       project_id = "b0b3a611-6bb0-47e5-add7-a83402cf7858")}
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
#' token = "your_token"
#' \donttest{
#' req = project_members(token,
#'       project_id = "b0b3a611-6bb0-47e5-add7-a83402cf7858")}
project_members = function (token = NULL, project_id = NULL, ...) {

    if (is.null(project_id)) stop('project_id must be provided')

    req = api(token = token,
              path = paste0('project/', project_id, '/members'),
              method = 'GET', ...)

    return(status_check(req))

}

# 3. Misc

#' Opens web browser to copy the auth token
#'
#' Click the "Generate Token" or "Regenerate" button, copy and paste
#' the authentication token string to the R console.
#' The function will return the token string.
#'
#' @param platform The Seven Bridges platform to use.
#'
#' @return auth token
#'
#' @export get_token
#' @importFrom utils browseURL
#' @examples
#' token = NULL
#' # Will be prompted to enter the auth token
#' \donttest{token = get_token(platform = "cgc")}
get_token = function(platform = c("cgc", "aws-us", "aws-eu", "gcp", "cavatica")) {

    platform = match.arg(platform)

    token_url = c(
        "cgc"      = "https://cgc.sbgenomics.com/developer#token",
        "aws-us"   = "https://igor.sbgenomics.com/developer#token",
        "aws-eu"   = "https://igor.sbgenomics.com/developer#token",
        "gcp"      = "https://igor.sbgenomics.com/developer#token",
        "cavatica" = "https://cavatica.sbgenomics.com/developer#token"
    )

    browseURL(token_url[platform])
    cat("\nPlease enter the generated authentication token:")
    token = scan(what = character(), nlines = 1L, quiet = TRUE)

    return(token)

}

#' @rdname get_token
#' @export misc_get_token
misc_get_token = function() {
    .Deprecated('get_token')
}

#' @rdname Metadata
#' @export misc_make_metadata
misc_make_metadata = function() {
    .Deprecated('Metadata')
}
