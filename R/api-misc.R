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
#' @return auth token
#'
#' @export misc_get_token
#' @importFrom utils browseURL
#' @examples
#' # Paste the auth token into R
#' # console then press enter:
#' token = NULL
#' \donttest{token = misc_get_token()}
misc_get_token = function () {

    browseURL('https://igor.sbgenomics.com/account/?current=developer#developer')
    cat("\nEnter the generated authentication token:")
    token = scan(what = character(), nlines = 1L, quiet = TRUE)

    return(token)

}

#' Download SBG uploader and extract to a specified directory
#'
#' Download SBG uploader and extract to a specified directory.
#'
#' @return \code{0L} if the SBG CLI uploader is successfully
#' downloaded and unarchived.
#'
#' @param destdir The directory to extract SBG uploader to.
#' If not present, it will be created automatically.
#'
#' @export misc_get_uploader
#' @importFrom utils untar download.file
#' @examples
#' dir = "~/sbg-uploader/"
#' \donttest{misc_get_uploader(dir)}
misc_get_uploader = function (destdir = NULL) {

    if (is.null(destdir)) stop('destdir must be provided')

    tmpfile = tempfile()

    download.file(url = 'https://igor.sbgenomics.com/sbg-uploader/sbg-uploader.tgz',
                  method = 'libcurl', destfile = tmpfile)

    untar(tarfile = tmpfile, exdir = path.expand(destdir))

}

#' Specify the parameters of the file metadata and return a list,
#' JSON string, or write to a file
#'
#' Specify the parameters of the file metadata and return a list,
#' JSON string, or write to a file.
#'
#' For more information about file metadata, please check the
#' File Metadata Documentation:
#' \url{https://developer.sbgenomics.com/platform/metadata}.
#'
#' @param output Output format,
#' could be \code{'list'}, \code{'json'}, or \code{'metafile'}.
#' @param destfile Filename to write to.
#' Must be specified when \code{output = 'metafile'}.
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
#' parameter is only required by some the tools and pipelines; however,
#' it is strongly recommended that you set it whenever possible, unless
#' you are certain that your pipeline will work without it.
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
#'
#' @return list, JSON string, or a file.
#'
#' @export misc_make_metadata
#' @references
#' \url{https://developer.sbgenomics.com/platform/metadata}
#'
#' @examples
#' destfile = "~/c.elegans_chr2_test.fastq.meta"
#' \donttest{misc_make_metadata(output = "metafile",
#'             destfile = destfile,
#'             name = "c.elegans_chr2_test.fastq",
#'             file_type = "fastq", qual_scale = "illumina13",
#'             seq_tech = "Illumina")}
misc_make_metadata = function (output = c('list', 'json', 'metafile'),
                               destfile = NULL,
                               name = NULL,
                               file_type = c('text', 'binary', 'fasta',
                                             'csfasta', 'fastq', 'qual',
                                             'xsq', 'sff', 'bam', 'bam_index',
                                             'illumina_export', 'vcf', 'sam',
                                             'bed', 'archive', 'juncs',
                                             'gtf','gff', 'enlis_genome'),
                               qual_scale = c('sanger', 'illumina13',
                                              'illumina15', 'illumina18',
                                              'solexa'),
                               seq_tech = c('454', 'Helicos', 'Illumina',
                                            'Solid', 'IonTorrent'),
                               sample = NULL, library = NULL,
                               platform_unit = NULL, paired_end = NULL) {

    body = list(list('file_type' = file_type,
                     'qual_scale' = qual_scale,
                     'seq_tech' = seq_tech))
    names(body) = 'metadata'

    if (!is.null(sample)) body$'metadata'$'sample' = as.character(sample)
    if (!is.null(library)) body$'metadata'$'library' = as.character(library)
    if (!is.null(platform_unit)) body$'metadata'$'platform_unit' = as.character(platform_unit)
    if (!is.null(paired_end)) body$'metadata'$'paired_end' = as.character(paired_end)

    if (!is.null(name)) body = c(list('name' = name), body)

    if (output == 'metafile') {
        if (is.null(destfile)) stop('destfile must be provided')
        body = toJSON(body, auto_unbox = TRUE)
        writeLines(body, con = destfile)
    } else if (output == 'json') {
        body = toJSON(body, auto_unbox = TRUE)
        return(body)
    } else if (output == 'list') {
        return(body)
    }

}

#' Upload files using SBG uploader
#'
#' Upload files using SBG uploader.
#'
#' @return The uploaded file's ID number.
#'
#' @param token auth token
#' @param uploader The directory where the SBG uploader is located
#' (the directory that contains the bin/ directory).
#' @param file The location of the file to upload.
#' @param project_id The project ID to upload the files to.
#' If you do not supply this, then the uploader will place the
#' incoming files in your "My Files" section.
#' @param proxy Allows you to specify a proxy server through which
#' the uploader should connect. About the details the proxy parameter format,
#' see \url{http://docs.sevenbridges.com/docs/upload-via-the-command-line}.
#'
#' @export misc_upload_cli
#' @references
#' \url{http://docs.sevenbridges.com/docs/upload-via-the-command-line}
#'
#' @examples
#' token = "your_token"
#' \donttest{misc_upload_cli(token = token,
#'                           uploader = "~/sbg-uploader/",
#'                           file = "~/example.fastq", project_id = "1234")}
misc_upload_cli = function (token = NULL, uploader = NULL,
                            file = NULL, project_id = NULL,
                            proxy = NULL) {

    if (is.null(token)) stop('token must be provided')
    if (is.null(uploader)) stop('SBG uploader location must be provided')
    if (is.null(file)) stop('File location must be provided')

    token = paste('-t', token)
    uploader = file.path(paste0(uploader, '/bin/sbg-uploader.sh'))
    file = file.path(file)

    if (!is.null(project_id)) project_id = paste('-p', project_id)
    if (!is.null(proxy)) proxy = paste('-x', proxy)

    cmd = paste(uploader, token, project_id, proxy, file)
    res = system(command = cmd, intern = TRUE)
    fid = strsplit(res, '\t')[[1]][1]
    return(fid)

}
