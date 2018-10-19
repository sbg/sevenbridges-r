#' Download Seven Bridges command line uploader and extract to
#' a specified directory
#'
#' This function downloads Seven Bridges command line uploader and
#' extract the \code{.tgz} archive to a specified directory.
#'
#' @param platform Seven Bridges platform for which the uploader
#' is designed. Possible choices are:
#' \code{"cgc"} (Cancer Genomics Cloud),
#' \code{"aws-us"} (Amazon Web Services US),
#' \code{"aws-eu"} (Amazon Web Services EU),
#' and \code{"gcp"} (Google Cloud Platform).
#' Default is \code{"cgc"}.
#' @param destdir The directory to extract the downloaded Seven Bridges
#' command line uploader to. If the specified directory is not present,
#' it will be created.
#' @param quiet Should the download progress be printed?
#'
#' @return \code{0} if the command line uploader is successfully
#' downloaded and unarchived.
#'
#' @references
#' \url{https://docs.sevenbridges.com/docs/upload-via-the-command-line}
#'
#' @export get_uploader
#' @importFrom utils untar download.file
#'
#' @examples
#' # Download CGC CLI uploader to `~/Downloads`
#' dir <- "~/Downloads/"
#' \dontrun{
#' get_uploader("cgc", dir)}
get_uploader <- function(platform = c("cgc", "aws-us", "aws-eu", "gcp"), destdir = NULL, quiet = FALSE) {
  if (is.null(destdir)) stop("Please provide `destdir` to extract the archive")

  platform <- match.arg(platform)

  uploader_dict <- c(
    "cgc" = "https://cgc.sbgenomics.com/cgc-uploader/cgc-uploader.tgz",
    "aws-us" = "https://igor.sbgenomics.com/sbg-uploader/sbg-uploader.tgz",
    "aws-eu" = "https://eu.sbgenomics.com/sbg-uploader/sbg-uploader.tgz",
    "gcp" = "https://gcp.sbgenomics.com/gcp-uploader/sbg-uploader.tgz"
  )

  uploader_url <- uploader_dict[platform]

  tmpfile <- tempfile()

  download.file(
    url = uploader_url, method = "libcurl",
    destfile = tmpfile, quiet = quiet
  )

  untar(tarfile = tmpfile, exdir = path.expand(destdir))
}

#' @rdname get_uploader
#' @export misc_get_uploader
misc_get_uploader <- function() {
  .Deprecated("get_uploader")
}

.ostype <- function() {
  sysname <- Sys.info()[["sysname"]]
  if (sysname == "Windows") return("Windows")
  if (sysname == "Linux") return("Linux")
  if (sysname == "Darwin") return("MacOS")
}

#' Upload files using Seven Bridges command line uploader
#'
#' Upload files using Seven Bridges command line uploader.
#'
#' @param token Authentication token.
#' @param uploader The directory where the command line uploader
#' is located (the directory that contains the \code{bin/} directory).
#' @param file The location of the (single) file to upload. To upload multiple
#' files, please use \code{manifest_file} to specify.
#' @param project The project identifier (e.g. \code{username/project-name})
#' to upload files to. This option is mandatory. To upload files to a project,
#' you must be a member of that project and must have the write permission
#' granted by the project administrator.
#' @param proxy A proxy server through which the uploader should connect.
#' For details the proxy parameter format, see the part on parameter
#' \code{--proxy} in the reference below.
#' @param tag Tags for your the files (optional). Use a vector of
#' character strings, for instance, \code{c("tag one", "the second tag")}.
#' @param manifest_file Location of the manifest file (for uploading
#' multiple files with metadata). See the reference URL below for the
#' format of a manifest file.
#' @param manifest_metadata Should we use all, none, or only a part of the
#' the metadata fields included in the manifest file? Default is \code{"all"}.
#' @param metadata_fields Character vector, the metadata fields to use
#' in the manifest file. This should be specified if and only if
#' \code{manifest_metadata = "partial"}.
#' @param dry_run Should we just output the data and check the settings
#' without uploading anything? Default is \code{FALSE}.
#' @param dry_run_fields Character vector, specific metadata fields
#' to output information about when \code{dry_run = TRUE}.
#'
#' @return The uploaded file's ID number.
#'
#' @note To use the command line uploader, Java 1.7 or newer
#' should be installed. See the reference link below for details.
#'
#' @seealso See \code{\link{get_uploader}} for downloading
#' the command line uploader for Seven Bridges platforms.
#' See \code{\link{cli_list_projects}} and \code{\link{cli_list_tags}}
#' for listing available projects or tags with the command line uploader.
#'
#' @export cli_upload
#'
#' @references
#' Seven Bridges Command Line Uploader:
#' \url{https://docs.sevenbridges.com/docs/upload-via-the-command-line}
#' Manifest file format:
#' \url{https://docs.sevenbridges.com/docs/format-of-a-manifest-file}
#'
#' @examples
#' token <- "your_token"
#' \dontrun{
#' cli_upload(
#'   token = token,
#'   uploader = "~/Downloads/cgc-uploader/",
#'   file = "~/example.fastq", project = "username/project-name"
#' )}
cli_upload <- function(token = NULL, uploader = NULL,
                       file = NULL, project = NULL,
                       proxy = NULL, tag = NULL,
                       manifest_file = NULL,
                       manifest_metadata = c("all", "none", "partial"),
                       metadata_fields = NULL,
                       dry_run = FALSE, dry_run_fields = NULL) {
  if (is.null(token)) {
    stop("Please provide the authentication token")
  }
  token <- paste("--token", token)

  if (is.null(uploader)) {
    stop("Please provide the path to the command line uploader")
  }

  # determine system type to use *.bat or *.sh
  if (.ostype() %in% c("Linux", "MacOS")) ext <- "sh"
  if (.ostype() == "Windows") ext <- "bat"

  # look for the executable with this extension (file names may vary)
  exe <- list.files(paste0(normalizePath(uploader), "/bin/"),
    pattern = paste0("*.", ext, "$")
  )[1L]
  uploader <- file.path(paste0(uploader, "/bin/", exe))

  if (is.null(project)) {
    stop('Please provide the project name, e.g. "username/project-name"')
  }
  if (!grepl("/", project)) {
    stop('Please provide a project name with the valid format: "username/project-name"')
  }
  project <- paste("--project", project)

  if (!is.null(proxy)) proxy <- paste("--proxy", proxy)

  dry_run_text <- if (dry_run) {
    paste("--dry-run", paste0(dry_run_fields, collapse = " "))
  } else {
    NULL
  }

  if (!is.null(tag)) tag <- paste(paste0(" --tag ", '"', tag, '"'), collapse = "")

  if (is.null(file) & is.null(manifest_file)) {
    stop("At least one of `file`` and `manifest_file` should be not NULL")
  }

  if (!is.null(file) & !is.null(manifest_file)) {
    stop("To avoid confusion, please use only one of `file`` and `manifest_file` at once")
  }

  if (!is.null(file) & is.null(manifest_file)) {
    file <- file.path(file)
    cmd <- paste(uploader, token, project, proxy, dry_run_text, tag, file)
  }

  if (is.null(file) & !is.null(manifest_file)) {
    manifest_file <- paste("--manifest-file", file.path(manifest_file))

    manifest_metadata <- match.arg(manifest_metadata)
    if (manifest_metadata == "partial") {
      manifest_metadata <- paste("--manifest-metadata", paste(metadata_fields, collapse = " "))
    }
    if (manifest_metadata == "all") {
      manifest_metadata <- paste("--manifest-metadata")
    }
    if (manifest_metadata == "none") {
      manifest_metadata <- NULL
    }

    cmd <- paste(uploader, token, project, proxy, dry_run_text, tag, manifest_metadata, manifest_file)
  }

  res <- system(command = cmd, intern = FALSE)
  invisible(res)
}

#' @rdname cli_upload
#' @export misc_upload_cli
misc_upload_cli <- function() {
  .Deprecated("cli_upload")
}

#' List projects using Seven Bridges command line uploader
#'
#' List projects available as upload targets using Seven Bridges
#' command line uploader.
#'
#' @param token Authentication token.
#' @param uploader The directory where Seven Bridges command line uploader
#' is located (the directory that contains the \code{bin/} directory).
#' @param proxy A proxy server through which the uploader should connect.
#' For details the proxy parameter format, see the part on parameter
#' \code{--proxy} in the reference below.
#'
#' @references
#' \url{https://docs.sevenbridges.com/docs/upload-via-the-command-line}
#'
#' @return Character vector of the available project names.
#'
#' @seealso See \code{\link{cli_upload}} for uploading files with
#' the command line uploader, \code{\link{cli_list_tags}} for listing
#' all tags in a project.
#'
#' @export cli_list_projects
#'
#' @examples
#' token <- "your_token"
#' \dontrun{
#' cli_list_projects(
#'   token = token,
#'   uploader = "~/Downloads/sbg-uploader/"
#' )}
cli_list_projects <- function(
                              token = NULL, uploader = NULL, proxy = NULL) {
  if (is.null(token)) {
    stop("Please provide the authentication token")
  }
  token <- paste("--token", token)

  if (is.null(uploader)) {
    stop("Please provide the path to the command line uploader")
  }

  # determine system type to use *.bat or *.sh
  if (.ostype() %in% c("Linux", "MacOS")) ext <- "sh"
  if (.ostype() == "Windows") ext <- "bat"

  # look for the executable with this extension (file names may vary)
  exe <- list.files(paste0(normalizePath(uploader), "/bin/"),
    pattern = paste0("*.", ext, "$")
  )[1L]
  uploader <- file.path(paste0(uploader, "/bin/", exe))

  if (!is.null(proxy)) proxy <- paste("--proxy", proxy)

  cmd <- paste(uploader, token, proxy, "--list-projects")

  res <- system(command = cmd, intern = TRUE)
  project_names <- sapply(strsplit(res, split = "\t"), "[[", 1)
  project_names
}

#' List all the tags in project using Seven Bridges command line uploader
#'
#' List all the tags in a destination project using Seven Bridges
#' command line uploader.
#'
#' @param token Authentication token.
#' @param uploader The directory where Seven Bridges command line uploader
#' is located (the directory that contains the \code{bin/} directory).
#' @param project Unique identifier of the project,
#' for example, \code{"username/project-name"}.
#' @param proxy A proxy server through which the uploader should connect.
#' For details the proxy parameter format, see the part on parameter
#' \code{--proxy} in the reference below.
#'
#' @references
#' \url{https://docs.sevenbridges.com/docs/upload-via-the-command-line}
#'
#' @return Chracter vector of file tags in the project.
#'
#' @seealso See \code{\link{cli_upload}} for uploading files with
#' the command line uploader, \code{\link{cli_list_projects}} for listing
#' available projects.
#'
#' @export cli_list_tags
#'
#' @examples
#' token <- "your_token"
#' \dontrun{
#' cli_list_tags(
#'   token = token,
#'   uploader = "~/Downloads/sbg-uploader/",
#'   project = "username/project-name"
#' )}
cli_list_tags <- function(
                          token = NULL, uploader = NULL, project = NULL, proxy = NULL) {
  if (is.null(token)) {
    stop("Please provide the authentication token")
  }
  token <- paste("--token", token)

  if (is.null(uploader)) {
    stop("Please provide the path to the command line uploader")
  }

  # determine system type to use *.bat or *.sh
  if (.ostype() %in% c("Linux", "MacOS")) ext <- "sh"
  if (.ostype() == "Windows") ext <- "bat"

  # look for the executable with this extension (file names may vary)
  exe <- list.files(paste0(normalizePath(uploader), "/bin/"),
    pattern = paste0("*.", ext, "$")
  )[1L]
  uploader <- file.path(paste0(uploader, "/bin/", exe))

  if (is.null(project)) {
    stop('Please provide the project name, e.g. "username/project-name"')
  }
  if (!grepl("/", project)) {
    stop('Please provide a project name with the valid format: "username/project-name"')
  }
  project <- paste("--project", project)

  if (!is.null(proxy)) proxy <- paste("--proxy", proxy)

  cmd <- paste(uploader, token, project, proxy, "--list-tags")

  res <- system(command = cmd, intern = TRUE)
  res
}
