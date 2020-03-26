# Seven Bridges API base url
.sbg_baseurl <- list(
  "cgc" = "https://cgc-api.sbgenomics.com/v2/",
  "aws-us" = "https://api.sbgenomics.com/v2/",
  "aws-eu" = "https://eu-api.sbgenomics.com/v2/",
  "ali-cn" = "https://api.sevenbridges.cn/v2/",
  "cavatica" = "https://cavatica-api.sbgenomics.com/v2/",
  "f4c" = "https://api.sb.biodatacatalyst.nhlbi.nih.gov/v2/"
)

# default platform
.sbg_default_platform <- "cgc"

# default user configuration file
.sbg_default_config_file <- "~/.sevenbridges/credentials"
.sbg_default_profile_name <- "default"

# default system environment variable names
.sbg_default_sysenv_url <- "SB_API_ENDPOINT"
.sbg_default_sysenv_token <- "SB_AUTH_TOKEN"

#' Set authentication environment variables for Seven Bridges API
#'
#' @param x Name of the system environment variable
#'
#' @return value of the environment variable
#'
#' @export sbg_get_env
#'
#' @examples
#' # set and get two environment variables for CGC
#' token <- "your_token"
#' \dontrun{
#' sbg_set_env("https://cgc-api.sbgenomics.com/v2", token)
#' sbg_get_env("SB_API_ENDPOINT")
#' sbg_get_env("SB_AUTH_TOKEN")}
sbg_get_env <- function(x) {
  res <- Sys.getenv(x)
  if (res == "") {
    stop("Environment variable ", x,
      " is blank, please check if it is set correctly",
      call. = FALSE
    )
  }
  res
}

#' Set authentication environment variables for Seven Bridges API
#'
#' @param url Base URL for API.
#' @param token Your authentication token.
#'
#' @return set two environment variables for authentication
#'
#' @export sbg_set_env
#'
#' @examples
#' # set and get environment variables for CGC
#' token <- "your_token"
#' \dontrun{
#' sbg_set_env("https://cgc-api.sbgenomics.com/v2", token)
#' sbg_get_env("SB_API_ENDPOINT")
#' sbg_get_env("SB_AUTH_TOKEN")}
sbg_set_env <- function(url = NULL, token = NULL) {
  if (is.null(url) | is.null(token)) {
    stop("url and token must be both specified", call. = FALSE)
  }

  args <- list(url, token)
  names(args) <- c(
    .sbg_default_sysenv_url,
    .sbg_default_sysenv_token
  )
  do.call(Sys.setenv, args)
}

# Read ini format file
# @param file character string, path to ini file
# @return Nested list keeping the hierarchical structure of the ini file
#' @importFrom stringr str_trim
.read_ini <- function(file) {

  # section name lines: starting with `[` ending with `]`
  pattern_section <- "^\\s*\\[\\s*(.+?)\\s*]"
  # key-value lines: key=value
  pattern_kv <- "^\\s*[^=]+=.+"

  x <- readLines(con = file, warn = FALSE)
  is_section <- grepl(pattern_section, x)
  idx_section <- which(is_section)
  count_section <- sum(is_section)

  cfg <- vector("list", count_section)

  # extract section names
  for (i in 1L:count_section) {
    names(cfg)[[i]] <- substr(
      x[idx_section[i]], 2L,
      nchar(x[idx_section[i]]) - 1L
    )
  }

  # extract key-value pairs
  range_section <- c(idx_section, length(x) + 1L)
  for (i in 1L:count_section) {
    for (j in (range_section[i] + 1L):(range_section[i + 1L] - 1L)) {
      tmp <- x[j]
      if (grepl(pattern_kv, tmp)) {
        kv <- stringr::str_trim(unlist(strsplit(tmp, "=")))
        cfg[[i]][kv[1L]] <- kv[2L]
      }
    }
  }

  # convert everything into a list
  for (i in 1L:count_section) cfg[[i]] <- as.list(cfg[[i]])

  cfg
}

# Write ini format file
# @param x nested list to write
# @param file character string naming the ini file
.write_ini <- function(x, file) {

  # create new blank file
  cat(NULL, file = file)

  for (i in names(x)) {
    # write section names
    cat(paste0("[", i, "]"), file = file, sep = "\n", append = TRUE)
    # write key-value pairs
    for (j in x[i]) cat(paste0(names(j), "=", j), file = file, sep = "\n", append = TRUE)
    # write new line between sections
    cat("", file = file, sep = "\n", append = TRUE)
  }

  # remove last redundant blank line
  x <- readLines(file)
  writeLines(x[1L:(length(x) - 1L)], file)
}

# parse Seven Bridges user config file into a nested list
sbg_parse_config <- function(file) {
  f <- file.path(path.expand(file))
  if (file.exists(f)) {
    res <- try(.read_ini(f), silent = TRUE)
    if (inherits(res, "try-error")) {
      stop("User config file format is incorrect", call. = FALSE)
      res <- NULL
    }
  } else {
    stop("User config file: ", f, " does not exist", call. = FALSE)
    res <- NULL
  }

  res
}

# add `/` to url ends
normalize_url <- function(x) if (!grepl("/$", x)) paste0(x, "/") else x

# platform name reverse lookup
sbg_platform_lookup <- function(baseurl) {
  x <- which(unlist(.sbg_baseurl) == normalize_url(baseurl))
  if (length(x) > 0L) names(x) else NULL
}

# append auth info to a simpleList (to every single component)
setAuth <- function(res, auth, className = NULL) {
  stopifnot(!is.null(className))

  rps <- response(res)
  if (is(res, className)) {
    res$auth <- auth
  } else if (is(res, "SimpleList")) {
    res <- endoapply(res, function(x) {
      x$auth <- auth
      x
    })
  }
  response(res) <- rps

  res
}
