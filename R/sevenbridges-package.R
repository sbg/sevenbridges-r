#' R Client for Seven Bridges Genomics API
#'
#' R Client for Seven Bridges Genomics API
#'
#' The vignette can be opened with \code{vignette('sevenbridges')}.
#'
#' \tabular{ll}{ Package: \tab sevenbridges\cr Type: \tab Package\cr
#' License: \tab MIT\cr }
#'
#' @name sevenbridges-package
#' @docType package
#' @author Nan Xiao <\email{nan.xiao@@sbgenomics.com}>
#'         Tengfei Yin <\email{tengfei.yin@@sbgenomics.com}>
#' @import httr methods
#' @importFrom utils modifyList
#' @importFrom liftr lift drender
#' @importFrom stats setNames
#' @importFrom curl curl_escape
#' @importFrom yaml yaml.load_file as.yaml yaml.load
#' @importFrom jsonlite toJSON prettify unbox
#' @importFrom docopt docopt
#' @importClassesFrom S4Vectors characterORNULL List SimpleList
#' @importFrom S4Vectors elementType endoapply
#' @importClassesFrom objectProperties SingleEnum Enum
#' @importFrom objectProperties setSingleEnum
NULL



