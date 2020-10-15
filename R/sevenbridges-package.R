#' Seven Bridges Platform API Client and CWL Tool Builder in R
#'
#' R client and utilities for Seven Bridges platform API, from Cancer
#' Genomics Cloud to other Seven Bridges supported platforms.
#'
#' \tabular{ll}{
#' Package: \tab sevenbridges\cr
#' Type: \tab Package\cr
#' License: \tab Apache License 2.0}
#'
#' @name sevenbridges-package
#' @docType package
#' @author
#' Soner Koc <\email{soner.koc@@sevenbridges.com}>
#' Nan Xiao
#' Tengfei Yin
#' Dusan Randjelovic
#' Emile Young
#'
#' @import httr methods
#' @importFrom data.table rbindlist
#' @importFrom utils modifyList
#' @importFrom stats setNames
#' @importFrom curl curl_escape
#' @importFrom yaml yaml.load_file as.yaml yaml.load
#' @importFrom jsonlite toJSON fromJSON prettify unbox
#' @importFrom docopt docopt
#' @importClassesFrom S4Vectors List SimpleList
#' @importFrom S4Vectors elementType endoapply
#' @importClassesFrom objectProperties SingleEnum Enum
#' @importFrom objectProperties setSingleEnum
NULL
