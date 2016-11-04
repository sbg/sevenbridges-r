#' R Client for Seven Bridges Platform API and CWL Tool builder in R
#'
#' R client and utilities for Seven Bridges platform API, from Cancer
#' Genomics Cloud to other Seven Bridges supported platforms.
#'
#' \tabular{ll}{ Package: \tab sevenbridges\cr Type: \tab Package\cr
#' License: \tab Apache License 2.0\cr }
#'
#' @name sevenbridges-package
#' @docType package
#' @author Nan Xiao <\email{nan.xiao@@sbgenomics.com}>
#'         Dusan Randjelovic <\email{dusan.randjelovic@sbgenomics.com}>
#'         Emile Young <\email{emile.young@sbgenomics.com}>
#'         Tengfei Yin <\email{tengfei.yin@@sbgenomics.com}>
#'    
#' 
#' @import httr methods
#' @importFrom dplyr bind_rows
#' @importFrom utils modifyList
#' @importFrom rstudioapi getActiveDocumentContext
#' @importFrom liftr lift drender
#' @importFrom stats setNames
#' @importFrom curl curl_escape
#' @importFrom yaml yaml.load_file as.yaml yaml.load
#' @importFrom jsonlite toJSON fromJSON prettify unbox
#' @importFrom docopt docopt
#' @importClassesFrom S4Vectors characterORNULL List SimpleList
#' @importFrom S4Vectors elementType endoapply
#' @importClassesFrom objectProperties SingleEnum Enum
#' @importFrom objectProperties setSingleEnum
NULL
