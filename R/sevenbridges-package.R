#' Seven Bridges Platform API Client and CWL Tool Builder in R
#'
#' R client and utilities for Seven Bridges platform API, from Cancer
#' Genomics Cloud to other Seven Bridges supported platforms.
#'
#' \tabular{ll}{ Package: \tab sevenbridges\cr Type: \tab Package\cr
#' License: \tab Apache License 2.0\cr }
#'
#' @name sevenbridges-package
#' @docType package
#' @author Nan Xiao <\email{nan.xiao@@sevenbridges.com}>
#'         Dusan Randjelovic <\email{dusan.randjelovic@@sevenbridges.com}>
#'         Emile Young <\email{emile.young@@sevenbridges.com}>
#'         Tengfei Yin <\email{tengfei.yin@@sevenbridges.com}>
#'
#' @import httr methods
#' @importFrom dplyr bind_rows
#' @importFrom utils modifyList
#' @importFrom rstudioapi getActiveDocumentContext
#' @importFrom liftr lift render_docker
#' @importFrom stats setNames
#' @importFrom curl curl_escape
#' @importFrom yaml yaml.load_file as.yaml yaml.load
#' @importFrom jsonlite toJSON fromJSON prettify unbox
#' @importFrom docopt docopt
#' @importClassesFrom S4Vectors List SimpleList
#' @importFrom S4Vectors elementType endoapply
#' @importClassesFrom objectProperties SingleEnum Enum
#' @importFrom objectProperties setSingleEnum
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom shiny textInput uiOutput fluidRow div
NULL
