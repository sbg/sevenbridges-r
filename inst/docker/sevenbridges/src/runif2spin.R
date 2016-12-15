#' ---
#' title: "Uniform random number generator example"
#' output:
#'     html_document:
#'     toc: true
#' number_sections: true
#' ---

#' # summary report
#'
#' This is a random number generator

#+
args = commandArgs(TRUE)

r = runif(n   = as.integer(args[1]),
          min = as.numeric(args[2]),
          max = as.numeric(args[3]))
head(r)
summary(r)
hist(r)
