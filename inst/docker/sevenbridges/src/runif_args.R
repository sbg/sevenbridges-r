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
args <- commandArgs(TRUE)

## quick hack to split named arguments
splitArgs <- function(x) {
    res <- do.call(rbind, lapply(x, function(i){
        res <- strsplit(i, "=")[[1]]
        nm <- gsub("-+", "",res[1])
        c(nm, res[2])
    }))
    .r <- res[,2]
    names(.r) <- res[,1]
    .r
}
args <- splitArgs(args)

#+
r <- runif(n   = as.integer(args["n"]),
           min = as.numeric(args["min"]),
           max = as.numeric(args["max"]))
summary(r)
hist(r)
write.csv(r, file = "out.csv")
