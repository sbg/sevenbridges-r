#!/usr/bin/Rscript
'usage: runif.R [--seed=<float> --design=<file> --bam=<file>...]

options:
--seed=<float>  seed for set.seed() function [default: 1]
--design=<file> a design matrix
--bam=<file>... a list of bam files, should match design matrix'  -> doc

library(docopt)
opts <- docopt(doc)

## validation
if(is.null(opts$design)){
    stop("design file not provided")
}else{
    if(!file.exists(opts$design))
        stop("design file not exist")
}

if(is.null(opts$bam)){
    stop("bam files not provided")
}else{
    if(!all(file.exists(opts$bam)))
        stop("bam file not exist")
}
## create param list
lst <- list(seed = as.numeric(opts$seed),
            design = opts$design,
            bam = opts$bam)

## execute your Rmarkdown with these parameters
rmarkdown::render("/report/performDE.Rmd", BiocStyle::html_document(toc = TRUE),
                  output_dir = ".", params = lst)

