#!/usr/bin/Rscript
"usage: performDE.R (--design=<file> --gtffile=<file> --bamfiles=<file>)

options:
--bamfiles=<file> bamfiles
--design=<file> design data frame
--gtffile=<file> gene feature file
" -> doc

library(docopt)
opts <- docopt(doc)

## create param list
lst <- list(design = normalizePath(opts$design), 
            gtffile = normalizePath(opts$gtffile), 
            bamfiles = normalizePath(strsplit(opts$bamfiles, ",")[[1]]))

## execute your Rmarkdown with these parameters
rmarkdown::render("/report/rnaseqGene.Rmd", BiocStyle::html_document(toc = TRUE),
                  output_dir = ".", params = lst)

