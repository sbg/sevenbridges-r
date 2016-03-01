#!/usr/bin/Rscript
"usage: performDE.R [--design=<file> --gtffile=<file> --bamfiles=<file>]

options:
--bamfiles=<file> bamfiles
--design=<file> design data frame
--gtffile=<file> gene feature file
" -> doc

library(docopt)
opts <- docopt(doc)

.design <- opts$design
if(is.null(.design))
    .design <- system.file("extdata/sample_table.csv", package="airway")

.bamfiles <- opts$bamfiles
if(is.null(.bamfiles)){
    .bamfiles <- list.files(system.file("extdata", package="airway"), "*.bam",
                            full.names = TRUE)
}else{
   .bamfiles <- strsplit(opts$bamfiles, ",")[[1]]
}

.gtffile <- opts$gtffile
if(is.null(.gtffile))
    .gtffile <- system.file("extdata/Homo_sapiens.GRCh37.75_subset.gtf", 
                            package="airway")
## create param list
lst <- list(design = normalizePath(.design), 
            gtffile = normalizePath(.gtffile), 
            bamfiles = normalizePath(.bamfiles),
            currentPath = normalizePath("."))

## execute your Rmarkdown with these parameters
## rmarkdown::render("/report/rnaseqGene.Rmd", BiocStyle::pdf_document(toc = TRUE),
##                  output_dir = ".", params = lst)

rmarkdown::render("/report/rnaseqGene.Rmd", "pdf_document", 
                  output_dir = ".",
                  params = lst)