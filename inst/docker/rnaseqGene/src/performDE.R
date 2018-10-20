#!/usr/bin/Rscript
"usage: performDE.R [options]

options:
--bamfiles=<file> bamfiles
--design=<file> design data frame
--gtffile=<file> gene feature file
--format=<string> pdf or html. [default: html]
" -> doc

library("docopt")
opts <- docopt(doc)

.design <- opts$design
if (is.null(.design)) {
  .design <- system.file("extdata/sample_table.csv", package = "airway")
}

.bamfiles <- opts$bamfiles
if (is.null(.bamfiles)) {
  .bamfiles <- list.files(system.file("extdata", package = "airway"), "*.bam",
    full.names = TRUE
  )
} else {
  .bamfiles <- strsplit(opts$bamfiles, ",")[[1]]
}

.gtffile <- opts$gtffile
if (is.null(.gtffile)) {
  .gtffile <- system.file("extdata/Homo_sapiens.GRCh37.75_subset.gtf",
    package = "airway"
  )
}

# create param list
lst <- list(
  design = normalizePath(.design),
  gtffile = normalizePath(.gtffile),
  bamfiles = normalizePath(.bamfiles),
  currentPath = normalizePath(".")
)

# render the report
.format <- switch(opts$format,
  "pdf" = "pdf_document",
  "html" = "html_document", {
    "pdf_document"
  }
)

rmarkdown::render("/report/rnaseqGene.Rmd", .format,
  output_dir = ".",
  params = lst
)

# For rabix execution in the cloud,
# this is now a workaround to move intermediate files
