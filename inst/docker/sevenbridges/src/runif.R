#!/usr/local/bin/Rscript
'usage: runif.R [--n=<int> --min=<float> --max=<float> --seed=<float>]

options:
--n=<int>        number of observations. If length(n) > 1, the length is taken to be the number required [default: 1].
--min=<float>   lower limits of the distribution. Must be finite [default: 0].
--max=<float>   upper limits of the distribution. Must be finite [default: 1].
--seed=<float>  seed for set.seed() function [default: 1]'  -> doc

library("docopt")
opts <- docopt(doc)

# create param list
lst <- list(
  n = as.integer(opts$n),
  min = as.numeric(opts$min),
  max = as.numeric(opts$max),
  seed = as.numeric(opts$seed)
)

# execute your Rmarkdown with these parameters
rmarkdown::render("/report/report.Rmd", rmarkdown::html_document(toc = TRUE), output_dir = ".", params = lst)
