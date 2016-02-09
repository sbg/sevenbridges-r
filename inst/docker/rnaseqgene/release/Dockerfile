FROM bioconductor/release_base
MAINTAINER "Tengfei Yin" yintengfei@gmail.com

RUN Rscript -e 'source("http://bioconductor.org/workflows.R"); workflowInstall("rnaseqGene")'

ADD performDE.R /usr/local/bin/
RUN mkdir /report/
ADD performDE.Rmd /report/

