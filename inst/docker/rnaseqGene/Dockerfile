FROM rocker/tidyverse

LABEL maintainer="nan.xiao@sevenbridges.com"

RUN Rscript -e 'source("http://bioconductor.org/workflows.R"); workflowInstall("rnaseqGene")'

ADD src/performDE.R /usr/local/bin/
RUN mkdir /report
ADD report/rnaseqGene.Rmd /report/

RUN chmod a+x /usr/local/bin/performDE.R \
    && chmod -R a+x /report
