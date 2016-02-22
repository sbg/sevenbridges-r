FROM bioconductor/devel_base
MAINTAINER "Tengfei Yin" yintengfei@gmail.com

## Install some external dependencies. 360 MB
RUN apt-get update \
   && apt-get install -y --no-install-recommends -t unstable \
     default-jdk \
     default-jre \
     vim \
      emacs \
   && R CMD javareconf 

RUN Rscript -e 'library(BiocInstaller); biocLite("sevenbridges", dep = TRUE)'

## (because --deps TRUE can fail when packages are added/removed from CRAN)
RUN apt-get clean \
    && rm -rf /var/lib/apt/lists/ \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds


## install SBG Command line uploader
RUN wget https://igor.sbgenomics.com/sbg-uploader/sbg-uploader.tgz \
    && tar zxvf sbg-uploader.tgz -C ~ \
    && ln -s  ~/sbg-uploader/bin/sbg-uploader.sh /usr/local/bin/sbg-uploader.sh
    

## install SBG rabix
RUN pip install rabix

## install SBG Command line uploader
RUN wget https://cgc.sbgenomics.com/cgc-uploader/cgc-uploader.tgz \
    && tar zxvf cgc-uploader.tgz -C ~ \
    && ln -s  ~/cgc-uploader/bin/cgc-uploader.sh /usr/local/bin/cgc-uploader.sh

## start insert command line interface and report template needed
ADD src/runif.R /usr/local/bin/
RUN mkdir /report/ 
ADD report/report.Rmd /report/

