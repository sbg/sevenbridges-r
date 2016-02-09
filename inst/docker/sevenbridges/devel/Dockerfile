FROM bioconductor/devel_base
MAINTAINER "Tengfei Yin" yintengfei@gmail.com

## Thanks to hadleyverse dockerfile:
## Add binaries for more CRAN packages, deb-src repositories in case we need `apt-get build-dep`
## RUN echo 'deb http://debian-r.debian.net/debian-r/ unstable main' >> /etc/apt/sources.list \
##   && gpg --keyserver keyserver.ubuntu.com --recv-keys AE05705B842492A68F75D64E01BF7284B26DD379 \
##   && gpg --export AE05705B842492A68F75D64E01BF7284B26DD379  | apt-key add - \
##   && echo 'deb-src http://debian-r.debian.net/debian-r/ unstable main' >> /etc/apt/sources.list \
##  && echo 'deb-src http://http.debian.net/debian testing main' >> /etc/apt/sources.list

## LaTeX:
## This installs inconsolata fonts used in R vignettes/manuals manually since texlive-fonts-extra is HUGE

## Install some external dependencies. 360 MB
RUN apt-get update \
## && apt-get -t unstable -y dist-upgrade \
   && apt-get install -y --no-install-recommends -t unstable \
     default-jdk \
     default-jre \
## libcairo2-dev \
##   libssl-dev \
## libgsl0-dev \
##   libmysqlclient-dev \
##   libopenblas-base \
##   libpq-dev \
##   libsqlite3-dev \
##   libv8-dev \
##   libxcb1-dev \
##   libxdmcp-dev \
##   libxml2-dev \
##   libxslt1-dev \
##   libxt-dev \
##   r-cran-rgl \
##   r-cran-rsqlite.extfuns \
   vim \
   emacs \
&& R CMD javareconf 

RUN Rscript -e 'library(BiocInstaller); biocLite("sevenbridges")'

## (because --deps TRUE can fail when packages are added/removed from CRAN)
RUN apt-get clean \
    && rm -rf /var/lib/apt/lists/ \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds


## install SBG Command line uploader
RUN wget https://igor.sbgenomics.com/sbg-uploader/sbg-uploader.tgz \
    && tar zxvf sbg-uploader.tgz -C ~ \
    && ln -s  ~/sbg-uploader/bin/sbg-uploader.sh /usr/local/bin/sbg-uploader.sh
    

## install SBG SDK
RUN wget https://bootstrap.pypa.io/get-pip.py \
    && python get-pip.py \
    && pip install sbg-cli

## install SBG rabix
RUN pip install rabix

## install CGC SDK
RUN pip install cgc-cli

## install SBG Command line uploader
RUN wget https://cgc.sbgenomics.com/cgc-uploader/cgc-uploader.tgz \
    && tar zxvf cgc-uploader.tgz -C ~ \
    && ln -s  ~/cgc-uploader/bin/cgc-uploader.sh /usr/local/bin/cgc-uploader.sh
    