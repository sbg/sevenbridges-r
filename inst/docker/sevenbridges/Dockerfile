FROM rocker/rstudio-daily:verse
MAINTAINER "Tengfei Yin" yintengfei@gmail.com

####################
### shiny
###################

## thanks to official shiny docker file

RUN apt-get update && apt-get install -y -t unstable \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev/unstable \
    libxt-dev \
    libssl1.0.2:amd64
			    
# Download and install shiny server
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb

RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cran.rstudio.com/')"

RUN R -e "devtools::install_github('rstudio/addinexamples', type = 'source')"

RUN R -e "devtools::install_github('rstudio/rsconnect')"

RUN R -e "devtools::install_github('rstudio/shinyapps')"

RUN cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/

EXPOSE 3838

COPY src/shiny-server.sh /usr/bin/shiny-server.sh

####################
### addin
###################

RUN R -e "install.packages(c('rstudioapi', 'miniUI'), type = 'source'); "


##################
### Bioc
#################

RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("sevenbridges", dep = TRUE)'

## (because --deps TRUE can fail when packages are added/removed from CRAN)
RUN apt-get clean \
##     && rm -rf /var/lib/apt/lists/ \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds


##################
### SBG
#################

## install SBG Command line uploader
RUN wget https://igor.sbgenomics.com/sbg-uploader/sbg-uploader.tgz \
    && tar zxvf sbg-uploader.tgz -C ~ \
    && ln -s  ~/sbg-uploader/bin/sbg-uploader.sh /usr/local/bin/sbg-uploader.sh
    

## install SBG rabix
RUN wget https://github.com/rabix/bunny/releases/download/v0.1.0-alpha/rabix-0.1.1-alpha.tar.gz \
    && tar -zvxf rabix-0.1.1-alpha.tar.gz \
    && ln -s  /rabix-0.1.1-alpha/rabix.sh /usr/local/bin/rabix.sh


## install SBG Command line uploader
RUN wget https://cgc.sbgenomics.com/cgc-uploader/cgc-uploader.tgz \
    && tar zxvf cgc-uploader.tgz -C ~ \
    && ln -s  ~/cgc-uploader/bin/cgc-uploader.sh /usr/local/bin/cgc-uploader.sh

## start insert command line interface and report template needed
ADD src/runif.R /usr/local/bin/
RUN mkdir /report/ 
ADD report/report.Rmd /report/

RUN apt-get install -y supervisor
COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf

## ~/ShinyApps
RUN yes | /opt/shiny-server/bin/deploy-example user-dirs

## liftr
RUN R -e "devtools::install_github('road2stat/liftr', ref = 'devel')"
RUN R -e "devtools::install_github('rstudio/packrat')"

RUN apt-get -y install unp

## install docker
RUN apt-get update \
    && apt-get install -y apt-transport-https ca-certificates \
    && apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D

RUN echo "deb https://apt.dockerproject.org/repo debian-stretch main" > /etc/apt/sources.list.d/docker.list \
    && echo "//APT::Default-Release \"testing\";" > /etc/apt/apt.conf.d/default   \
    && apt-get update \
    && apt-get install -y docker-engine

## emacs

RUN apt-get install -y emacs

CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/conf.d/supervisord.conf"]