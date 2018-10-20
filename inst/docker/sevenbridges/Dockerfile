FROM rocker/tidyverse
MAINTAINER "Tengfei Yin" yintengfei@gmail.com

## Install Docker (Docker in Docker)
RUN apt-get update && apt-get install -y  \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    curl \
    libcairo2-dev \
    libxt-dev \
    unp \
    emacs \
    supervisor \
    libfuse-dev \
    gnupg
#   aufs-tools \
#   cgroupfs-mount

# RUN wget --no-verbose http://ftp.us.debian.org/debian/pool/main/l/lvm2/libdevmapper1.02.1_1.02.136-1_amd64.deb && \
#    dpkg -i libdevmapper1.02.1_1.02.136-1_amd64.deb && \
#    rm -f libdevmapper1.02.1_1.02.136-1_amd64.deb

# RUN wget --no-verbose http://ftp.us.debian.org/debian/pool/main/libt/libtool/libltdl7_2.4.6-2_amd64.deb && \
#    dpkg -i libltdl7_2.4.6-2_amd64.deb && \
#    rm -f libltdl7_2.4.6-2_amd64.deb

## Install Docker
RUN apt-get install -y apt-transport-https ca-certificates \
    && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D \
    && echo "deb https://apt.dockerproject.org/repo debian-$(lsb_release -cs) main" > /etc/apt/sources.list.d/docker.list \
    && echo "//APT::Default-Release \"testing\";" > /etc/apt/apt.conf.d/default   \
    && apt-cache policy docker-engine

RUN apt-get update -y \
    && apt-get upgrade -y \
    && apt-get install -y docker-engine

#################
# RStudio addin #
#################

RUN Rscript -e "install.packages(c('rstudioapi', 'miniUI'), type = 'source');\
    devtools::install_github('rstudio/addinexamples')"

##################
# sevenbridges-r #
##################

# Install from GitHub instead of Bioconductor for the latest version
RUN Rscript -e "install.packages('BiocManager');\
    devtools::install_github('sbg/sevenbridges-r', repos = BiocManager::repositories(), build_vignettes = FALSE, dependencies = TRUE)"

#####################
# Seven Bridges SDK #
#####################

## Install SBG Rabix
RUN wget https://github.com/rabix/bunny/releases/download/v1.0.3/rabix-1.0.3.tar.gz \
    && tar -zvxf rabix-1.0.3.tar.gz \
    && ln -s  /rabix-cli-1.0.3/rabix /usr/local/bin/rabix

## Install SBG Command line uploader
# RUN wget https://igor.sbgenomics.com/sbg-uploader/sbg-uploader.tgz \
#     && tar zxvf sbg-uploader.tgz -C / \
#    && ln -s  /sbg-uploader/bin/sbg-uploader.sh /usr/local/bin/sbg-uploader.sh

## Install CGC Command line uploader
# RUN wget https://cgc.sbgenomics.com/cgc-uploader/cgc-uploader.tgz \
#    && tar zxvf cgc-uploader.tgz -C / \
#    && ln -s  /cgc-uploader/bin/cgc-uploader.sh /usr/local/bin/cgc-uploader.sh

## Copy command line interface and report templates needed
ADD src/runif.R /usr/local/bin/
RUN mkdir /report/
ADD report/report.Rmd /report/

## Install liftr
RUN Rscript -e "devtools::install_github('road2stat/liftr', ref = 'devel')"
RUN Rscript -e "devtools::install_github('rstudio/packrat')"

## (because --deps TRUE can fail when packages are added/removed from CRAN)
RUN rm -rf /var/lib/apt/lists/ \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

################
# Shiny Server #
################

## Thanks: rocker-org/shiny

## Download and install Shiny Server
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O shiny-server-latest.deb && \
    gdebi -n shiny-server-latest.deb && \
    rm -f version.txt shiny-server-latest.deb

RUN Rscript -e "install.packages(c('shiny', 'rmarkdown', 'rsconnect'), repos = 'https://cloud.r-project.org/')"

RUN cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/

RUN mkdir /home/rstudio/ShinyApps/

RUN cp -R /usr/local/lib/R/site-library/shiny/examples/* /home/rstudio/ShinyApps/

EXPOSE 3838 8787

# COPY src/shiny-server.sh /usr/bin/shiny-server.sh
# RUN wget --no-verbos https://raw.githubusercontent.com/sbg/sevenbridges-r/master/inst/docker/sevenbridges/src/shiny-server.conf -P /etc/shiny-server/
# RUN wget --no-verbos https://raw.githubusercontent.com/sbg/sevenbridges-r/master/inst/docker/sevenbridges/src/supervisord.conf	-P /etc/shiny-server/
COPY src/shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY src/supervisord.conf /etc/supervisor/conf.d/supervisord.conf

## set directory to `~/ShinyApps`
# RUN yes | /opt/shiny-server/bin/deploy-example user-dirs
# RUN cp -R /usr/local/lib/R/site-library/shiny/examples/* ~/ShinyApps/

CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/conf.d/supervisord.conf"]
# CMD ["/init;/usr/bin/shiny-server.sh"]
# CMD ["sh", "-c", "/usr/bin/shiny-server.sh;/init"]
