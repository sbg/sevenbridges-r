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
    libxt-dev
			    
# Download and install libssl 0.9.8
RUN wget --no-verbose http://ftp.us.debian.org/debian/pool/main/o/openssl/libssl0.9.8_0.9.8o-4squeeze14_amd64.deb && \
    dpkg -i libssl0.9.8_0.9.8o-4squeeze14_amd64.deb && \
    rm -f libssl0.9.8_0.9.8o-4squeeze14_amd64.deb

# Download and install shiny server
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb

RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cran.rstudio.com/')"
RUN R -e "devtools::install_github('rstudio/addinexamples', type = 'source')"

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
RUN apt-get install -y python-dev python-pip docker.io phantomjs libyaml-dev
RUN pip install rabix

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

CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/conf.d/supervisord.conf"]