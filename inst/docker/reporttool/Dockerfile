FROM rocker/tidyverse

MAINTAINER "Tengfei Yin" tengfei.yin@sevenbridges.com

RUN rm -f /var/lib/dpkg/available \
    && rm -rf  /var/cache/apt/* \
    && apt-get update \
    && apt-get install -y libssl-dev

RUN R -e "install.packages(c('packrat', 'devtools', 'rsconnect', 'shiny', 'rmarkdown'), repos = 'https://cloud.r-project.org/')"

ADD src/report.R /usr/local/bin/

RUN chmod a+x /usr/local/bin/report.R
