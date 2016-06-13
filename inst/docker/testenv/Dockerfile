FROM rocker/hadleyverse
MAINTAINER "Tengfei Yin" yintengfei@gmail.com

## install docker (docker in docker for running with rabix)
RUN apt-get update && apt-get install -y \
    apt-transport-https \
    ca-certificates \
    wget

RUN apt-get install -y apt-transport-https ca-certificates \
    && apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D \
    && echo "deb https://apt.dockerproject.org/repo debian-stretch main" > /etc/apt/sources.list.d/docker.list \
    && echo "//APT::Default-Release \"testing\";" > /etc/apt/apt.conf.d/default   \
    && apt-get update \
    && apt-get install -y docker-engine

##################
### SBG rabix 
#################

# install bunny 
RUN wget https://github.com/rabix/bunny/releases/download/v0.4.0-alpha/rabix-0.4.0-alpha.tar.gz \
    && mkdir /opt/bunny && tar -zvxf rabix-0.4.0-alpha.tar.gz -C /opt/bunny \
    && ln -s  /opt/bunny/rabix.sh /usr/local/bin/rabix.sh

# rabix-legacy (python executor)
RUN apt-get update && apt-get install -y python-dev python-pip phantomjs libyaml-dev
RUN pip install git+https://github.com/rabix/rabix.git@devel -U
RUN apt-get install -y nodejs nodejs-legacy

# start docker service 
RUN usermod -aG docker root
RUN service docker start