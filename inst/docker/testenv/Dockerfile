FROM ubuntu:14.04

LABEL maintainer="nan.xiao@sevenbridges.com"

## install docker engine (docker client/executable is needed - daemon is bind mounted from host)
RUN apt-get update && apt-get install -y \
    apt-transport-https \
    ca-certificates \
    wget
RUN apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
RUN echo "deb https://apt.dockerproject.org/repo ubuntu-trusty main" > /etc/apt/sources.list.d/docker.list
RUN apt-get update && apt-get install -y \
    docker-engine

###################
# SBG rabix/bunny #
###################

# Java
RUN apt-get -y install software-properties-common
RUN add-apt-repository ppa:webupd8team/java
RUN apt-get update
RUN echo oracle-java7-installer shared/accepted-oracle-license-v1-1 select true | /usr/bin/debconf-set-selections
RUN apt-get -y install oracle-java7-installer && apt-get clean
ENV JAVA_HOME /usr/lib/jvm/java-7-oracle

# bunny 0.6.5-alpha
RUN wget https://github.com/rabix/bunny/releases/download/v0.6.5-alpha/rabix-0.6.5-alpha.tar.gz \
    && mkdir /opt/bunny && tar -zvxf rabix-0.6.5-alpha.tar.gz -C /opt/bunny \
    && chmod +x /opt/bunny/rabix
ENV DOCKER_HOST unix:///var/run/docker.sock

# rabix-legacy (python executor)
RUN apt-get update && apt-get install -y python-dev python-pip phantomjs libyaml-dev
RUN pip install git+https://github.com/rabix/rabix.git@devel -U
RUN apt-get install -y nodejs nodejs-legacy

# cwl-runner
RUN pip install cwl-runner
