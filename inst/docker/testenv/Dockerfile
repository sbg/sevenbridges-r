FROM ubuntu:14.04
MAINTAINER "Dusan Randjelovic" dusan.randjelovic@sbgenomics.com

## install docker engine (docker client/executable is needed - daemon is bind mounted from host)
RUN apt-get update && apt-get install -y \
    apt-transport-https \
    ca-certificates \
    wget
RUN apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
RUN echo "deb https://apt.dockerproject.org/repo ubuntu-trusty main" > /etc/apt/sources.list.d/docker.list 
RUN apt-get update && apt-get install -y \
    docker-engine

##################
### SBG rabix/bunny 
#################

# install java
RUN apt-get -y install software-properties-common
RUN add-apt-repository ppa:webupd8team/java
RUN apt-get update
RUN echo oracle-java7-installer shared/accepted-oracle-license-v1-1 select true | /usr/bin/debconf-set-selections
RUN apt-get -y install oracle-java7-installer && apt-get clean
ENV JAVA_HOME /usr/lib/jvm/java-7-oracle

# install bunny 0.4.0
RUN wget https://github.com/rabix/bunny/releases/download/v0.4.0-alpha/rabix-0.4.0-alpha.tar.gz \
    && mkdir /opt/bunny && tar -zvxf rabix-0.4.0-alpha.tar.gz -C /opt/bunny \
    && ln -s  /opt/bunny/rabix.sh /usr/local/bin/rabix.sh

# rabix-legacy (python executor)
#RUN apt-get update && apt-get install -y python-dev python-pip phantomjs libyaml-dev
#RUN pip install git+https://github.com/rabix/rabix.git@devel -U
#RUN apt-get install -y nodejs nodejs-legacy

# start docker service (only needed for DinD, not host mode)
#RUN usermod -aG docker root
#ENTRYPOINT service docker start && bash