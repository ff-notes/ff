FROM ubuntu:18.04

# ARG GHC_VERSION=8.6.3
ARG apt_install="apt install --no-install-recommends --yes"
ARG PACKAGE_NAME=ff
ARG BUILD_DIRECTORY=ff-deb
ARG BIN=$BUILD_DIRECTORY/opt/$PACKAGE_NAME

ENV LANG=C.UTF-8        \
    LC_ALL=C.UTF-8
    # PATH=/opt/ghc/bin:$PATH

RUN mkdir -p $BIN

# RUN apt update
# RUN $apt_install software-properties-common

# RUN add-apt-repository ppa:hvr/ghc
RUN apt update
# RUN $apt_install ghc-$GHC_VERSION
RUN $apt_install g++ libtinfo-dev zlib1g-dev qt5-default
