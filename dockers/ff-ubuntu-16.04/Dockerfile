FROM ubuntu:16.04

ARG GHC_VERSION=8.6.3
ARG apt_install="apt install --no-install-recommends --yes"

ENV LANG=C.UTF-8        \
    LC_ALL=C.UTF-8      \
    PATH=/opt/ghc/bin:$PATH

RUN apt update
RUN $apt_install software-properties-common

RUN add-apt-repository ppa:hvr/ghc
RUN apt update
RUN $apt_install ghc-$GHC_VERSION
RUN $apt_install g++ libtinfo-dev zlib1g-dev
        # qt5-default

# TODO(2019-04-02, #146) revert to qt5-default
RUN apt-add-repository ppa:beineri/opt-qt-5.12.0-xenial
RUN apt update
RUN $apt_install qt512base mesa-common-dev
