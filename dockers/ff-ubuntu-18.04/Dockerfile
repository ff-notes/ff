FROM ubuntu:18.04

ARG GHC_VERSION=8.6.3

ENV PATH=/opt/ghc/bin:$PATH

RUN apt update
RUN apt install --no-install-recommends --yes \
        curl g++ software-properties-common sudo zlib1g-dev

RUN add-apt-repository ppa:hvr/ghc
RUN apt update
RUN apt install --no-install-recommends --yes ghc-$GHC_VERSION
