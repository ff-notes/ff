FROM ubuntu:18.04

# ARG GHC_VERSION=8.6.3
ARG apt_install="apt install --no-install-recommends --yes"

ARG PACKAGE_NAME=ff
ARG BUILD_DIRECTORY=ff-deb

ENV LANG=C.UTF-8        \
    LC_ALL=C.UTF-8
    # PATH=/opt/ghc/bin:$PATH

RUN apt update
# RUN $apt_install software-properties-common

RUN $apt_install lintian fakeroot debhelper git curl
RUN apt update
RUN $apt_install g++ libtinfo-dev zlib1g-dev qt5-default ca-certificates libgmp-dev
RUN apt update

RUN curl -sSL https://get.haskellstack.org/ | sh

# RUN add-apt-repository ppa:hvr/ghc
# RUN apt update
# RUN $apt_install ghc-$GHC_VERSION

# RUN mkdir -p $BUILD_DIRECTORY
RUN mkdir -p $BUILD_DIRECTORY/opt/$PACKAGE_NAME/

COPY ./debian/ $BUILD_DIRECTORY/debian
COPY ./debian/docs $BUILD_DIRECTORY

RUN git clone https://github.com/willbasky/ff.git

RUN cd ff && stack --local-bin-path=../$BUILD_DIRECTORY/opt/$PACKAGE_NAME/ install $PACKAGE_NAME

RUN cd $BUILD_DIRECTORY && dpkg-buildpackage -rfakeroot -b -us -uc -d && cd -

RUN lintian --suppress-tags dir-or-file-in-opt,embedded-library *.deb
