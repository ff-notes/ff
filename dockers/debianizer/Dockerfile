FROM ubuntu:18.04

ENV LANG=C.UTF-8        \
    LC_ALL=C.UTF-8

ARG EULER_UID
ARG EULER_GID
ARG apt_install="apt install --no-install-recommends --yes"
ARG PACKAGE_NAME=ff
ARG USER=ff
ARG GROUP=ff
ARG REPO=$HOME/ff-repo
ARG TMP_BIN=$HOME/temp
ARG HOME=/home/$USER
ARG BUILD_DIRECTORY=ff-deb
ARG BIN=$BUILD_DIRECTORY/opt/$PACKAGE_NAME

RUN \
# Check for mandatory build arguments
    : "${EULER_UID:?mandatory build argument is missing}" \
    : "${EULER_GID:?mandatory build argument is missing}"

RUN groupadd -g ${EULER_GID} $GROUP
RUN useradd -m -u ${EULER_UID} -g ${EULER_GID} $USER

RUN mkdir $HOME/.stack
RUN mkdir $TMP_BIN
RUN mkdir $REPO

RUN chown -R ${EULER_UID}:${EULER_GID} $HOME/

# VOLUME $HOME/.stack
# VOLUME $HOME/$REPO

RUN apt update
RUN $apt_install lintian fakeroot debhelper git curl tree \
  g++ libtinfo-dev zlib1g-dev qt5-default ca-certificates libgmp-dev

WORKDIR $HOME

RUN curl -sSL https://get.haskellstack.org/ | sh

RUN git clone https://github.com/willbasky/ff.git $REPO
RUN chown -R ${EULER_UID}:${EULER_GID} $HOME/

# RUN tree

RUN stack --allow-different-user --resolver=lts-14.20 install aeson

WORKDIR $REPO

RUN stack --allow-different-user --local-bin-path=$TMP_BIN/ install $PACKAGE_NAME

WORKDIR $HOME
RUN mkdir -pv $BUILD_DIRECTORY
RUN mkdir -pv $BIN
RUN cp $TMP_BIN/$PACKAGE_NAME $BIN
VOLUME $BUILD_DIRECTORY
RUN tree

# RUN chown -R ${EULER_UID}:${EULER_GID} $HOME/

WORKDIR $BUILD_DIRECTORY

COPY . .
# RUN chown -R ${EULER_UID}:${EULER_GID} $HOME/
RUN tree

WORKDIR $HOME
# RUN chown -R ${EULER_UID}:${EULER_GID} $HOME/
# RUN tree





