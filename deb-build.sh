#!/bin/bash
set -eux -o pipefail

#
# Environment
#
export PACKAGE_NAME=ff
export BUILD_DIRECTORY=ff-deb

#
# Pack deb
#
# Remove recent package stuff
rm -rf $BUILD_DIRECTORY
mkdir -p $BUILD_DIRECTORY
stack clean
#
# Build bin and copy to build directory
#
mkdir -p $BUILD_DIRECTORY/usr/bin/
stack --local-bin-path=$BUILD_DIRECTORY/usr/bin/ install $PACKAGE_NAME
#
# Copy debian info files
#
cp -r debian/ $BUILD_DIRECTORY/
cp -r $BUILD_DIRECTORY/debian/docs $BUILD_DIRECTORY/
#
# Build deb
#
cd $BUILD_DIRECTORY \
&& dpkg-buildpackage -rfakeroot -b -us -uc -d \
&& cd -
#
# Remove package directories
#
rm -rf $BUILD_DIRECTORY
#
# Lint deb
#

lintian *.deb

# rm *.deb *.buildinfo *.changes
