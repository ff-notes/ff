#
# Environment
#
BIN_NAME=ff

PACKAGE_NAME=ff

MAINTAINER_NAME=Yuriy Syrovetskiy

MAINTAINER_EMAIL=ff@cblp.su

PACKAGE_DESCRIPTION=Distributed note taker and task manager.

EXTENDED_PACKAGE_DESCRIPTION=Available offline and synchronizes without conflicts.

SOURCE_URL=https://github.com/ff-notes/ff

LOCAL_BINARY_PATH=$(shell stack path --local-install-root)

APP_VERSION=$(shell stack query locals $(PACKAGE_NAME) version | tr -d \')

BIN_FILEPATH=$(LOCAL_BINARY_PATH/bin/$(BIN_NAME)

BUILD_DIRECTORY=ff-deb

YEAR=2019

LICENSE=$(shell cat LICENSE)

#
# pack deb
#
.PHONY: deb
deb:
	#
	# Remove recent package directories
	rm -rf $(BUILD_DIRECTORY)
	rm -rf debian

	mkdir -p $(BUILD_DIRECTORY)

	stack build $(PACKAGE_NAME)
	#
	# Create debian template directory
	#
	DEBFULLNAME='$(MAINTAINER_NAME)' && DEBEMAIL='<$(MAINTAINER_EMAIL)>' \
	&& export DEBFULLNAME && export DEBEMAIL \
	&& dh_make --createorig -s -p $(BIN_NAME)_$(APP_VERSION) -n -c gpl3 -y
	#
	# Create .install and .dirs files
	#
	touch debian/$(BIN_NAME).install
	echo "usr/bin/$(BIN_NAME)" >> debian/$(BIN_NAME).install

	touch debian/$(BIN_NAME).dirs
	echo "usr/bin" >> debian/$(BIN_NAME).dirs
	#
	# Copy bin build directory
	#
	mkdir -p $(BUILD_DIRECTORY)/usr/bin/
	stack --local-bin-path=$(BUILD_DIRECTORY)/usr/bin/ install $(PACKAGE_NAME)
  #
	# Create control file
	#
	mkdir $(BUILD_DIRECTORY)/debian
	touch $(BUILD_DIRECTORY)/debian/control
	#
	# Add content to control file
	#
	echo "Source: $(BIN_NAME) \
	 \nSection: text \
	 \nPriority: optional \
	 \nMaintainer: $(MAINTAINER_NAME) <$(MAINTAINER_EMAIL)> \
	 \nBuild-Depends: libyaml-0-2 \
	 \nStandards-Version: $(APP_VERSION) \
	 \n\nPackage: $(BIN_NAME) \
	 \nArchitecture: any \
	 \nDepends: ${shlibs:Depends}, ${misc:Depends} \
	 \nHomepage: $(SOURCE_URL) \
	 \nDescription: $(PACKAGE_DESCRIPTION) \
	  \n $(EXTENDED_PACKAGE_DESCRIPTION)" \
	 > $(BUILD_DIRECTORY)/debian/control
	#
	# Copy debian info files
	#
	cp debian/copyright $(BUILD_DIRECTORY)/debian
	cp debian/changelog $(BUILD_DIRECTORY)/debian
	cp debian/rules $(BUILD_DIRECTORY)/debian
	cp debian/compat $(BUILD_DIRECTORY)/debian
	cp debian/$(BIN_NAME).dirs $(BUILD_DIRECTORY)/debian
	cp debian/$(BIN_NAME).install $(BUILD_DIRECTORY)/debian
	cp debian/manpage.1.ex $(BUILD_DIRECTORY)/debian
	#
	# Change content in copyright file
	#
	echo -e "Format: https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/ \
	\nUpstream-Name: $(BIN_NAME) \
	\nSource: $(SOURCE_URL) \
	\n\nFiles: debian/* \
	\nCopyright: $(YEAR) $(MAINTAINER_NAME) <$(MAINTAINER_EMAIL)> \
	\nLicense: GPL-3.0+ \
 	\nThis program is free software: you can redistribute it and/or modify \
 	\nit under the terms of the GNU General Public License as published by \
 	\nthe Free Software Foundation, either version 3 of the License, or \
 	\n(at your option) any later version. \
 	\n. \
 	\nThis package is distributed in the hope that it will be useful, \
 	\nbut WITHOUT ANY WARRANTY; without even the implied warranty of \
 	\nMERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the \
 	\nGNU General Public License for more details. \
 	\n. \
 	\nYou should have received a copy of the GNU General Public License \
 	\nalong with this program. If not, see <https://www.gnu.org/licenses/>. \
 	\n. \
 	\nOn Debian systems, the complete text of the GNU General \
 	\nPublic License version 3 can be found in "/usr/share/common-licenses/GPL-3"." \ \
	 > $(BUILD_DIRECTORY)/debian/copyright
	#
	# Build deb
	#
	cd $(BUILD_DIRECTORY) \
	&& dpkg-buildpackage -rfakeroot -b -us -uc -d
	#
	# Remove package directories
	#
	rm -rf $(BUILD_DIRECTORY)
	rm -rf debian
	#
	# Lint deb
	#
	lintian *.deb
