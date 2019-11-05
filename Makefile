#
# Environment
#
PACKAGE_NAME=ff

BUILD_DIRECTORY=ff-deb

#
# pack deb
#
.PHONY: deb
deb:
	#
	# Remove recent package stuff
	rm -rf $(BUILD_DIRECTORY)
	mkdir -p $(BUILD_DIRECTORY)
	stack clean
	#
	# Build bin and copy to build directory
	#
	mkdir -p $(BUILD_DIRECTORY)/usr/bin/
	stack --local-bin-path=$(BUILD_DIRECTORY)/usr/bin/ install $(PACKAGE_NAME)
  #
	# Copy debian info files
	#
	cp -r debian/ $(BUILD_DIRECTORY)/
	cp -r $(BUILD_DIRECTORY)/debian/docs $(BUILD_DIRECTORY)/
	#
	# Build deb
	#
	cd $(BUILD_DIRECTORY) \
	&& dpkg-buildpackage -rfakeroot -b -us -uc -d
	#
	# Remove package directories
	#
	rm -rf $(BUILD_DIRECTORY)
	#
	# Lint deb
	#
	lintian *.deb

# rm *.deb *.buildinfo *.changes