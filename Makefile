# Environment
BIN_NAME := ff

LOCAL_BINARY_PATH = $(shell stack path --local-install-root)

LOCAL_FF_VERSION = $(shell stack query locals ff version | tr -d \')

LOCAL_FF_SIZE = $(shell ls -l --b=K $(LOCAL_BINARY_PATH)/bin/$(BIN_NAME) | cut -d " " -f5 | tr -d K)

# prepare deb
.PHONY: deb
deb:
	rm -rf ff-deb

	mkdir -p ff-deb/
	mkdir -p ff-deb/usr/bin
	cp $(LOCAL_BINARY_PATH)/bin/$(BIN_NAME) ff-deb/usr/bin/

	mkdir ff-deb/DEBIAN
	touch ff-deb/DEBIAN/control

	echo \
	"Package: ff \
	\nVersion: $(LOCAL_FF_VERSION) \
	\nArchitecture: all \
	\nMaintainer: Yuriy Syrovetskiy  \
	\nInstalled-Size: $(LOCAL_FF_SIZE) \
	\nSection: text \
	\nPriority: optional  \
	\nHomepage: https://github.com/ff-notes/ff \
	\nDescription: A distributed note taker and task manager." \
	>> ff-deb/DEBIAN/control

	cp LICENSE ff-deb/DEBIAN/copyright

	cd ff-deb && dpkg-deb -b ./ ./
