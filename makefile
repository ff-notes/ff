
# BUILD_DIRECTORY ?= ff-deb

GID := $(shell id -g)
UID := $(shell id -u)

build:
	EULER_GID=${GID} EULER_UID=${UID} docker-compose -p ff-deb -f dockers/debianizer/docker-compose.yml up -d --build

rebuild: build-stop
	EULER_GID=${GID} EULER_UID=${UID} docker-compose -p ff-deb -f dockers/debianizer/docker-compose.yml up -d --build

build-stop:
	EULER_GID=${GID} EULER_UID=${UID} docker-compose -p ff-deb -f dockers/debianizer/docker-compose.yml down

run:
	docker exec -ti ff-deb_ff_1 bash

stop:
	docker stop ff-deb_ff_1
