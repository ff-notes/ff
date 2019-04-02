#!/bin/bash
set -eux -o pipefail

docker build --tag ff-ubuntu-16.04 dockers/ff-ubuntu-16.04
docker build --tag ff-ubuntu-18.04 dockers/ff-ubuntu-18.04
