#!/bin/bash
set -eux -o pipefail

docker build --tag ff-deb dockers/cradle-for-deb
