#!/bin/bash

set -e

docker build -t connect-four-web-build .

mkdir -p ./bin
stack --docker install --flag connect-four-web:static --local-bin-path ./bin
