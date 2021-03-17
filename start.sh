#!/bin/bash

export CONNECT_FOUR_WEB_PORT=5123
export CONNECT_FOUR_WEB_URL="http://localhost/api"
export CONNECT_FOUR_SOCKET_URL="ws://localhost:5123"
export CONNECT_FOUR_BASE_PATH="connect-four"

(cd ./server; stack run) &
api=$!

(cd ./client; npm run serve) &
client=$!

docker run --rm --name dev-nginx --net="host" -v $PWD/nginx.conf:/etc/nginx/nginx.conf:ro nginx:alpine &
proxy=$!

wait $api $client $proxy
