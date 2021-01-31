#!/bin/bash

export CONNECT_FOUR_WEB_PORT=5123
export CONNECT_FOUR_WEB_URL="http://localhost:$CONNECT_FOUR_WEB_PORT"

(cd ./server; stack run) &
api=$!

(cd ./client; npm run serve) &
client=$!

wait $api $client
