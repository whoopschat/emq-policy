#!/bin/bash

ROOT_DIR=$(pwd)/..
EMQ_DIR=${ROOT_DIR}/build/emq-relx

echo -e
echo "======================================="
echo "|         Console EMQ Server          |"
echo "======================================="
echo -e

${EMQ_DIR}/_rel/emqttd/bin/emqttd console
