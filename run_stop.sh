#!/bin/bash

CURRENT_DIR=$(pwd)
BUILD_DIR=${CURRENT_DIR}/build
EMQ_DIR=${BUILD_DIR}/emq-relx

echo -e
echo "======================================="
echo "|          Stop EMQ Server           |"
echo "======================================="
echo -e

${EMQ_DIR}/_rel/emqttd/bin/emqttd stop
