#!/bin/bash

ROOT_DIR=..
EMQ_DIR=${ROOT_DIR}/build/emq-relx

echo -e
echo "======================================="
echo "|          Start EMQ Server           |"
echo "======================================="
echo -e

${EMQ_DIR}/_rel/emqttd/bin/emqttd start
