#!/bin/bash

CURRENT_DIR=$(pwd)
BUILD_DIR=${CURRENT_DIR}/build
DATA_DIR=${CURRENT_DIR}/data
EMQ_DIR=${BUILD_DIR}/emq-relx
EMQ_REPO_GIT=https://github.com/emqtt/emq-relx
EMQ_REPO_GIT_VERSION=2.3.5
EMQ_BUILD_DIR=${EMQ_DIR}/_rel/emqttd

if [ ! -d ${BUILD_DIR} ]; then
mkdir ${BUILD_DIR}
fi

if [ -d ${EMQ_BUILD_DIR} ]; then
rm -rf ${EMQ_BUILD_DIR}
fi

echo "======================================="
echo "|       Pull emq-relx from git        |"
echo "=======================================\n"

if [ -d ${EMQ_DIR} ]; then
cd ${EMQ_DIR}
git pull
else
cd ${BUILD_DIR}
git clone -b release ${EMQ_REPO_GIT}
fi

echo "======================================="
echo "|               Building              |"
echo "=======================================\n"
cd ${CURRENT_DIR}
cp ${DATA_DIR}/Makefile ${EMQ_DIR}/Makefile
cp ${DATA_DIR}/relx.config ${EMQ_DIR}/relx.config
cd ${EMQ_DIR}
rm -rf ${EMQ_DIR}/deps/emp_policy_server
make

echo "======================================="
echo "|            Compression              |"
echo "=======================================\n"
zip -r -q -o ${BUILD_DIR}/emqttd.zip  ${EMQ_BUILD_DIR}

