#!/bin/bash
ROOT_DIR=$(pwd)/../
VER=v2.3.11

BIN_DIR=${ROOT_DIR}/bin
BUILD_DIR=${ROOT_DIR}/build
DATA_DIR=${ROOT_DIR}/data
EMQ_DIR=${BUILD_DIR}/emq-relx
EMQ_DEP_DIR=${EMQ_DIR}/deps

EMQ_REL_X_REPO_GIT=https://github.com/emqtt/emq-relx
EMQ_REL_X_REPO_GIT_BRANCH=${VER}

EMQ_RELEASE_DIR=${EMQ_DIR}/_rel
EMQ_RELEASE_TARGET=${BIN_DIR}/release_emq_${VER}.zip

if [ ! -d ${BUILD_DIR} ]; then
mkdir ${BUILD_DIR}
fi

if [ -d ${EMQ_RELEASE_DIR} ]; then
rm -rf ${EMQ_RELEASE_DIR}
fi

if [ -d ${EMQ_RELEASE_TARGET} ]; then
rm -rf ${EMQ_RELEASE_TARGET}
fi

if [ -d ${EMQ_DEP_DIR} ]; then
rm -rf ${EMQ_DEP_DIR}
fi

echo -e
echo "========================================"
echo "|        Pull emq-relx from git        |"
echo "========================================"
echo "| emqttd version:${VER}"
echo "========================================"
echo -e

if [ -d ${EMQ_DIR} ]; then
cd ${EMQ_DIR}
git fetch --all
git reset --hard
else
cd ${BUILD_DIR}
git clone ${EMQ_REL_X_REPO_GIT}
fi

cd ${EMQ_DIR}
git pull
git checkout -b ${EMQ_REL_X_REPO_GIT_BRANCH}

echo -e
echo "======================================="
echo "|               Building              |"
echo "======================================="
echo -e

cd ${ROOT_DIR}
cp ${DATA_DIR}/Makefile ${EMQ_DIR}/Makefile
cp ${DATA_DIR}/relx.config ${EMQ_DIR}/relx.config
cd ${EMQ_DIR}
make VER=${VER}

echo -e
echo "======================================="
echo "|            Compression              |"
echo "======================================="
echo -e

if [ ! -d ${EMQ_RELEASE_DIR} ]; then
echo -e
echo "Failure"
else
cd ${EMQ_RELEASE_DIR}
zip -r -q -o ${EMQ_RELEASE_TARGET}  ./
echo "Success , release path : ${EMQ_RELEASE_TARGET}"
fi
echo -e

