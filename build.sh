#!/bin/bash

BUILD_DIR=$(pwd)/build
DATA_DIR=$(pwd)/data
EMQ_DIR=$BUILD_DIR/emq-relx
EMQ_REPO_GIT=https://github.com/emqtt/emq-relx
EMQ_REPO_GIT_VERSION=2.3.5

if [ -d $BUILD_DIR ]; then
echo ""
else
echo ""
mkdir $BUILD_DIR
fi

echo " ================> Pull emq-relx from git"

if [ -d $EMQ_DIR ]; then
cd $EMQ_DIR
git pull
else
cd $BUILD_DIR
git clone -b release $EMQ_REPO_GIT
fi

echo " ================> Replacement Makefile configuration"

cd $(pwd)
cp $DATA_DIR/Makefile $EMQ_DIR/Makefile


echo " ================> Make"
cd $EMQ_DIR

# make


