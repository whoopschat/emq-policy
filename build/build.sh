#!/bin/bash

# emq-relx
EMQ_REL_X_REPO=./emq-relx
EMQ_REL_X_GIT=https://github.com/emqtt/emq-relx

echo "Pull emq-relx SDK from git"
CURRENT_FOLDER=$(pwd)

if [ -d $EMQ_REL_X_REPO ]; then
cd $EMQ_REL_X_REPO
git pull
else
cd ..
git clone $EMQ_REL_X_GIT
fi