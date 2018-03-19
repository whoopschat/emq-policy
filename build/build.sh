#!/bin/bash

CURRENT_FOLDER=$(pwd)

# emq-relx
EMQ_REL_X_REPO=$CURRENT_FOLDER/emq-relx
EMQ_REL_X_REPO_GIT=https://github.com/emqtt/emq-relx

echo "Pull emq-relx SDK from git"

if [ -d $EMQ_REL_X_REPO ]; then
cd $EMQ_REL_X_REPO
git pull
else
git clone $EMQ_REL_X_REPO_GIT
fi