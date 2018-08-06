#!/bin/bash

ROOT_DIR=$(pwd)/..
cd ${ROOT_DIR}

echo -e
echo "======================================="
echo "|        Push project from git        |"
echo "======================================="
echo -e

git config --global user.name 'whoopschat'
git config --global user.email whoopschat@qq.com

git add .
git status -s
git commit -m 'release build'
git push origin master