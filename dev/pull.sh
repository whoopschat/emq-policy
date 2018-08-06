#!/bin/bash

ROOT_DIR=..
cd ${ROOT_DIR}

echo -e
echo "======================================="
echo "|        Pull project from git        |"
echo "======================================="
echo -e

git fetch --all
git reset --hard origin/master
git pull

chmod 777 -R *.sh