#!/bin/bash

echo -e
echo "======================================="
echo "|        Pull project from git        |"
echo "======================================="
echo -e

git fetch --all
git reset --hard origin/master
git pull

chmod 777 -R *.sh