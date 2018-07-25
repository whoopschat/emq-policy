#!/bin/bash

echo -e
echo "======================================="
echo "|        Push project from git        |"
echo "======================================="
echo -e

git add .
git status -s
git commit -m 'release build'
git push origin master