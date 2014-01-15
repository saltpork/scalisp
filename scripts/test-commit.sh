#!/bin/sh
# -*- mode: sh -*-

COMMIT=`git log -1 HEAD | head -1 | awk '{ print $2 }'`
DATE=`date +%F--%T`

mkdir -p logs
echo Running $COMMIT on $DATE
./mk > logs/$COMMIT-bench-$DATE.log

