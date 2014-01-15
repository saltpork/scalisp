#!/bin/sh
# -*- mode: sh -*-

COMMIT=`git log -1 HEAD | head -1 | awk '{ print $2 }'`
DATE=`date yyyy-mm-dd--HH-MM-SS`

mkdir -p logs
echo Running $COMMIT on $DATE
./mk > logs/$COMMIT-bench-$DATE.log

