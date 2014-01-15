#!/bin/sh
# -*- mode: sh -*-

COMMIT=`git log -1 HEAD | head -1 | awk '{ print $2 }'`

mkdir -p logs
echo Running $COMMIT
./mk > logs/$COMMIT-bench.log

