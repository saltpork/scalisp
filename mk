#!/bin/bash -x

set -e

if [[ "$1" == "repl" ]]; then
    scala -cp scalisp.jar scalisp.REPL
else
    mkdir -p build
    scalac -d build src/*.scala 
    jar cf scalisp.jar -C build scalisp
    scala -cp scalisp.jar scalisp.Tests
fi
