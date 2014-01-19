#!/bin/bash -x
set -e

OPTIONS='-S-optimise -S-target:jvm-1.7 -S-deprecation' # -S-Yinline-warnings
SCALAC="zinc -nailed -scala-home /usr/local/scala/lib -cp /usr/local/scala/lib/jline.jar"

#OPTIONS="-optimise -target:jvm-1.7 -deprecation"
#SCALAC=scalac

if [[ "$1" == "repl" ]]; then
    scala -cp scalisp.jar scalisp.REPL
elif  [[ "$1" == "clean" ]]; then
    rm -rf build build-bench scalisp.jar scalisp-bench.jar
elif  [[ "$1" == "profile" ]]; then
    scala -cp $CP:scalisp.jar:scalisp-bench.jar scabench.Profile
else
    mkdir -p build
    # build

    $SCALAC $OPTIONS -d build src/*.scala 
    jar cf scalisp.jar -C build scalisp
    
    # build bench
    CP="lib/scalameter_2.10-0.4.jar"
    $SCALAC $OPTIONS -cp $CP:scalisp.jar -d build-bench bench/*.scala 
    jar cf scalisp-bench.jar -C build-bench scabench

    # run tests and bench
    scala -cp scalisp.jar scalisp.Tests
    scala -cp $CP:scalisp.jar:scalisp-bench.jar scabench.DispatchPerformance
fi
