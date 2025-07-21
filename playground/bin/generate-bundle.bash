#!/bin/bash

# Figure out where we are running and identify playground project root.
root=$(realpath $(dirname $0)/../)

# TODO: identify hissc, hissvm executables

# Create directory for Lambda bundle
mkdir -p $root/build/bundle/
cp -R $root/src/* $root/build/bundle
cd $root/build/bundle

# Create the Lambda bundle
zip -r ../bundle.zip .
echo "Wrote Lambda bundle"