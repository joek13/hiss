#!/bin/bash

# Figure out where we are running and identify playground project root.
root=$(realpath $(dirname $0)/../)

# TODO: locate binaries in CI environment
hissvm="$root/hissvm"
hissc="$root/hissc"

# Create directory for Lambda bundle
mkdir -p $root/build/bundle/
cp -R $root/src/* $root/build/bundle
cp "$hissvm" "$root/build/bundle/hissvm"
cp "$hissc" "$root/build/bundle/hissc"
cd $root/build/bundle

# Create the Lambda bundle
zip -r ../bundle.zip .
echo "Wrote Lambda bundle"