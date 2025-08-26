#!/bin/bash
git submodule update --init --recursive
./emsdk/emsdk install latest
./emsdk/emsdk activate latest
cd maxima-code
./bootstrap
cd ../gnuplot
autoconf
cd ../wasm
make -j
