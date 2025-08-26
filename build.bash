#!/bin/bash
git sumodule update --init --recursive
./emsdk/emsdk install latest
./emsdk/emsdk activate latest
cd maxima-code
boostrap.sh
cd ../gnuplot
autoconf
cd ../wasm
make -j
