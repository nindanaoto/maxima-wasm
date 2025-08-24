SHELL := /bin/bash

INSTALL_DIR = $(shell pwd)
ECL_TO_RUN = ${INSTALL_DIR}/ecl-host/bin/ecl

SHARE_TARGETS = #maxima-ecl-combined/share/maxima/*/share/lapack/lapack-interface.fas maxima-ecl-combined/share/maxima/*/share/linearalgebra/linearalgebra.fas maxima-ecl-combined/share/maxima/*/share/hompack/hompack-interface.fas maxima-ecl-combined/share/maxima/*/share/odepack/dlsode-interface.fas maxima-ecl-combined/share/maxima/*/share/cobyla/cobyla-interface.fas maxima-ecl-combined/share/maxima/*/share/minpack/minpack-interface.fas maxima-ecl-combined/share/maxima/*/share/graphs/graphs.fas # maxima-ecl-combined/share/maxima/*/share/fftpack5/fftpack5-interface.fas

all: maxima-ecl-combined/maxima.html maxima-ecl-combined/maxima_thread.js maxima-ecl-combined/maxima.wasm maxima-ecl-combined/libecl.so maxima-ecl-combined/maxima-slatec.fas maxima-ecl-combined/maxima-quadpack.fas maxima-ecl-combined/maxima-plot.fas maxima-ecl-combined/maxima-solve.fas maxima-ecl-combined/maxima-poisson-series.fas maxima-ecl-combined/maxima-matrix-algebra.fas maxima-ecl-combined/maxima-number-theory.fas maxima-ecl-combined/maxima-limits.fas maxima-ecl-combined/maxima-integration.fas maxima-ecl-combined/share maxima-ecl-combined/share_files.js maxima-ecl-combined/mathjax maxima-ecl-combined/doc maxima-ecl-combined/favicon.ico ${SHARE_TARGETS}

maxima-ecl-combined/share: ${ECL_TO_RUN} ../tests/*
	cd ..; ./configure --with-ecl=`pwd`/wasm/ecl-host/bin/ecl --prefix=/ && $(MAKE) && rm -rf maxima-ecl-combined/share && $(MAKE) DESTDIR=`pwd`/wasm/maxima-ecl-combined/ install && rm -rf src/binary-ecl/maxima

maxima-ecl-combined/share_files.js: maxima-ecl-combined/share ${SHARE_TARGETS}
	rm -f maxima-ecl-combined/share_files.js
	cd maxima-ecl-combined/; find share/ -type d -exec echo "FS.mkdir('{}');" >> share_files.js \;
	cd maxima-ecl-combined/; find share/ -type f -exec echo "FS.createLazyFile('/', '{}', '{}', true, false);" >> share_files.js \;

../src/binary-ecl/wasm32-emscripten/maxima.wasm: ${ECL_TO_RUN} ecl-emscripten ../src/*.lisp ../tests/* maxima-ecl-combined/share
	rm -rf ../src/maxima
	${ECL_TO_RUN} --norc --load build.lisp --eval '(cross-compile-maxima)' --eval "(ext:quit)"

../src/binary-ecl/wasm32-emscripten/maxima/plot.fasb: ${ECL_TO_RUN} ecl-emscripten ../src/plot.lisp ../src/gnuplot_def.lisp ../src/xmaxima_def.lisp ../src/geomview_def.lisp ../src/binary-ecl/wasm32-emscripten/maxima.wasm maxima-ecl-combined/libgnuplot.so
	${ECL_TO_RUN} --norc --load build.lisp --eval "(cross-compile-maxima-module \"maxima/plot\" :linker-libs \"-lgnuplot\" :linker-flags \"-L`pwd`/maxima-ecl-combined/\")" --eval "(ext:quit)"

../src/binary-ecl/wasm32-emscripten/maxima/solve.fasb: ${ECL_TO_RUN} ecl-emscripten ../src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../src/solve.lisp ../src/psolve.lisp ../src/algsys.lisp ../src/sqrtdenest.lisp ../src/polyrz.lisp ../src/cpoly.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(cross-compile-maxima-module \"maxima/solve\")" --eval "(ext:quit)"

../src/binary-ecl/wasm32-emscripten/maxima/poisson-series.fasb: ${ECL_TO_RUN} ecl-emscripten ../src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../src/pois2.lisp ../src/pois3.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(cross-compile-maxima-module \"maxima/poisson-series\")" --eval "(ext:quit)"

../src/binary-ecl/wasm32-emscripten/maxima/matrix-algebra.fasb: ${ECL_TO_RUN} ecl-emscripten ../src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../src/mat.lisp ../src/linnew.lisp ../src/matrix.lisp ../src/sprdet.lisp ../src/newinv.lisp ../src/newdet.lisp ../src/mdot.lisp ../src/invert.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(cross-compile-maxima-module \"maxima/matrix-algebra\")" --eval "(ext:quit)"

../src/binary-ecl/wasm32-emscripten/maxima/number-theory.fasb: ${ECL_TO_RUN} ecl-emscripten ../src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../src/numth.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(cross-compile-maxima-module \"maxima/number-theory\")" --eval "(ext:quit)"

../src/binary-ecl/wasm32-emscripten/maxima/limits.fasb: ${ECL_TO_RUN} ecl-emscripten ../src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../src/tlimit.lisp ../src/limit.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(cross-compile-maxima-module \"maxima/limits\")" --eval "(ext:quit)"

../src/binary-ecl/wasm32-emscripten/maxima/integration.fasb: ${ECL_TO_RUN} ecl-emscripten ../src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../src/sinint.lisp ../src/sin.lisp ../src/risch.lisp ../src/defint.lisp ../src/residu.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(cross-compile-maxima-module \"maxima/integration\")" --eval "(ext:quit)"

../src/binary-ecl/wasm32-emscripten/maxima/slatec.fasb: ${ECL_TO_RUN} ecl-emscripten ../src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../src/numerical/*.lisp ../src/numerical/slatec/*.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval '(cross-compile-maxima-module "maxima/slatec")' --eval "(ext:quit)"

../src/binary-ecl/wasm32-emscripten/maxima/quadpack.fasb: ${ECL_TO_RUN} ecl-emscripten ../src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../src/numerical/*.lisp ../src/numerical/slatec/*.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval '(cross-compile-maxima-module "maxima/quadpack")' --eval "(ext:quit)"

../share/lapack/lapack/cross/interface--all-systems.fasb: ${ECL_TO_RUN} ecl-emscripten ../src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../share/lapack/*.lisp ../share/lapack/blas/*.lisp ../share/lapack/lapack/*.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(push \"`pwd`/../share/lapack/\" asdf:*central-registry*)" --eval "(asdf:load-system \"maxima\")" --eval "(cross-compile-maxima-module \"lapack/interface\" :monolithic t)" --eval "(ext:quit)"

../share/linearalgebra/cross/linearalgebra--all-systems.fasb: ${ECL_TO_RUN} ecl-emscripten ../src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../share/linearalgebra/*.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(push \"`pwd`/../share/linearalgebra/\" asdf:*central-registry*)" --eval "(asdf:load-system \"maxima\")" --eval "(cross-compile-maxima-module \"linearalgebra\" :monolithic t)" --eval "(ext:quit)"

../share/hompack/hompack/cross/interface--all-systems.fasb: ${ECL_TO_RUN} ecl-emscripten ../src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../share/hompack/*.lisp ../share/hompack/lisp/*.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(push \"`pwd`/../share/hompack/\" asdf:*central-registry*)" --eval "(asdf:load-system \"maxima\")" --eval "(cross-compile-maxima-module \"hompack/interface\" :monolithic t)" --eval "(ext:quit)"

../share/odepack/dlsode/cross/interface--all-systems.fasb: ${ECL_TO_RUN} ecl-emscripten ../src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../share/odepack/*.lisp ../share/odepack/src/*.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(push \"`pwd`/../share/odepack/\" asdf:*central-registry*)" --eval "(asdf:load-system \"maxima\")" --eval "(cross-compile-maxima-module \"dlsode/interface\" :monolithic t)" --eval "(ext:quit)"

../share/fftpack5/fftpack5/cross/interface--all-systems.fasb: ${ECL_TO_RUN} ecl-emscripten ../src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../share/fftpack5/*.lisp ../share/fftpack5/lisp/*.lisp
	${ECL_TO_RUN} --norc --heap-size 2147483648 --load build.lisp --eval "(push \"`pwd`/../share/fftpack5/\" asdf:*central-registry*)" --eval "(asdf:load-system \"maxima\")" --eval "(cross-compile-maxima-module \"fftpack5/interface\" :monolithic t)" --eval "(ext:quit)"

../share/cobyla/cobyla/cross/interface--all-systems.fasb: ${ECL_TO_RUN} ecl-emscripten ../src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../share/cobyla/*.lisp ../share/cobyla/lisp/*.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(push \"`pwd`/../share/cobyla/\" asdf:*central-registry*)" --eval "(asdf:load-system \"maxima\")" --eval "(cross-compile-maxima-module \"cobyla/interface\" :monolithic t)" --eval "(cross-compile-maxima-module \"bf-cobyla/interface\" :monolithic t)" --eval "(ext:quit)"

../share/minpack/minpack/cross/interface--all-systems.fasb: ${ECL_TO_RUN} ecl-emscripten ../src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../share/minpack/*.lisp ../share/minpack/lisp/*.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(push \"`pwd`/../share/minpack/\" asdf:*central-registry*)" --eval "(asdf:load-system \"maxima\")" --eval "(cross-compile-maxima-module \"minpack/interface\" :monolithic t)" --eval "(ext:quit)"

../share/graphs/cross/graphs--all-systems.fasb: ${ECL_TO_RUN} ecl-emscripten ../src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../share/graphs/*.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(push \"`pwd`/../share/graphs/\" asdf:*central-registry*)" --eval "(asdf:load-system \"maxima\")" --eval "(cross-compile-maxima-module \"graphs\" :monolithic t)" --eval "(ext:quit)"

maxima-ecl-combined/maxima.wasm: ../src/binary-ecl/wasm32-emscripten/maxima.wasm
	mkdir -p maxima-ecl-combined
	cp ../src/binary-ecl/wasm32-emscripten/maxima.js maxima-ecl-combined/
	cp ../src/binary-ecl/wasm32-emscripten/maxima.wasm maxima-ecl-combined/

maxima-ecl-combined/maxima-plot.fas: ../src/binary-ecl/wasm32-emscripten/maxima/plot.fasb
	mkdir -p maxima-ecl-combined
	cp ../src/binary-ecl/wasm32-emscripten/maxima/plot.fasb maxima-ecl-combined/maxima-plot.fas

maxima-ecl-combined/maxima-solve.fas: ../src/binary-ecl/wasm32-emscripten/maxima/solve.fasb
	mkdir -p maxima-ecl-combined
	cp ../src/binary-ecl/wasm32-emscripten/maxima/solve.fasb maxima-ecl-combined/maxima-solve.fas

maxima-ecl-combined/maxima-poisson-series.fas: ../src/binary-ecl/wasm32-emscripten/maxima/poisson-series.fasb
	mkdir -p maxima-ecl-combined
	cp ../src/binary-ecl/wasm32-emscripten/maxima/poisson-series.fasb maxima-ecl-combined/maxima-poisson-series.fas

maxima-ecl-combined/maxima-matrix-algebra.fas: ../src/binary-ecl/wasm32-emscripten/maxima/matrix-algebra.fasb
	mkdir -p maxima-ecl-combined
	cp ../src/binary-ecl/wasm32-emscripten/maxima/matrix-algebra.fasb maxima-ecl-combined/maxima-matrix-algebra.fas

maxima-ecl-combined/maxima-number-theory.fas: ../src/binary-ecl/wasm32-emscripten/maxima/number-theory.fasb
	mkdir -p maxima-ecl-combined
	cp ../src/binary-ecl/wasm32-emscripten/maxima/number-theory.fasb maxima-ecl-combined/maxima-number-theory.fas

maxima-ecl-combined/maxima-limits.fas: ../src/binary-ecl/wasm32-emscripten/maxima/limits.fasb
	mkdir -p maxima-ecl-combined
	cp ../src/binary-ecl/wasm32-emscripten/maxima/limits.fasb maxima-ecl-combined/maxima-limits.fas

maxima-ecl-combined/maxima-integration.fas: ../src/binary-ecl/wasm32-emscripten/maxima/integration.fasb
	mkdir -p maxima-ecl-combined
	cp ../src/binary-ecl/wasm32-emscripten/maxima/integration.fasb maxima-ecl-combined/maxima-integration.fas

maxima-ecl-combined/maxima-slatec.fas: ../src/binary-ecl/wasm32-emscripten/maxima/slatec.fasb
	mkdir -p maxima-ecl-combined
	cp ../src/binary-ecl/wasm32-emscripten/maxima/slatec.fasb maxima-ecl-combined/maxima-slatec.fas

maxima-ecl-combined/maxima-quadpack.fas: ../src/binary-ecl/wasm32-emscripten/maxima/quadpack.fasb
	mkdir -p maxima-ecl-combined
	cp ../src/binary-ecl/wasm32-emscripten/maxima/quadpack.fasb maxima-ecl-combined/maxima-quadpack.fas

maxima-ecl-combined/share/maxima/*/share/lapack/lapack-interface.fas: ../share/lapack/lapack/cross/interface--all-systems.fasb
	mkdir -p maxima-ecl-combined
	cp ../share/lapack/lapack/cross/interface--all-systems.fasb maxima-ecl-combined/share/maxima/`ls -1 maxima-ecl-combined/share/maxima/`/share/lapack/lapack-interface.fas

maxima-ecl-combined/share/maxima/*/share/linearalgebra/linearalgebra.fas: ../share/linearalgebra/cross/linearalgebra--all-systems.fasb
	mkdir -p maxima-ecl-combined
	cp ../share/linearalgebra/cross/linearalgebra--all-systems.fasb maxima-ecl-combined/share/maxima/`ls -1 maxima-ecl-combined/share/maxima/`/share/linearalgebra/linearalgebra.fas

maxima-ecl-combined/share/maxima/*/share/hompack/hompack-interface.fas: ../share/hompack/hompack/cross/interface--all-systems.fasb
	mkdir -p maxima-ecl-combined
	cp ../share/hompack/hompack/cross/interface--all-systems.fasb maxima-ecl-combined/share/maxima/`ls -1 maxima-ecl-combined/share/maxima/`/share/hompack/hompack-interface.fas

maxima-ecl-combined/share/maxima/*/share/odepack/dlsode-interface.fas: ../share/odepack/dlsode/cross/interface--all-systems.fasb
	mkdir -p maxima-ecl-combined
	cp ../share/odepack/dlsode/cross/interface--all-systems.fasb maxima-ecl-combined/share/maxima/`ls -1 maxima-ecl-combined/share/maxima/`/share/odepack/dlsode-interface.fas

maxima-ecl-combined/share/maxima/*/share/fftpack5/fftpack5-interface.fas: ../share/fftpack5/fftpack5/cross/interface--all-systems.fasb
	mkdir -p maxima-ecl-combined
	cp ../share/fftpack5/fftpack5/cross/interface--all-systems.fasb maxima-ecl-combined/share/maxima/`ls -1 maxima-ecl-combined/share/maxima/`/share/fftpack5/fftpack5-interface.fas

maxima-ecl-combined/share/maxima/*/share/cobyla/cobyla-interface.fas: ../share/cobyla/cobyla/cross/interface--all-systems.fasb
	mkdir -p maxima-ecl-combined
	cp ../share/cobyla/cobyla/cross/interface--all-systems.fasb maxima-ecl-combined/share/maxima/`ls -1 maxima-ecl-combined/share/maxima/`/share/cobyla/cobyla-interface.fas
	cp ../share/cobyla/bf-cobyla/cross/interface--all-systems.fasb maxima-ecl-combined/share/maxima/`ls -1 maxima-ecl-combined/share/maxima/`/share/cobyla/bf-cobyla-interface.fas

maxima-ecl-combined/share/maxima/*/share/minpack/minpack-interface.fas: ../share/minpack/minpack/cross/interface--all-systems.fasb
	mkdir -p maxima-ecl-combined
	cp ../share/minpack/minpack/cross/interface--all-systems.fasb maxima-ecl-combined/share/maxima/`ls -1 maxima-ecl-combined/share/maxima/`/share/minpack/minpack-interface.fas

maxima-ecl-combined/share/maxima/*/share/graphs/graphs.fas: ../share/graphs/cross/graphs--all-systems.fasb
	mkdir -p maxima-ecl-combined
	cp ../share/graphs/cross/graphs--all-systems.fasb maxima-ecl-combined/share/maxima/`ls -1 maxima-ecl-combined/share/maxima/`/share/graphs/graphs.fas

maxima-ecl-combined/libecl.so: ecl-emscripten
	mkdir -p maxima-ecl-combined
	cp -r ecl-emscripten/libecl.so ecl-emscripten/*.fas ecl-emscripten/*.asd ecl-emscripten/encodings ecl-emscripten/help.doc maxima-ecl-combined/

maxima-ecl-combined/maxima.html: maxima.html
	mkdir -p maxima-ecl-combined
	cp maxima.html maxima-ecl-combined/

maxima-ecl-combined/index.html: maxima-ecl-combined/maxima.html
	cd maxima-ecl-combined/; rm index.html; ln -s maxima.html index.html

maxima-ecl-combined/maxima_thread.js: maxima_thread.js
	mkdir -p maxima-ecl-combined
	cp maxima_thread.js maxima-ecl-combined/

ecl-emscripten: ${ECL_TO_RUN}
	rm -rf ${ECL_SRCDIR}/build/
	cd ${ECL_SRCDIR}; source ${EMSDK_PATH}/emsdk_env.sh; ECL_TO_RUN=${ECL_TO_RUN} CFLAGS="-O0 -DECLDIR='' -Wno-macro-redefined" LDFLAGS="-sINITIAL_MEMORY=134217728 -sALLOW_MEMORY_GROWTH -sSTACK_OVERFLOW_CHECK=0 -sEXPORTED_RUNTIME_METHODS=dynCall" emconfigure ./configure --host=wasm32-unknown-emscripten --build=x86_64-pc-linux-gnu --with-cross-config=${ECL_SRCDIR}/src/util/wasm32-unknown-emscripten.cross_config --prefix=${INSTALL_DIR}/ecl-emscripten --with-tcp=no --with-cmp=no --with-bytecmp=builtin --with-asdf=no && emmake $(MAKE) $(MAKEFLAGS) && rm -rf ${INSTALL_DIR}/ecl-emscripten && emmake $(MAKE) install && cp build/bin/ecl.js build/bin/ecl.html build/bin/ecl.wasm* build/libecl.so* ${INSTALL_DIR}/ecl-emscripten/

${ECL_TO_RUN}:
	rm -rf ${ECL_SRCDIR}/build/
	cd ${ECL_SRCDIR}; ./configure ABI=32 CFLAGS="-g -m32 -O2 -DECL_C_COMPATIBLE_VARIADIC_DISPATCH" LDFLAGS="-g -m32 -O2" --prefix=${INSTALL_DIR}/ecl-host --disable-threads
	cd ${ECL_SRCDIR}; $(MAKE) && rm -rf ${INSTALL_DIR}/ecl-host && $(MAKE) install

maxima-ecl-combined/libgnuplot.so:
	cd ${GNUPLOT_SRCDIR}; make clean; rm -rf ${INSTALL_DIR}/gnuplot/; source ${EMSDK_PATH}/emsdk_env.sh; CFLAGS="-DLIBGNUPLOT -UREADLINE" emconfigure ./configure --without-libcerf --without-latex --without-readline --without-lua --without-tektronix --without-cairo --without-qt --disable-history-file --disable-plugins --disable-wxwidgets --without-x --without-kitty --without-gd --prefix=${INSTALL_DIR}/gnuplot && emmake $(MAKE) $(MAKEFLAGS) && emmake $(MAKE) install && cp ${INSTALL_DIR}/gnuplot/lib/libgnuplot.so.0.0.0 ${INSTALL_DIR}/maxima-ecl-combined/libgnuplot.so

maxima-ecl-combined/mathjax: mathjax/es5
	cp -r mathjax/es5 maxima-ecl-combined/mathjax

maxima-ecl-combined/doc: maxima-ecl-combined/share
	cd maxima-ecl-combined/; rm doc; ln -s `find share -name doc` .

maxima-ecl-combined/favicon.ico: maxima-ecl-combined/share
	cd maxima-ecl-combined/; rm favicon.ico; ln -s `find share -name favicon.ico` .

clean-host-ecl:
	rm -rf ${INSTALL_DIR}/ecl-host

clean-target-ecl:
	rm -rf ${INSTALL_DIR}/ecl-emscripten

clean-gnuplot:
	rm -rf ${INSTALL_DIR}/gnuplot ${INSTALL_DIR}/maxima-ecl-combined/libgnuplot.so

clean-share:
	rm -rf ../share/lapack/cross ../share/lapack/lapack/cross ../share/lapack/blas/cross ../share/lapack/*.fas ../share/lapack/*.o ../share/lapack/*.c ../share/lapack/*.eclh ../share/lapack/*.data ../share/lapack/blas/*.fas ../share/lapack/blas/*.o ../share/lapack/blas/*.c ../share/lapack/blas/*.eclh ../share/lapack/blas/*.data ../share/lapack/lapack/*.fas ../share/lapack/lapack/*.o ../share/lapack/lapack/*.c ../share/lapack/lapack/*.eclh ../share/lapack/lapack/*.data ../share/lapack/lapack.fasb ../share/linearalgebra/cross ../share/graphs/cross ../share/minpack/cross ../share/minpack/*.o ../share/minpack/*.c ../share/minpack/*.data ../share/minpack/*.eclh ../share/minpack/*.fas ../share/minpack/lisp/*.o ../share/minpack/lisp/*.c ../share/minpack/lisp/*.data ../share/minpack/lisp/*.eclh ../share/minpack/lisp/*.fas ../share/minpack/lisp/cross ../share/cobyla/cross ../share/cobyla/lisp/cross ../share/cobyla/lisp/*.o ../share/cobyla/lisp/*.c ../share/cobyla/lisp/*.data ../share/cobyla/lisp/*.eclh ../share/cobyla/lisp/*.fas ../share/cobyla/*.o ../share/cobyla/*.c ../share/cobyla/*.data ../share/cobyla/*.eclh ../share/cobyla/*.fas ../share/fftpack5/cross ../share/fftpack5/lisp/cross ../share/fftpack5/*.o ../share/fftpack5/*.c ../share/fftpack5/*.data ../share/fftpack5/*.eclh ../share/fftpack5/*.fas ../share/fftpack5/lisp/*.o ../share/fftpack5/lisp/*.c ../share/fftpack5/lisp/*.data ../share/fftpack5/lisp/*.eclh ../share/fftpack5/lisp/*.fas ../share/odepack/*.o ../share/odepack/*.c ../share/odepack/*.data ../share/odepack/*.eclh ../share/odepack/*.fas ../share/odepack/cross ../share/odepack/src/cross ../share/odepack/src/*.o ../share/odepack/src/*.c ../share/odepack/src/*.data ../share/odepack/src/*.eclh ../share/odepack/src/*.fas ../share/hompack/cross ../share/hompack/lisp/*.o ../share/hompack/lisp/*.c ../share/hompack/lisp/*.data ../share/hompack/lisp/*.eclh ../share/hompack/lisp/*.fas ../share/hompack/lisp/cross ../share/hompack/hompack ../share/odepack/dlsode ../share/minpack/minpack ../share/cobyla/cobyla ../share/cobyla/bf-cobyla

clean-maxima:
	rm -rf ${INSTALL_DIR}/maxima-ecl-combined ../src/binary-ecl ../src/*.c  ../src/*.eclh  ../src/*.data ../src/*.o ../src/*.fas ../src/*.fasb ../src/*.a ../src/numerical/*.c ../src/numerical/*.eclh  ../src/numerical/*.data ../src/numerical/*.o ../src/numerical/*.fas ../src/numerical/slatec/*.c ../src/numerical/slatec/*.eclh  ../src/numerical/slatec/*.data ../src/numerical/slatec/*.o ../src/numerical/slatec/*.fas

clean: clean-host-ecl clean-target-ecl clean-gnuplot clean-maxima clean-share
