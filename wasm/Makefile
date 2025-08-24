SHELL := /bin/bash

INSTALL_DIR = $(shell pwd)
ECL_TO_RUN = ${INSTALL_DIR}/ecl-host/bin/ecl

SHARE_TARGETS = #maxima-ecl-combined/share/maxima/*/share/lapack/lapack-interface.fas maxima-ecl-combined/share/maxima/*/share/linearalgebra/linearalgebra.fas maxima-ecl-combined/share/maxima/*/share/hompack/hompack-interface.fas maxima-ecl-combined/share/maxima/*/share/odepack/dlsode-interface.fas maxima-ecl-combined/share/maxima/*/share/cobyla/cobyla-interface.fas maxima-ecl-combined/share/maxima/*/share/minpack/minpack-interface.fas maxima-ecl-combined/share/maxima/*/share/graphs/graphs.fas # maxima-ecl-combined/share/maxima/*/share/fftpack5/fftpack5-interface.fas

all: maxima-ecl-combined/maxima.html maxima-ecl-combined/maxima_thread.js maxima-ecl-combined/maxima.wasm maxima-ecl-combined/libecl.so maxima-ecl-combined/maxima-slatec.fas maxima-ecl-combined/maxima-quadpack.fas maxima-ecl-combined/maxima-plot.fas maxima-ecl-combined/maxima-solve.fas maxima-ecl-combined/maxima-poisson-series.fas maxima-ecl-combined/maxima-matrix-algebra.fas maxima-ecl-combined/maxima-number-theory.fas maxima-ecl-combined/maxima-limits.fas maxima-ecl-combined/maxima-integration.fas maxima-ecl-combined/share maxima-ecl-combined/share_files.js maxima-ecl-combined/mathjax maxima-ecl-combined/doc maxima-ecl-combined/favicon.ico ${SHARE_TARGETS}

maxima-ecl-combined/share: ${ECL_TO_RUN} ../maxima-code/tests/*
	cd ../maxima-code; ./configure --with-ecl=`pwd`/../wasm/ecl-host/bin/ecl --prefix=/ && $(MAKE) && rm -rf ../wasm/maxima-ecl-combined/share && $(MAKE) DESTDIR=`pwd`/../wasm/maxima-ecl-combined/ install && rm -rf src/binary-ecl/maxima

maxima-ecl-combined/share_files.js: maxima-ecl-combined/share ${SHARE_TARGETS}
	rm -f maxima-ecl-combined/share_files.js
	cd maxima-ecl-combined/; find share/ -type d -exec echo "FS.mkdir('{}');" >> share_files.js \;
	cd maxima-ecl-combined/; find share/ -type f -exec echo "FS.createLazyFile('/', '{}', '{}', true, false);" >> share_files.js \;

../maxima-code/src/binary-ecl/wasm32-emscripten/maxima.wasm: ${ECL_TO_RUN} ecl-emscripten ../maxima-code/src/*.lisp ../maxima-code/tests/* maxima-ecl-combined/share
	rm -rf ../maxima-code/src/maxima
	${ECL_TO_RUN} --norc --load build.lisp --eval '(cross-compile-maxima)' --eval "(ext:quit)"

../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/plot.fasb: ${ECL_TO_RUN} ecl-emscripten ../maxima-code/src/plot.lisp ../maxima-code/src/gnuplot_def.lisp ../maxima-code/src/xmaxima_def.lisp ../maxima-code/src/geomview_def.lisp ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima.wasm maxima-ecl-combined/libgnuplot.so
	${ECL_TO_RUN} --norc --load build.lisp --eval "(cross-compile-maxima-module \"maxima/plot\" :linker-libs \"-lgnuplot\" :linker-flags \"-L`pwd`/maxima-ecl-combined/\")" --eval "(ext:quit)"

../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/solve.fasb: ${ECL_TO_RUN} ecl-emscripten ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../maxima-code/src/solve.lisp ../maxima-code/src/psolve.lisp ../maxima-code/src/algsys.lisp ../maxima-code/src/sqrtdenest.lisp ../maxima-code/src/polyrz.lisp ../maxima-code/src/cpoly.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(cross-compile-maxima-module \"maxima/solve\")" --eval "(ext:quit)"

../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/poisson-series.fasb: ${ECL_TO_RUN} ecl-emscripten ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../maxima-code/src/pois2.lisp ../maxima-code/src/pois3.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(cross-compile-maxima-module \"maxima/poisson-series\")" --eval "(ext:quit)"

../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/matrix-algebra.fasb: ${ECL_TO_RUN} ecl-emscripten ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../maxima-code/src/mat.lisp ../maxima-code/src/linnew.lisp ../maxima-code/src/matrix.lisp ../maxima-code/src/sprdet.lisp ../maxima-code/src/newinv.lisp ../maxima-code/src/newdet.lisp ../maxima-code/src/mdot.lisp ../maxima-code/src/invert.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(cross-compile-maxima-module \"maxima/matrix-algebra\")" --eval "(ext:quit)"

../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/number-theory.fasb: ${ECL_TO_RUN} ecl-emscripten ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../maxima-code/src/numth.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(cross-compile-maxima-module \"maxima/number-theory\")" --eval "(ext:quit)"

../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/limits.fasb: ${ECL_TO_RUN} ecl-emscripten ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../maxima-code/src/tlimit.lisp ../maxima-code/src/limit.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(cross-compile-maxima-module \"maxima/limits\")" --eval "(ext:quit)"

../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/integration.fasb: ${ECL_TO_RUN} ecl-emscripten ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../maxima-code/src/sinint.lisp ../maxima-code/src/sin.lisp ../maxima-code/src/risch.lisp ../maxima-code/src/defint.lisp ../maxima-code/src/residu.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(cross-compile-maxima-module \"maxima/integration\")" --eval "(ext:quit)"

../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/slatec.fasb: ${ECL_TO_RUN} ecl-emscripten ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../maxima-code/src/numerical/*.lisp ../maxima-code/src/numerical/slatec/*.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval '(cross-compile-maxima-module "maxima/slatec")' --eval "(ext:quit)"

../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/quadpack.fasb: ${ECL_TO_RUN} ecl-emscripten ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../maxima-code/src/numerical/*.lisp ../maxima-code/src/numerical/slatec/*.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval '(cross-compile-maxima-module "maxima/quadpack")' --eval "(ext:quit)"

../maxima-code/share/lapack/lapack/cross/interface--all-systems.fasb: ${ECL_TO_RUN} ecl-emscripten ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../maxima-code/share/lapack/*.lisp ../maxima-code/share/lapack/blas/*.lisp ../maxima-code/share/lapack/lapack/*.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(push \"`pwd`/../maxima-code/share/lapack/\" asdf:*central-registry*)" --eval "(asdf:load-system \"maxima\")" --eval "(cross-compile-maxima-module \"lapack/interface\" :monolithic t)" --eval "(ext:quit)"

../maxima-code/share/linearalgebra/cross/linearalgebra--all-systems.fasb: ${ECL_TO_RUN} ecl-emscripten ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../maxima-code/share/linearalgebra/*.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(push \"`pwd`/../maxima-code/share/linearalgebra/\" asdf:*central-registry*)" --eval "(asdf:load-system \"maxima\")" --eval "(cross-compile-maxima-module \"linearalgebra\" :monolithic t)" --eval "(ext:quit)"

../maxima-code/share/hompack/hompack/cross/interface--all-systems.fasb: ${ECL_TO_RUN} ecl-emscripten ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../maxima-code/share/hompack/*.lisp ../maxima-code/share/hompack/lisp/*.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(push \"`pwd`/../maxima-code/share/hompack/\" asdf:*central-registry*)" --eval "(asdf:load-system \"maxima\")" --eval "(cross-compile-maxima-module \"hompack/interface\" :monolithic t)" --eval "(ext:quit)"

../maxima-code/share/odepack/dlsode/cross/interface--all-systems.fasb: ${ECL_TO_RUN} ecl-emscripten ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../maxima-code/share/odepack/*.lisp ../maxima-code/share/odepack/src/*.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(push \"`pwd`/../maxima-code/share/odepack/\" asdf:*central-registry*)" --eval "(asdf:load-system \"maxima\")" --eval "(cross-compile-maxima-module \"dlsode/interface\" :monolithic t)" --eval "(ext:quit)"

../maxima-code/share/fftpack5/fftpack5/cross/interface--all-systems.fasb: ${ECL_TO_RUN} ecl-emscripten ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../maxima-code/share/fftpack5/*.lisp ../maxima-code/share/fftpack5/lisp/*.lisp
	${ECL_TO_RUN} --norc --heap-size 2147483648 --load build.lisp --eval "(push \"`pwd`/../maxima-code/share/fftpack5/\" asdf:*central-registry*)" --eval "(asdf:load-system \"maxima\")" --eval "(cross-compile-maxima-module \"fftpack5/interface\" :monolithic t)" --eval "(ext:quit)"

../maxima-code/share/cobyla/cobyla/cross/interface--all-systems.fasb: ${ECL_TO_RUN} ecl-emscripten ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../maxima-code/share/cobyla/*.lisp ../maxima-code/share/cobyla/lisp/*.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(push \"`pwd`/../maxima-code/share/cobyla/\" asdf:*central-registry*)" --eval "(asdf:load-system \"maxima\")" --eval "(cross-compile-maxima-module \"cobyla/interface\" :monolithic t)" --eval "(cross-compile-maxima-module \"bf-cobyla/interface\" :monolithic t)" --eval "(ext:quit)"

../maxima-code/share/minpack/minpack/cross/interface--all-systems.fasb: ${ECL_TO_RUN} ecl-emscripten ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../maxima-code/share/minpack/*.lisp ../maxima-code/share/minpack/lisp/*.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(push \"`pwd`/../maxima-code/share/minpack/\" asdf:*central-registry*)" --eval "(asdf:load-system \"maxima\")" --eval "(cross-compile-maxima-module \"minpack/interface\" :monolithic t)" --eval "(ext:quit)"

../maxima-code/share/graphs/cross/graphs--all-systems.fasb: ${ECL_TO_RUN} ecl-emscripten ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/plot.fasb ../maxima-code/share/graphs/*.lisp
	${ECL_TO_RUN} --norc --load build.lisp --eval "(push \"`pwd`/../maxima-code/share/graphs/\" asdf:*central-registry*)" --eval "(asdf:load-system \"maxima\")" --eval "(cross-compile-maxima-module \"graphs\" :monolithic t)" --eval "(ext:quit)"

maxima-ecl-combined/maxima.wasm: ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima.wasm
	mkdir -p maxima-ecl-combined
	cp ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima.js maxima-ecl-combined/
	cp ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima.wasm maxima-ecl-combined/

maxima-ecl-combined/maxima-plot.fas: ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/plot.fasb
	mkdir -p maxima-ecl-combined
	cp ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/plot.fasb maxima-ecl-combined/maxima-plot.fas

maxima-ecl-combined/maxima-solve.fas: ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/solve.fasb
	mkdir -p maxima-ecl-combined
	cp ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/solve.fasb maxima-ecl-combined/maxima-solve.fas

maxima-ecl-combined/maxima-poisson-series.fas: ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/poisson-series.fasb
	mkdir -p maxima-ecl-combined
	cp ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/poisson-series.fasb maxima-ecl-combined/maxima-poisson-series.fas

maxima-ecl-combined/maxima-matrix-algebra.fas: ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/matrix-algebra.fasb
	mkdir -p maxima-ecl-combined
	cp ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/matrix-algebra.fasb maxima-ecl-combined/maxima-matrix-algebra.fas

maxima-ecl-combined/maxima-number-theory.fas: ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/number-theory.fasb
	mkdir -p maxima-ecl-combined
	cp ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/number-theory.fasb maxima-ecl-combined/maxima-number-theory.fas

maxima-ecl-combined/maxima-limits.fas: ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/limits.fasb
	mkdir -p maxima-ecl-combined
	cp ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/limits.fasb maxima-ecl-combined/maxima-limits.fas

maxima-ecl-combined/maxima-integration.fas: ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/integration.fasb
	mkdir -p maxima-ecl-combined
	cp ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/integration.fasb maxima-ecl-combined/maxima-integration.fas

maxima-ecl-combined/maxima-slatec.fas: ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/slatec.fasb
	mkdir -p maxima-ecl-combined
	cp ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/slatec.fasb maxima-ecl-combined/maxima-slatec.fas

maxima-ecl-combined/maxima-quadpack.fas: ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/quadpack.fasb
	mkdir -p maxima-ecl-combined
	cp ../maxima-code/src/binary-ecl/wasm32-emscripten/maxima/quadpack.fasb maxima-ecl-combined/maxima-quadpack.fas

maxima-ecl-combined/share/maxima/*/share/lapack/lapack-interface.fas: ../maxima-code/share/lapack/lapack/cross/interface--all-systems.fasb
	mkdir -p maxima-ecl-combined
	cp ../maxima-code/share/lapack/lapack/cross/interface--all-systems.fasb maxima-ecl-combined/share/maxima/`ls -1 maxima-ecl-combined/share/maxima/`/share/lapack/lapack-interface.fas

maxima-ecl-combined/share/maxima/*/share/linearalgebra/linearalgebra.fas: ../maxima-code/share/linearalgebra/cross/linearalgebra--all-systems.fasb
	mkdir -p maxima-ecl-combined
	cp ../maxima-code/share/linearalgebra/cross/linearalgebra--all-systems.fasb maxima-ecl-combined/share/maxima/`ls -1 maxima-ecl-combined/share/maxima/`/share/linearalgebra/linearalgebra.fas

maxima-ecl-combined/share/maxima/*/share/hompack/hompack-interface.fas: ../maxima-code/share/hompack/hompack/cross/interface--all-systems.fasb
	mkdir -p maxima-ecl-combined
	cp ../maxima-code/share/hompack/hompack/cross/interface--all-systems.fasb maxima-ecl-combined/share/maxima/`ls -1 maxima-ecl-combined/share/maxima/`/share/hompack/hompack-interface.fas

maxima-ecl-combined/share/maxima/*/share/odepack/dlsode-interface.fas: ../maxima-code/share/odepack/dlsode/cross/interface--all-systems.fasb
	mkdir -p maxima-ecl-combined
	cp ../maxima-code/share/odepack/dlsode/cross/interface--all-systems.fasb maxima-ecl-combined/share/maxima/`ls -1 maxima-ecl-combined/share/maxima/`/share/odepack/dlsode-interface.fas

maxima-ecl-combined/share/maxima/*/share/fftpack5/fftpack5-interface.fas: ../maxima-code/share/fftpack5/fftpack5/cross/interface--all-systems.fasb
	mkdir -p maxima-ecl-combined
	cp ../maxima-code/share/fftpack5/fftpack5/cross/interface--all-systems.fasb maxima-ecl-combined/share/maxima/`ls -1 maxima-ecl-combined/share/maxima/`/share/fftpack5/fftpack5-interface.fas

maxima-ecl-combined/share/maxima/*/share/cobyla/cobyla-interface.fas: ../maxima-code/share/cobyla/cobyla/cross/interface--all-systems.fasb
	mkdir -p maxima-ecl-combined
	cp ../maxima-code/share/cobyla/cobyla/cross/interface--all-systems.fasb maxima-ecl-combined/share/maxima/`ls -1 maxima-ecl-combined/share/maxima/`/share/cobyla/cobyla-interface.fas
	cp ../maxima-code/share/cobyla/bf-cobyla/cross/interface--all-systems.fasb maxima-ecl-combined/share/maxima/`ls -1 maxima-ecl-combined/share/maxima/`/share/cobyla/bf-cobyla-interface.fas

maxima-ecl-combined/share/maxima/*/share/minpack/minpack-interface.fas: ../maxima-code/share/minpack/minpack/cross/interface--all-systems.fasb
	mkdir -p maxima-ecl-combined
	cp ../maxima-code/share/minpack/minpack/cross/interface--all-systems.fasb maxima-ecl-combined/share/maxima/`ls -1 maxima-ecl-combined/share/maxima/`/share/minpack/minpack-interface.fas

maxima-ecl-combined/share/maxima/*/share/graphs/graphs.fas: ../maxima-code/share/graphs/cross/graphs--all-systems.fasb
	mkdir -p maxima-ecl-combined
	cp ../maxima-code/share/graphs/cross/graphs--all-systems.fasb maxima-ecl-combined/share/maxima/`ls -1 maxima-ecl-combined/share/maxima/`/share/graphs/graphs.fas

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
	rm -rf ../maxima-code/share/lapack/cross ../maxima-code/share/lapack/lapack/cross ../maxima-code/share/lapack/blas/cross ../maxima-code/share/lapack/*.fas ../maxima-code/share/lapack/*.o ../maxima-code/share/lapack/*.c ../maxima-code/share/lapack/*.eclh ../maxima-code/share/lapack/*.data ../maxima-code/share/lapack/blas/*.fas ../maxima-code/share/lapack/blas/*.o ../maxima-code/share/lapack/blas/*.c ../maxima-code/share/lapack/blas/*.eclh ../maxima-code/share/lapack/blas/*.data ../maxima-code/share/lapack/lapack/*.fas ../maxima-code/share/lapack/lapack/*.o ../maxima-code/share/lapack/lapack/*.c ../maxima-code/share/lapack/lapack/*.eclh ../maxima-code/share/lapack/lapack/*.data ../maxima-code/share/lapack/lapack.fasb ../maxima-code/share/linearalgebra/cross ../maxima-code/share/graphs/cross ../maxima-code/share/minpack/cross ../maxima-code/share/minpack/*.o ../maxima-code/share/minpack/*.c ../maxima-code/share/minpack/*.data ../maxima-code/share/minpack/*.eclh ../maxima-code/share/minpack/*.fas ../maxima-code/share/minpack/lisp/*.o ../maxima-code/share/minpack/lisp/*.c ../maxima-code/share/minpack/lisp/*.data ../maxima-code/share/minpack/lisp/*.eclh ../maxima-code/share/minpack/lisp/*.fas ../maxima-code/share/minpack/lisp/cross ../maxima-code/share/cobyla/cross ../maxima-code/share/cobyla/lisp/cross ../maxima-code/share/cobyla/lisp/*.o ../maxima-code/share/cobyla/lisp/*.c ../maxima-code/share/cobyla/lisp/*.data ../maxima-code/share/cobyla/lisp/*.eclh ../maxima-code/share/cobyla/lisp/*.fas ../maxima-code/share/cobyla/*.o ../maxima-code/share/cobyla/*.c ../maxima-code/share/cobyla/*.data ../maxima-code/share/cobyla/*.eclh ../maxima-code/share/cobyla/*.fas ../maxima-code/share/fftpack5/cross ../maxima-code/share/fftpack5/lisp/cross ../maxima-code/share/fftpack5/*.o ../maxima-code/share/fftpack5/*.c ../maxima-code/share/fftpack5/*.data ../maxima-code/share/fftpack5/*.eclh ../maxima-code/share/fftpack5/*.fas ../maxima-code/share/fftpack5/lisp/*.o ../maxima-code/share/fftpack5/lisp/*.c ../maxima-code/share/fftpack5/lisp/*.data ../maxima-code/share/fftpack5/lisp/*.eclh ../maxima-code/share/fftpack5/lisp/*.fas ../maxima-code/share/odepack/*.o ../maxima-code/share/odepack/*.c ../maxima-code/share/odepack/*.data ../maxima-code/share/odepack/*.eclh ../maxima-code/share/odepack/*.fas ../maxima-code/share/odepack/cross ../maxima-code/share/odepack/src/cross ../maxima-code/share/odepack/src/*.o ../maxima-code/share/odepack/src/*.c ../maxima-code/share/odepack/src/*.data ../maxima-code/share/odepack/src/*.eclh ../maxima-code/share/odepack/src/*.fas ../maxima-code/share/hompack/cross ../maxima-code/share/hompack/lisp/*.o ../maxima-code/share/hompack/lisp/*.c ../maxima-code/share/hompack/lisp/*.data ../maxima-code/share/hompack/lisp/*.eclh ../maxima-code/share/hompack/lisp/*.fas ../maxima-code/share/hompack/lisp/cross ../maxima-code/share/hompack/hompack ../maxima-code/share/odepack/dlsode ../maxima-code/share/minpack/minpack ../maxima-code/share/cobyla/cobyla ../maxima-code/share/cobyla/bf-cobyla

clean-maxima:
	rm -rf ${INSTALL_DIR}/maxima-ecl-combined ../maxima-code/src/binary-ecl ../maxima-code/src/*.c  ../maxima-code/src/*.eclh  ../maxima-code/src/*.data ../maxima-code/src/*.o ../maxima-code/src/*.fas ../maxima-code/src/*.fasb ../maxima-code/src/*.a ../maxima-code/src/numerical/*.c ../maxima-code/src/numerical/*.eclh  ../maxima-code/src/numerical/*.data ../maxima-code/src/numerical/*.o ../maxima-code/src/numerical/*.fas ../maxima-code/src/numerical/slatec/*.c ../maxima-code/src/numerical/slatec/*.eclh  ../maxima-code/src/numerical/slatec/*.data ../maxima-code/src/numerical/slatec/*.o ../maxima-code/src/numerical/slatec/*.fas

clean: clean-host-ecl clean-target-ecl clean-gnuplot clean-maxima clean-share
