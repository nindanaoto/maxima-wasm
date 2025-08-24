#!/bin/bash
set -e
source ${EMSDK_PATH}/emsdk_env.sh
maxima_files=$(${ECL_TO_RUN} --norc --quiet \
			     --eval "(let ((*standard-output* (make-broadcast-stream)))
                        (require :asdf))" \
			     --eval "(pushnew :wasm *features*)" \
			     --eval "(defclass find-files (asdf:downward-operation) ())" \
			     --eval "(defmethod asdf:perform ((op find-files) (c asdf:cl-source-file))
			               (princ (namestring (asdf::component-pathname c)))
                        (princ #\Space))" \
			     --eval "(push \"`pwd`/../maxima-code/src/\" asdf:*central-registry*)" \
			     --eval "(asdf:operate (asdf:make-operation 'find-files) :maxima)" \
			     --eval "(ext:quit)")
rm -rf ${ECL_SRCDIR}/build/
cd ${ECL_SRCDIR}
LDFLAGS="-sINITIAL_MEMORY=67108864 -sALLOW_MEMORY_GROWTH" emconfigure ./configure \
       --host=wasm32-unknown-emscripten \
       --build=x86_64-pc-linux-gnu \
       --with-cross-config=`pwd`/src/util/wasm32-unknown-emscripten.cross_config \
       --prefix=${INSTALL_DIR}/maxima-ecl-combined \
       --with-tcp=no \
       --with-cmp=no \
       --with-bytecmp=builtin \
       --with-asdf=no \
       --with-extra-files="${maxima_files}" \
       --with-init-form="(progn (ext:install-bytecodes-compiler) (cl-user::run))"
emmake make
rm -rf ${INSTALL_DIR}/maxima-ecl-combined
emmake make install
cp build/bin/ecl.js build/bin/ecl.wasm ${INSTALL_DIR}/maxima-ecl-combined/
