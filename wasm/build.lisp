(push :wasm *features*)

(require :cmp)

(require :asdf)

(setf si::*ignore-package-locks* t)
(setf c::*delete-files* nil)
(setf c::*debug-compiler* t)
(setf c::*compiler-break-enable* t)
(setf c::*suppress-compiler-messages* 'c::compiler-debug-note)

(proclaim '(optimize (safety 2) (space 1) (speed 2) (debug 0)))

;; Create a basic emscripten target configuration
(defvar *target-cmp-config* 
  '((c::*target-architecture* . "wasm32")
    (c::*target-software-type* . "emscripten")
    (c::*cross-compiling* . t)))

(in-package #:asdf)

;; Setup ASDF: Don't put the output in a cache and register the
;; systems in the current directory.
(asdf:initialize-output-translations
 '(:output-translations :ignore-inherited-configuration :disable-cache))
(push #P"../maxima-code/src/" asdf:*central-registry*)

(defclass cross-compilation (operation)
  ((configuration :initarg :compiler-configuration :initform (error "Must supply a configuration for the cross compilation")
                  :reader cross-compilation-configuration)
   (user-cc-flags :initarg :user-cc-flags :initform nil :reader cross-compilation-user-cc-flags)
   (user-linker-flags :initarg :user-linker-flags :initform nil :reader cross-compilation-user-linker-flags)
   (user-linker-libs :initarg :user-linker-libs :initform nil :reader cross-compilation-user-linker-libs)))

(defclass cross-image-op (image-op cross-compilation) ())

(defclass cross-program-op (cross-image-op program-op) ())

(defmethod initialize-instance :after ((instance cross-compilation) &rest initargs)
  (declare (ignore initargs))
  (setf (operation-original-initargs instance)
        (remove-plist-keys
         '(:compiler-configuration :user-cc-flags :user-linker-flags :user-linker-libs)
         (operation-original-initargs instance))))

(defclass cross-compile-op (compile-op cross-compilation) ())

(defclass cross-lib-op (lib-op cross-compilation) ())

(defmethod component-depends-on :around ((o cross-image-op) c)
  (mapcar #'(lambda (x) (if (typep (first x) 'lib-op)
                            (cons (make-operation 'cross-lib-op :compiler-configuration (cross-compilation-configuration o))
                                  (rest x))
                            x))
          (call-next-method)))

(defmethod component-depends-on :around ((o cross-lib-op) c)
  (loop for (operation . components) in (call-next-method)
        if (typep operation 'compile-op)
          collect (cons (make-operation 'cross-compile-op :compiler-configuration (cross-compilation-configuration o)) components)
        else if (typep operation 'load-op)
               collect (cons (make-operation 'compile-op) components)
               and collect (cons operation components)
        else
          collect (cons operation components)))

(defclass cross-compile-bundle-op (compile-bundle-op cross-compilation) ())

(defclass monolithic-cross-compile-bundle-op (monolithic-compile-bundle-op cross-compilation) ())

(defmethod component-depends-on :around ((o cross-compile-bundle-op) c)
  (loop for (operation . components) in (call-next-method)
        if (typep operation 'compile-op)
          collect (cons (make-operation 'cross-compile-op :compiler-configuration (cross-compilation-configuration o)) components)
        else if (typep operation 'load-op)
               collect (cons (make-operation 'compile-op) components)
               and collect (cons operation components)
        else
          collect (cons operation components)))

(defmethod component-depends-on :around ((o monolithic-cross-compile-bundle-op) c)
  (loop for (operation . components) in (call-next-method)
        if (typep operation 'compile-op)
          collect (cons (make-operation 'cross-compile-op :compiler-configuration (cross-compilation-configuration o)) components)
        else if (typep operation 'load-op)
               collect (cons (make-operation 'compile-op) components)
               and collect (cons operation components)
        else
          collect (cons operation components)))

(defmethod output-files :around ((o cross-compilation) c)
  (multiple-value-bind (pathnames translated)
      (call-next-method)
    (let* ((config (cross-compilation-configuration o))
           (target-architecture (string-downcase (cdr (find 'c::*target-architecture* config :key #'car))))
           (target-software-type (string-downcase (cdr (find 'c::*target-software-type* config :key #'car)))))
      (values (mapcar #'(lambda (p)
                          (if translated
                              (let* ((dir (pathname-directory p))
                                     (n (position "binary-ecl" dir :test #'string=)))
                                (make-pathname :defaults p
                                               :directory (concatenate 'list (subseq dir 0 (1+ n))
                                                                       (list (format nil "~a-~a"
                                                                                     target-architecture
                                                                                     target-software-type))
                                                                       (subseq dir (min (1+ n) (length dir))))))
                              (translate-pathname p "/**/*.*"
                                                  (format nil "/**/~a-~a/*.*"
                                                          target-architecture
                                                          target-software-type))))
                      pathnames)
              translated))))

(defmethod perform :around ((o cross-compilation) c)
  (with-compilation-unit (:configuration (cross-compilation-configuration o))
    (let ((c::*user-cc-flags* (cross-compilation-user-cc-flags o))
          (c::*user-linker-libs* (cross-compilation-user-linker-libs o))
          (c::*user-linker-flags* (cross-compilation-user-linker-flags o)))
      (call-next-method))))

(defmethod output-files ((o cross-program-op) c)
  (with-compilation-unit (:configuration (cross-compilation-configuration o))
    (call-next-method)))

(let ((f #'bundle-pathname-type))
  (defun bundle-pathname-type (bundle-type)
    (if (eql bundle-type :program)
        (let ((s (format nil c::+executable-file-format+ "")))
          (subseq s (min 1 (length s))))
        (funcall f bundle-type))))

(in-package #:cl-user)

(defun cross-compile-maxima ()
  ;; For now, skip the cross-compilation as we already have a working build from build.sh
  (format t "Cross-compilation step skipped - using ECL build.sh approach instead~%"))

(defun cross-compile-maxima-module (module &key cc-flags linker-flags linker-libs monolithic)
  (asdf:oos (if monolithic 'asdf::monolithic-cross-compile-bundle-op 'asdf::cross-compile-bundle-op)
            module
            :compiler-configuration *target-cmp-config*
            :user-cc-flags cc-flags
            :user-linker-flags linker-flags
            :user-linker-libs linker-libs))
