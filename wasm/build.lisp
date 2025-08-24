(push :wasm *features*)

(require :cmp)

(require :asdf)

(setf si::*ignore-package-locks* t)
(setf c::*delete-files* nil)
(setf c::*debug-compiler* t)
(setf c::*compiler-break-enable* t)
(setf c::*suppress-compiler-messages* 'c::compiler-debug-note)

(proclaim '(optimize (safety 2) (space 1) (speed 2) (debug 0)))

(defvar *target-cmp-config* (c:read-compiler-configuration "ecl-emscripten/compiler-configuration"))

(in-package #:asdf)

;; Setup ASDF: Don't put the output in a cache and register the
;; systems in the current directory.
(asdf:initialize-output-translations
 (let* ((path (butlast (pathname-directory (ext:getcwd))))
	(src-dir (make-pathname :defaults (ext:getcwd) :directory (append path '("src"))))
	(share-dir (make-pathname :defaults (ext:getcwd) :directory (append path '("share")))))
   `(:output-translations
     :ignore-inherited-configuration
     :disable-cache
     (,(make-pathname :defaults (merge-pathnames "**/*.*" src-dir) :version nil)
      ,(make-pathname :defaults (merge-pathnames "binary-ecl/**/*.*" src-dir) :version nil))
     (,(make-pathname :defaults (merge-pathnames "**/*.*" share-dir) :version nil)
      ,(make-pathname :defaults (merge-pathnames "binary-ecl/share/**/*.*" src-dir) :version nil)))))
(push #P"../src/" asdf:*central-registry*)

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

;; (defmethod gather-op ((o cross-image-op))
;;   (format t "gather-op for cross-image-op~%")
;;   (make-operation 'cross-lib-op :compiler-configuration (cross-compilation-configuration o)))

;; (defmethod initialize-instance :after ((o cross-image-op) &rest initargs)
;;   (declare (ignore initargs))
;;   (format t "initialize-instance for cross-image-op~%")
;;   (setf (slot-value o 'gather-op)
;;      ))

(defclass cross-compile-op (compile-op cross-compilation) ())

(defclass cross-lib-op (lib-op cross-compilation) ())

(defmethod component-depends-on :around ((o cross-image-op) c)
  ;; (format t "~&component-depends-on: ~a ~a~%" o c)
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

;; (defclass cross-link-op (link-op cross-compilation) ())

;; (defclass cross-lib-op (cross-link-op gather-op non-propagating-operation)
;;   ((gather-type :initform :object :allocation :class)
;;    (bundle-type :initform :lib :allocation :class)))

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
  (asdf:oos 'asdf::cross-program-op "maxima"
            :compiler-configuration *target-cmp-config*
            :epilogue-code '(progn (ext:install-bytecodes-compiler)
                             (cl-user::run))))

(defun cross-compile-maxima-module (module &key cc-flags linker-flags linker-libs monolithic)
  (asdf:oos (if monolithic 'asdf::monolithic-cross-compile-bundle-op 'asdf::cross-compile-bundle-op)
            module
            :compiler-configuration *target-cmp-config*
            :user-cc-flags cc-flags
            :user-linker-flags linker-flags
            :user-linker-libs linker-libs))


;; (defclass compile-host-op (asdf:compile-op) ())
;; (defmethod asdf:output-files ((o compile-host-op) (c asdf:cl-source-file))
;;   (break)
;;   (multiple-value-bind (pathnames translated)
;;       (call-next-method)
;;     (values (mapcar #'(lambda (p) (translate-pathname p "/**/*.*" "/**/host/*.*")) pathnames)
;;             translated)))
;; (defmethod asdf:perform ((o compile-host-op) (c asdf:cl-source-file))
;;   (break)
;;   (with-cmp-config *host-cmp-config*
;;     (format t "~&;;; Compiling with host config~%")
;;     (call-next-method))
;;   (let ((p (first (asdf:output-files o c))))
;;     (rename-file (translate-pathname p "/**/host/*.*" "/**/*.*") p :if-exists :overwrite)))
;; (defclass load-host-op (asdf::basic-load-op asdf:selfward-operation)
;;   ((asdf:selfward-operation :initform '(asdf:prepare-op compile-host-op) :allocation :class)))

;; (defmacro cross-compile (config &body body)
;;   `(with-cmp-config ,config
;;      (format t "~&;;; Cross compiling~%")
;;      (let ((asdf:*load-system-operation* 'load-host-op))
;;        (format t "asdf:*load-system-operation*: ~a~%" asdf:*load-system-operation*)
;;        ,@body)))

;; (defun cross-compile-maxima ()
;;   (cross-compile *target-cmp-config*
;;     (asdf:make-build "maxima"
;;                   :type :program
;;                   :move-here #P"./"
;;                   :epilogue-code '(progn (ext:install-bytecodes-compiler)
;;                                          (cl-user::run)))))
