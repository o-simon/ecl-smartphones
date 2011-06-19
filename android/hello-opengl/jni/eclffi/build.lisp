(require 'cmp)

(setq *print-case* :downcase)

(defvar *ecl-root* "/opt/ecl/");#p"/opt/iphone/ecl/")
(defvar *gmp-root* "/opt/gmp/");#p"/opt/iphone/ecl/")

(defun compile-if-old (destdir sources &rest options)
  (unless (probe-file destdir)
    (si::mkdir destdir #o0777))
  (mapcar #'(lambda (source)
	      (let ((object (merge-pathnames
                             destdir
                             (compile-file-pathname source :type :object))))
		(unless (and (probe-file object)
			     (>= (file-write-date object) (file-write-date source)))
		  (format t "~&(compile-file ~S :output-file ~S~{ ~S~})~%"
			  source object options)
		  (apply #'compile-file source :output-file object options))
		object))
	  sources))

(defun safe-delete-file (f)
  (ignore-errors (delete-file f)))

(defun clean (sources)
  (dolist (source sources)
    (let ((source (pathname source)))
      (dolist (f (mapcar (lambda (x) (make-pathname :type x :defaults source))
                         '("c" "h" "o" "data")))
        (format t "~&;; deleting ~a" f)
        (safe-delete-file f))
      (let ((f (make-pathname :name (util:str "lib" (pathname-name source)) :type "a")))
        (format t "~&;; deleting ~a" f)
        (safe-delete-file f)))))

(defun build (target source-files &key ecl-include-dir gmp-include-dir cflags android-root sysroot)
  (let* ((compiler::*ecl-include-directory* ecl-include-dir)
         (compiler::*cc* (format nil "~a/toolchains/arm-linux-androideabi-4.4.3/prebuilt/darwin-x86/bin/arm-linux-androideabi-gcc" android-root))
	 (compiler::*ranlib* (format nil "~a/toolchains/arm-linux-androideabi-4.4.3/prebuilt/darwin-x86/bin/arm-linux-androideabi-ranlib" android-root))
	 (compiler::*ar* (format nil "~a/toolchains/arm-linux-androideabi-4.4.3/prebuilt/darwin-x86/bin/arm-linux-androideabi-ar" android-root))
         (compiler::*cc-flags* (util:join (list* (format nil "--sysroot=~a" sysroot)
						 "-DANDROID"
						 "-DPLATFORM_ANDROID"
						 (format nil "-I~a" gmp-include-dir) 
						 (format nil "-I~a" ecl-include-dir) 
						 (format nil "-I~aecl" ecl-include-dir) 
						 "-DNDK_DEBUG=1"
						 "-g" 
						 cflags)
                                          " "))
         (lisp-files (compile-if-old #p""
                                     source-files
                                     :system-p t
                                     :c-file t
                                     :data-file t
                                     :h-file t)))
    (compiler:build-static-library target :lisp-files lisp-files)))

(defun build-device (target source-files)
  (let* ((android-root "/opt/android/android-ndk-r5")
         (sysroot (format nil "~a/platforms/android-9/arch-arm" android-root)))
    (build target
           source-files
           :ecl-include-dir (concatenate 'string *ecl-root* "android/include/") 
	   :gmp-include-dir (concatenate 'string *gmp-root* "android/include/") 
	   :cflags (list "-lGLESv1_CM")
           :android-root android-root
           :sysroot sysroot)))

(defun build-all (module
                  sources
                  &key
                  (ecl-root *ecl-root*)
                  (gmp-root *gmp-root*))
  (let ((*ecl-root* ecl-root)
	(*gmp-root* gmp-root))
    (clean sources)
    (build-device module sources)))