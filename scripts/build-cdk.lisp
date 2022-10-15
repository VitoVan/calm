;; this file is for building CDK, and is intended to be load inside a bash-like environment
;;
;; tested on Windows with MSYS2 shell,
;; on Linux and macOS, it should be working without any surprise.
;;
;; please load this file inside the root directory of CALM (where `calm.asd` exists),
;; this SBCL instance should be the one installed on your machine instead of `./sbcl`,
;; since before CDK was built, `./sbcl` may not be ready.
;;
;; sbcl --load build-cdk.lisp
;;
(ql:quickload '(:cffi :str))

(defmacro echo (control-string &rest rest)
  `(format t (str:concat ,control-string "~%") ,@rest))

(defmacro exec (control-string &rest rest)
  `(let ((cmd (format nil ,control-string ,@rest)))
     (echo "EXECUTING: ~A~%" cmd)
     (uiop:run-program cmd :output *standard-output* :error-output *error-output*)))

(unless (uiop:probe-file* "./calm.asd")
  (echo "Please load this file in the root directory of CALM (where `calm.asd` exists).")
  (uiop:quit 42))

(defun copy-deps ()
  (mapcar
   #'(lambda (lib)
       (let* ((lib-pathname (cffi:foreign-library-pathname lib))
              (lib-path-namestring (namestring lib-pathname)))
         (unless (str:starts-with-p "/System" lib-path-namestring)
           (exec "cp ~A  ./cdk/lib/calm/" ;; this has to be `cp`, since the `uiop:copy-file` works weirdly on MSYS2
                 (namestring
                  #+linux
                  (uiop:merge-pathnames* lib-pathname "/usr/lib64/")
                  #+darwin
                  (uiop:merge-pathnames* lib-pathname "/usr/local/lib/")
                  #+win32
                  (uiop:merge-pathnames* lib-pathname "/mingw64/bin/"))))))
   (cffi:list-foreign-libraries :loaded-only t)))

(defun otool-l (dylib)
  (loop for otool-result in (str:lines (uiop:run-program (format nil "otool -L ~A" dylib)
                                                         :error-output *error-output*
                                                         :output '(:string :stripped t)))
        when
        (let ((load-path (car (str:split " " (str:trim otool-result)))))
          (when (and (str:starts-with-p "/usr/local" load-path) (not (str:containsp ":" load-path)))
            load-path))
        collect it))

(defun config-deps ()
  (echo "Configuring dependencies ...")
  (uiop:with-current-directory ("./cdk/lib/calm/")
    (echo "Current working directory: ~A" (uiop:getcwd))
    (echo "Copying all dependencies' dependencies ...")
    (dotimes (i 42)
      (dolist (dylib (uiop:directory-files "./"))
        (dolist (load-path (otool-l dylib))
          (unless (uiop:probe-file* (file-namestring load-path))
            (exec "cp -n ~A ./" load-path)))))
    (exec "chmod +w *.dylib")

    (echo "Modifying LOADER_PATH & RPATH ...")
    (dolist (dylib (uiop:directory-files "./"))
      (exec "install_name_tool -add_rpath @loader_path/. ~A" dylib)
      (exec "install_name_tool -id @rpath/~A ~A" (file-namestring dylib) dylib)
      (dolist (load-path (otool-l dylib))
        (exec "install_name_tool -change ~A @rpath/~A ~A" load-path (file-namestring load-path) dylib)))
    (echo "Fixing libSDL2.dylib ...")
    (exec "rm libSDL2.dylib")
    (exec "ln -s ~A libSDL2.dylib" (file-namestring (car (uiop:directory* "libSDL2-*.dylib")))))
  (echo "Current working directory: ~A" (uiop:getcwd)))

(echo "Cleaning old files ...")

(exec "rm -rf ./install_root")

(exec "rm -rf ./cdk")

(echo "Building macOS CDK")

(echo "Downloading pre-built SBCL (--with-fancy --with-compression) ...")

(unless (uiop:probe-file* "./install_root-macOS.zip")
  (exec "curl -OL https://github.com/VitoVan/sbcl-with-compression/releases/download/test-2.2.9-09/install_root-macOS.zip"))

(exec "unzip install_root-macOS.zip")

(exec "mv ./install_root ./cdk")

(echo "Installing Quicklisp ...")

(exec "curl -o ./cdk/quicklisp.lisp -L https://beta.quicklisp.org/quicklisp.lisp")

(exec "./sbcl --load ./cdk/quicklisp.lisp --eval '(quicklisp-quickstart:install :proxy \"http://127.0.0.1:1087/\" :path \"./cdk/quicklisp/\")' --eval '(quit)'")

(echo "Load calm.asd ...")
(load "calm.asd")
(ql:quickload :calm)

(echo "Copying dependencies ...")
(exec "mkdir -p ./cdk/lib/calm/")
(copy-deps)
(config-deps)

(echo "Copying libzstd")

#+darwin
(exec "cp /usr/local/lib/libzstd.dylib ./cdk/lib/calm/")
#+linux
(exec "cp /usr/lib64/libzstd.so* ./cdk/lib/calm/")
#+win32
(exec "cp /mingw64/bin/libzstd.dll ./cdk/lib/calm/")

(echo "Building launcher ...")

(exec "./sbcl --load launcher.lisp")

(echo "Starting CALM ...")

(exec "CI=true ./calm")

(echo "DONE.")

(uiop:quit)
