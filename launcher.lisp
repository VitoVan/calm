;; this file was intended to be loaded by SBCL > 2.2.5 and with compression feature enabled
;;
;; by loading this file, you will get a executable `launcher[.exe]`,
;; and you will need libzstd in the right place to run the executable.
;;
;; on Linux and macOS, you could use bash script, e.g.:
;;
;; Linux
;;    LD_LIBRARY_PATH=/the/path/of/libzstd:$LD_LIBRARY_PATH ./launcher
;; macOS
;;    DYLD_FALLBACK_LIBRARY_PATH=/the/path/of/libzstd ./launcher
;;
;; on Windows, you only need to place `libzstd.dll` alongside with your `launcher.exe`.

(ql:quickload :str)

(defmacro llog (control-string &rest rest)
  `(format t ,control-string ,@rest))


(defparameter *dist-mode* nil)

(defun launch ()

  (setf (uiop:getenv "CALM_DIR") (namestring (uiop:pathname-directory-pathname sb-ext:*core-pathname*)))
  (if *dist-mode*
      (setf (uiop:getenv "APP_DIR") (namestring (uiop:getenv "CALM_DIR")))
      (setf (uiop:getenv "APP_DIR") (namestring (uiop:getcwd))))
  (uiop:chdir (uiop:getenv "CALM_DIR"))
  (when (uiop:probe-file* "config.lisp") (load "config.lisp"))

  (llog "CORE_PATH: ~A~%" sb-ext:*core-pathname*)
  (llog "CALM_DIR: ~A~%" (uiop:getenv "CALM_DIR"))
  (llog "APP_DIR: ~A~%" (uiop:getenv "APP_DIR"))
  (llog "CURRENT_DIR: ~A~%" (uiop:getcwd))

  (llog "ARGS: ~A~%" (uiop:command-line-arguments))
  
  (let* ((calm-dir (uiop:getenv "CALM_DIR"))
         (app-dir (uiop:getenv "APP_DIR"))
         (before-canvas-file (uiop:probe-file* (str:concat app-dir "/before-canvas.lisp")))
         (canvas-file (uiop:probe-file* (str:concat app-dir "/canvas.lisp")))
         (args (uiop:command-line-arguments)))
    (cond
      ((equal '(dist --with-canvas) args) (format t "OK, distributing with canvas.lisp ...~%"))
      ((equal '(dist --fancy-app) args) (format t "OK, distributing fancy application ...~%"))
      
      ;; starting CALM
      ((equal nil args)
       (format t "Starting CALM ...~%")
       (uiop:run-program
        (str:concat
         "/usr/local/bin/sbcl --load 'calm.asd'"
         " --eval '(ql:quickload :calm)' "
         " --eval '(uiop:chdir \"" app-dir "\")'"
         (when before-canvas-file (str:concat " --load '" (namestring before-canvas-file) "' "))
         (when canvas-file (str:concat " --load '" (namestring canvas-file) "' "))
         " --eval '(calm:calm-start)'")
        )
       )
      
      (t (format t "Example usages:

Normal start, inside a directory contains canvas.lisp

    calm

Distribute binaries:

    calm dist

Distribute binaries with canvas.lisp:

    calm dist --with-canvas

Distribute fancy application:

    calm dist --fancy-app
")))))

(sb-ext:save-lisp-and-die
 #+win32
 "launcher.exe"
 #+(or darwin linux)
 "launcher"
;; :compression 22
 :executable t
 :toplevel #'launch)
