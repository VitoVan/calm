#-calm
(ql:quickload :calm)
(in-package :calm)

(defun load-and-compile (lisp-files)
  (let* ((app-web-dir (merge-pathnames "web/" *calm-env-app-dir*))
         (app-output-file (merge-pathnames "canvas.js" app-web-dir))
         (quicklisp-setup-file (merge-pathnames "quicklisp/setup.lisp" *calm-env-calm-home*))
         (jscl-git-path (merge-pathnames "build/web/jscl/" *calm-env-calm-home*))
         (jscl-git-url "https://github.com/jscl-project/jscl.git")
         (jscl-lisp-file (merge-pathnames "build/web/jscl/jscl.lisp" *calm-env-calm-home*))
         (jscl-compile-script-template (merge-pathnames "s/usr/web/jscl-compile-script-template.lisp" *calm-env-calm-home*))
         (jscl-compile-script (merge-pathnames "build/web/jscl/jscl-compile-script.lisp" *calm-env-calm-home*)))
    (ensure-directories-exist app-web-dir)
    (unless (probe-file jscl-git-path)
      (u:exec (str:concat "git clone " jscl-git-url " " (uiop:native-namestring jscl-git-path)))
      (u:exec (str:concat
               "cd " (uiop:native-namestring jscl-git-path)
               " && git checkout 0c21063a66f5043e6aadbae940a612db6ed0c539")))
    (u:calm-log "generating JSCL compile scripts ~A~%" jscl-compile-script)
    (str:to-file
     jscl-compile-script
     (str:replace-using
      (list "__LISP_FILES_LIST__" lisp-files
            "__APP_OUTPUT_FILE__" (uiop:native-namestring app-output-file)
            "__CALM_HOME__" *calm-env-calm-home*)
                        (str:from-file jscl-compile-script-template)))
    (u:calm-log "load and compile with JSCL~%")
    (u:exec (str:concat "CALM_NO_CORE=1 calm sbcl --load " (uiop:native-namestring quicklisp-setup-file)
                        " --load " (uiop:native-namestring jscl-lisp-file)
                        " --load " (uiop:native-namestring jscl-compile-script)))))

(load-and-compile
 (u:get-from-env-or-ask 'lisp-files
                        (write-to-string (list (uiop:native-namestring (uiop:merge-pathnames* "canvas.lisp" *calm-env-app-dir*))))))
