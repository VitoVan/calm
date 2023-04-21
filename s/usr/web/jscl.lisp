#-calm
(ql:quickload :calm)
(in-package :calm)

(defun load-jscl ()
  (let* ((jscl-git-path (merge-pathnames "build/web/jscl/" *calm-env-calm-home*))
         (jscl-lisp-file (merge-pathnames "build/web/jscl/jscl.lisp" *calm-env-calm-home*)))
    (unless (probe-file jscl-git-path)
      (u:exec (str:concat "git clone " (uiop:native-namestring jscl-git-path)))
      (u:exec (str:concat
               "cd " (uiop:native-namestring jscl-git-path)
               " && git checkout 0c21063a66f5043e6aadbae940a612db6ed0c539")))
    (format t "loading JSCL ~A~%" jscl-lisp-file)
    (load jscl-lisp-file)))

(load-jscl)
