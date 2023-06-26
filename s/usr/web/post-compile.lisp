#-calm
(ql:quickload :calm)
(in-package :calm)

(defun post-compile ()
  (let* ((app-web-dir (merge-pathnames "web/" *calm-env-app-dir*))
         (app-build-web-dir (merge-pathnames "build-web/" *calm-env-app-dir*))
         (build-web-dir (merge-pathnames "build/web/" *calm-env-calm-home*))
         (jscl-js-file (merge-pathnames "jscl/jscl.js" build-web-dir))
         (jscl-output-file (merge-pathnames "jscl.js" app-web-dir)))

    (format t "copying jscl.js~%")
    (u:copy-file jscl-js-file jscl-output-file)
    (format t "copying calm.js~%")
    (u:copy-file (merge-pathnames "calm.js" app-build-web-dir) (merge-pathnames "calm.js" app-web-dir))
    (format t "copying calm.worker.js~%")
    (u:copy-file (merge-pathnames "calm.worker.js" app-build-web-dir) (merge-pathnames "calm.worker.js" app-web-dir))
    (format t "copying calm.wasm~%")
    (u:copy-file (merge-pathnames "calm.wasm" app-build-web-dir) (merge-pathnames "calm.wasm" app-web-dir))
    (format t "copying calm.data~%")
    (u:copy-file (merge-pathnames "calm.data" app-build-web-dir) (merge-pathnames "calm.data" app-web-dir))
    (format t "copying calm.html~%")
    (u:copy-file (merge-pathnames "calm.html" app-build-web-dir) (merge-pathnames "calm.html" app-web-dir))
    (format t "copying favicon.ico~%")
    (u:copy-file (merge-pathnames "build/app.ico" *calm-env-calm-home*) (merge-pathnames "favicon.ico" app-web-dir))
    (when (probe-file app-build-web-dir)
      (uiop:delete-directory-tree app-build-web-dir :validate t))
    (u:calm-log-fancy "Web page generated: ~A~%" app-web-dir)
    (u:exec (str:concat "ls -lah " (uiop:native-namestring app-web-dir)))))

(post-compile)