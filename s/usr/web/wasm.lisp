#-calm
(ql:quickload :calm)
(in-package :calm)

(defun make-wasm (rebuild-wasm-p)
  (let ((app-wasm-file (merge-pathnames "web/calm.wasm" *calm-env-app-dir*))
        (wasm-file (merge-pathnames "build/web/calm.wasm" *calm-env-calm-home*))
        (wasm-sh-file (merge-pathnames "s/usr/web/wasm.sh" *calm-env-calm-home*)))
    (if (or (not (probe-file app-wasm-file)) (not (string= rebuild-wasm-p "no")))
        (progn
          (u:exec (str:concat "bash " (uiop:native-namestring wasm-sh-file)))
          (u:calm-log-fancy "~%WASM file compiled."))
        (format t "Using cached WASM: ~A" app-wasm-file))))

(make-wasm
 (u:get-from-env-or-ask 'rebuild-wasm-p "no"))
