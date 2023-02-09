(load "quicklisp/setup.lisp")
;; Load CALM
(load "calm.asd")
(ql:quickload :calm)
;; switch to the APP_DIR
(uiop:chdir (uiop:getenv "APP_DIR"))

;; show / dist / dist-with-canvas
(defparameter cmd (uiop:getenv "CALM_CMD"))

(defun prepare-for-dist (dir-name &key with-canvas)
  (let* ((ori-lib-dir (str:concat (uiop:getenv "CALM_DIR") "lib/"))
         (dist-dir (str:concat (uiop:getenv "APP_DIR") dir-name "/"))
         (lib-dir (str:concat dist-dir "lib/"))
         (bin-dir (str:concat dist-dir "bin/"))
         (calm-bin #+win32 "calmNoConsole.exe" #-win32 "calm"))
    (mapcar #'ensure-directories-exist (list lib-dir bin-dir))
    (loop for x in (uiop:directory-files (pathname ori-lib-dir))
          do (uiop:copy-file x (str:concat lib-dir (pathname-name x) "." (pathname-type x))))
    (uiop:copy-file (str:concat  (uiop:getenv "CALM_DIR") calm-bin) (str:concat dist-dir "calm" #+win32 ".exe" #-win32 ""))
    (when with-canvas
      (uiop:copy-file (str:concat  (uiop:getenv "APP_DIR") "canvas.lisp") (str:concat dist-dir "canvas.lisp")))))

(cond

  ;; Show the Canvas
  ((string= cmd "test")
   (format t "Nothing to test.~%")
   (uiop:quit 0))

  ;; Show the Canvas
  ((string= cmd "show")
   (calm:calm-load-and-start))

  ;; Rebuild calm.core
  ((string= cmd "core") (save-lisp-and-die "calm.core"))

  ;; Distribute the App with canvas.lisp
  ((string= cmd "dist-with-canvas")
   (prepare-for-dist "dist-with-canvas" :with-canvas t)
   (sb-ext:save-lisp-and-die
    #+win32 ".\\dist-with-canvas\\bin\\calm-app.exe"
    #-win32 "./dist-with-canvas/bin/calm-app"
    :compression 22
    #+win32 :application-type
    #+win32 :gui
    :executable t
    :toplevel #'calm:calm-load-and-start))

  ;; Distribute the App
  ((string= cmd "dist")
   (prepare-for-dist "dist" :with-canvas nil)
   (load (merge-pathnames "canvas.lisp" (uiop:getcwd)))
   (sb-ext:save-lisp-and-die
    #+win32 ".\\dist\\bin\\calm-app.exe"
    #-win32 "./dist/bin/calm-app"
    :compression 22
    #+win32 :application-type
    #+win32 :gui
    :executable t
    :toplevel #'calm:calm-start))

  (t (format t "UNKOWN CALM_CMD: ~A~%" cmd)))

(quit)
