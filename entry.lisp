#-quicklisp
(let ((quicklisp-setup (probe-file "quicklisp/setup.lisp")))
  (when quicklisp-setup
    (load quicklisp-setup)))
;; https://groups.google.com/g/quicklisp/c/wrULkRePVE4
#+win32
(quicklisp-client::make-system-index "quicklisp/local-projects/")
;; Load CALM
#-calm
(load "calm.asd")
#-calm
(ql:quickload :calm)
(in-package #:calm)
(calm-config)

;; switch to the CALM_APP_DIR
(uiop:chdir *calm-env-app-dir*)

(defun copy-dist-files (pathname)
  ;; copy files
  (let* ((ori-lib-dir (merge-pathnames "lib/" *calm-env-calm-home*))
         (dist-dir (merge-pathnames pathname *calm-env-app-dir*))
         (ori-assets-dir (merge-pathnames "assets/" *calm-env-app-dir*))
         (dist-assets-dir (merge-pathnames "assets/" dist-dir))
         (ori-fonts-dir (merge-pathnames "fonts/" *calm-env-app-dir*))
         (dist-fonts-dir (merge-pathnames "fonts/" dist-dir))
         (dist-lib-dir (merge-pathnames "lib/" dist-dir))
         (dist-bin-dir (merge-pathnames "bin/" dist-dir))
         (calm-bin #+win32 "calmNoConsole.exe" #-win32 "calm"))
    ;; clean old dist
    (uiop:delete-directory-tree dist-dir :validate t :if-does-not-exist :ignore)
    ;; mkdir
    (mapcar #'ensure-directories-exist (list dist-bin-dir))
    ;; copy dependencies
    (u:copy-dir ori-lib-dir dist-lib-dir)
    ;; copy assets
    (u:copy-dir ori-assets-dir dist-assets-dir)
    ;; copy fonts
    (u:copy-dir ori-fonts-dir dist-fonts-dir)
    ;; copy calm launcher
    (u:copy-file
     (merge-pathnames calm-bin *calm-env-calm-home*)
     (merge-pathnames (str:concat "calm" #+win32 ".exe")  dist-dir))))

(defun dist (pathname)
  (u:calm-log "binary pathname ~A~%" pathname)
  (copy-dist-files pathname)
  ;; load `canvas.lisp'
  (load (merge-pathnames "canvas.lisp" *calm-env-app-dir*))
  ;; dump binary
  (sb-ext:save-lisp-and-die
   (merge-pathnames (str:concat "bin/calm-app" #+win32 ".exe") pathname)
   :compression 22
   #+win32 :application-type
   #+win32 :gui
   :executable t
   :toplevel
   #'calm:calm-start))

(defun dist-by-new-process ()
  (let ((calm-bin-abs (str:concat (namestring *calm-env-calm-home*) "calm" #+win32 ".exe")))
    ;; the following command will quit SBCL,
    ;; so exec it as external cmd instead of load lisp file
    (u:calm-log-fancy "dumping binary, this may take a few minutes...")
    (u:exec (str:concat calm-bin-abs " dist"))))

(defun publish-web ()
  (u:load-from-calm "s/usr/web/wasm.lisp")
  (u:load-from-calm "s/usr/web/load-and-compile.lisp")
  (u:load-from-calm "s/usr/web/post-compile.lisp"))

(defun publish ()
  (setf (uiop:getenv "DIST_DIR") (uiop:native-namestring (uiop:merge-pathnames* "dist/" *calm-env-app-dir*)))
  #+darwin
  (progn
    (dist-by-new-process)
    (u:calm-log "building macOS Application...")
    (u:load-from-calm "s/usr/macos/bundle.lisp")
    (u:calm-log "building macOS DMG, this may take a while...")
    (u:load-from-calm "s/usr/macos/dmg.lisp"))
  #+win32
  (progn
    (u:load-from-calm "s/usr/windows/icon.lisp")
    (setf (uiop:getenv "CALM_WITH_ICON") "YES_MY_LORD")
    (dist-by-new-process)
    (u:calm-log "building Windows Installer...")
    (u:load-from-calm "s/usr/windows/installer.lisp"))
  #+linux
  (progn
    (dist-by-new-process)
    (u:calm-log "building Linux AppImage...")
    (u:load-from-calm "s/usr/linux/appimage.lisp")))

(alexandria:switch (*calm-env-calm-cmd* :test #'equal)

  ("test"
   (u:calm-log-fancy "Nothing to test.")
   (uiop:quit 0))

  ("alive"
   (u:load-from-calm "s/dev/all/start-alive.lisp"))

  ("show"
   (calm:calm-load-and-start))

  ("hello"
   (u:copy-file (merge-pathnames "s/usr/all/panic.lisp" *calm-env-calm-home*)
                (merge-pathnames "canvas.lisp" *calm-env-app-dir*))
   (ensure-directories-exist (merge-pathnames "assets/" *calm-env-app-dir*))
   (ensure-directories-exist (merge-pathnames "fonts/" *calm-env-app-dir*))
   (u:copy-file (merge-pathnames "s/usr/all/fonts.conf" *calm-env-calm-home*)
                (merge-pathnames "fonts/fonts.conf" *calm-env-app-dir*))
   (u:calm-log-fancy "Hello, sample files and directories created, please enjoy"))

  ("publish" (publish))

  ("publish-web" (publish-web))

  ("publish-with-options"
   (setf (uiop:getenv "CALM_ASK_ME") "yes-please")
   (publish))

  ("publish-web-with-options"
   (setf (uiop:getenv "CALM_ASK_ME") "yes-please")
   (publish-web))

  ;; rebuild calm.core
  ;; this could speed up your CALM command
  #+sbcl
  ("core" (sb-ext:save-lisp-and-die (merge-pathnames *calm-env-calm-home* "calm.core")))

  #+darwin
  ("make-bundle" (u:load-from-calm "s/usr/macos/bundle.lisp"))
  #+darwin
  ("make-dmg" (u:load-from-calm "s/usr/macos/dmg.lisp"))

  #+win32
  ("make-installer" (u:load-from-calm "s/usr/windows/installer.lisp"))
  #+win32
  ("set-icon" (u:load-from-calm "s/usr/windows/icon.lisp"))

  #+linux
  ("make-appimage" (u:load-from-calm "s/usr/linux/appimage.lisp"))

  #+sbcl
  ("dist" (dist #p"dist/"))

  (t (format t "UNKOWN CALM_CMD: ~A~%" *calm-env-calm-cmd*)))

(uiop:quit)
