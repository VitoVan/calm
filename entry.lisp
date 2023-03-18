(let ((quicklisp-setup (probe-file "quicklisp/setup.lisp")))
  (when quicklisp-setup
    (load quicklisp-setup)))
;; Load CALM
(load "calm.asd")
(ql:quickload :calm)
(in-package #:calm)
(calm-config)

;; switch to the CALM_APP_DIR
(uiop:chdir *calm-env-app-dir*)

(defun copy-dist-files (pathname &key with-canvas)
  ;; copy files
  (let* ((ori-lib-dir (merge-pathnames "lib/" *calm-env-calm-home*))
         (dist-dir (merge-pathnames pathname *calm-env-app-dir*))
         (ori-assets-dir (merge-pathnames "assets/" *calm-env-app-dir*))
         (dist-assets-dir (merge-pathnames "assets/" dist-dir))
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
    ;; copy calm launcher
    (u:copy-file
     (merge-pathnames calm-bin *calm-env-calm-home*)
     (merge-pathnames (str:concat "calm" #+win32 ".exe")  dist-dir))
    ;; copy `canvas.lisp' and `src' directory
    (when with-canvas
      (u:copy-file (merge-pathnames "canvas.lisp"  *calm-env-app-dir*) (merge-pathnames "canvas.lisp" dist-dir))
      (u:copy-dir (merge-pathnames "src/" *calm-env-app-dir*) (merge-pathnames "src/" dist-dir)))))

(defun dist (pathname &key with-canvas)
  (u:calm-log "binary pathname ~A [~A]~%" pathname (if with-canvas "(with-canvas)" ""))
  (copy-dist-files pathname :with-canvas with-canvas)
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
   (if with-canvas
       #'calm:calm-load-and-start
       #'calm:calm-start)))

(defun dist-by-new-process (&key with-canvas)
  (let ((calm-bin-abs (str:concat (namestring *calm-env-calm-home*) "calm" #+win32 ".exe")))
    ;; the following command will quit SBCL,
    ;; so exec it as external cmd instead of load lisp file
    (u:calm-log-fancy "dumping binary, this may take a few minutes...")
    (if with-canvas
        (u:exec (str:concat calm-bin-abs " dist-with-canvas"))
        (u:exec (str:concat calm-bin-abs " dist")))))

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

(cond

  ((string= *calm-env-calm-cmd* "test")
   (u:calm-log-fancy "Nothing to test.")
   (uiop:quit 0))

  ((string= *calm-env-calm-cmd* "show")
   (calm:calm-load-and-start))

  ((string= *calm-env-calm-cmd* "hello")
   (u:copy-file (merge-pathnames "s/usr/all/panic.lisp" *calm-env-calm-home*)
                (merge-pathnames "canvas.lisp" *calm-env-app-dir*))
   (ensure-directories-exist (merge-pathnames "src/" *calm-env-app-dir*))
   (ensure-directories-exist (merge-pathnames "assets/" *calm-env-app-dir*))
   (u:calm-log-fancy "Hello, sample files and directories created, please enjoy"))

  ((string= *calm-env-calm-cmd* "publish") (publish))

  ((string= *calm-env-calm-cmd* "publish-with-options")
   (setf (uiop:getenv "CALM_ASK_ME") "yes-please")
   (publish))

  ((string= *calm-env-calm-cmd* "share")
   (let ((calm-archive-name (str:concat "calm-share-" (write-to-string (get-universal-time)) ".tar.gz")))
     (ensure-directories-exist (merge-pathnames "src/" *calm-env-app-dir*))
     (ensure-directories-exist (merge-pathnames "assets/" *calm-env-app-dir*))
     (u:exec-if #+win32 "whereis tar" #-win32 "command -v tar"
                (str:concat "tar -cvzf ./" calm-archive-name " canvas.lisp src assets")
                ;; yes, Windows also has tar
                ;; https://techcommunity.microsoft.com/t5/containers/tar-and-curl-come-to-windows/ba-p/382409
                "Can NOT find command tar, so you don't have tar?"
                )
     (u:calm-log-fancy "CALM Archive Created: ~A" calm-archive-name)
     (u:calm-log "Generating Sharing Link:~%")
     (u:exec-if #+win32 "whereis curl" #-win32 "command -v curl"
                (str:concat "curl -s --upload-file ./" calm-archive-name " https://transfer.sh/")
                ;; yes, Windows also has curl
                ;; https://techcommunity.microsoft.com/t5/containers/tar-and-curl-come-to-windows/ba-p/382409
                "Can NOT find command tar, so you don't have curl?"
                )
     (u:calm-log "~%")))


  ;; rebuild calm.core
  ;; this could speed up your CALM command
  #+sbcl
  ((string= *calm-env-calm-cmd* "core") (sb-ext:save-lisp-and-die (merge-pathnames *calm-env-calm-home* "calm.core")))

  #+darwin
  ((string= *calm-env-calm-cmd* "make-bundle") (u:load-from-calm "s/usr/macos/bundle.lisp"))
  #+darwin
  ((string= *calm-env-calm-cmd* "make-dmg") (u:load-from-calm "s/usr/macos/dmg.lisp"))

  #+win32
  ((string= *calm-env-calm-cmd* "make-installer") (u:load-from-calm "s/usr/windows/installer.lisp"))
  #+win32
  ((string= *calm-env-calm-cmd* "set-icon") (u:load-from-calm "s/usr/windows/icon.lisp"))

  #+linux
  ((string= *calm-env-calm-cmd* "make-appimage") (u:load-from-calm "s/usr/linux/appimage.lisp"))

  #+sbcl
  ((string= *calm-env-calm-cmd* "dist-with-canvas") (dist #p"dist-with-canvas/" :with-canvas t))

  #+sbcl
  ((string= *calm-env-calm-cmd* "dist") (dist #p"dist/"))

  (t (format t "UNKOWN CALM_CMD: ~A~%" *calm-env-calm-cmd*)))

(uiop:quit)
