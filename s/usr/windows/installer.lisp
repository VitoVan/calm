#-calm
(ql:quickload :calm)
(in-package :calm)
(calm-config)

(defun make-installer (app-name dist-dir)
  (uiop:chdir *calm-env-app-dir*)

  (uiop:delete-directory-tree (merge-pathnames "calm-app-root/" *calm-env-app-dir*) :validate t :if-does-not-exist :ignore)
  (uiop:delete-file-if-exists (merge-pathnames "installer.nsi" *calm-env-app-dir*))

  (u:copy-dir
   (or
    (uiop:absolute-pathname-p dist-dir)
    (uiop:merge-pathnames* dist-dir *calm-env-app-dir*))
   (merge-pathnames "calm-app-root" *calm-env-app-dir*))

  ;; make installer script
  (str:to-file (merge-pathnames "installer.nsi" *calm-env-app-dir*)
               (str:replace-all
                "_APP_NAME_" app-name
                (str:from-file (merge-pathnames "s/usr/windows/installer.nsi" *calm-env-calm-home*))))

  ;; copy installer assets
  (let* ((installer-assets-dir (merge-pathnames "calm-installer-assets/" *calm-env-app-dir*))
         (calm-build-dir (merge-pathnames "build/" *calm-env-calm-home*)))
    (unless (probe-file installer-assets-dir)
      (ensure-directories-exist installer-assets-dir)
      (mapcar
       #'(lambda (filename)
           (u:copy-file (merge-pathnames filename calm-build-dir) (merge-pathnames filename installer-assets-dir)))
       '("app-installer.ico" "app-uninstaller.ico" "installer-header.bmp" "installer-page.bmp"))))

  (setf (uiop:getenv "PATH") (str:concat "C:\\Program Files (x86)\\NSIS;" (uiop:getenv "PATH")))

  (when (not (= (u:exec "where makensis" :ignore-error-status t) 0))
    (when (not (= (u:exec "where winget" :ignore-error-status t) 0))
      (u:exec "calm s usr windows winget.ps1"))
    (u:exec "winget install nsis -s winget --accept-source-agreements --accept-package-agreements"))

  (u:exec "makensis -V4 installer.nsi")

  (u:calm-log-fancy "~%Installer created: ~A-Installer.exe~%" app-name))

(make-installer
 (u:get-from-env-or-ask 'app-name "Hello")
 (u:get-from-env-or-ask 'dist-dir (uiop:native-namestring (uiop:merge-pathnames* "dist/" *calm-env-app-dir*))))
