#-calm
(ql:quickload :calm)
(in-package :calm)
(calm-config)

(defun make-appimage (app-name app-icon dist-dir)
  (uiop:chdir *calm-env-app-dir*)

  (let ((appimage-tool-bin (merge-pathnames "s/usr/linux/appimagetool.AppImage" *calm-env-calm-home*))
        (appimage-tool-url "https://github.com/AppImage/AppImageKit/releases/download/13/appimagetool-x86_64.AppImage")
        (appimage-app-dir (merge-pathnames (str:concat app-name ".AppDir/") *calm-env-app-dir*))
        (appimage-app-run (merge-pathnames "s/usr/linux/AppRun" *calm-env-calm-home*)))
    (unless (probe-file appimage-tool-bin)
      (u:calm-log "downloading appimagetool from:~%~A~%" appimage-tool-url)
      (u:exec (str:concat "curl -o " (uiop:native-namestring appimage-tool-bin) " -L " appimage-tool-url))
      (u:exec (str:concat "chmod +x " (uiop:native-namestring appimage-tool-bin))))

    ;; mkdir
    (ensure-directories-exist appimage-app-dir)

    ;; default SDL Window ICON
    (ensure-directories-exist (merge-pathnames "build/" dist-dir))
    (u:copy-file
     (merge-pathnames "build/app.png" *calm-env-calm-home*)
     (merge-pathnames "build/app.png" dist-dir))

    ;; copy dist-dir
    (ensure-directories-exist (merge-pathnames "usr/" appimage-app-dir))
    (u:copy-dir dist-dir (merge-pathnames "usr/bin/" appimage-app-dir))

    ;; copy AppRun
    (u:copy-file appimage-app-run (merge-pathnames "AppRun" appimage-app-dir))
    ;; make it executable
    (u:exec (str:concat "chmod +x " (uiop:native-namestring (merge-pathnames "AppRun" appimage-app-dir))))

    ;; copy AppImage ICON
    (u:copy-file app-icon (merge-pathnames (str:concat "icon." (pathname-type app-icon)) appimage-app-dir))

    ;; generate desktop file
    (str:to-file (merge-pathnames (str:concat app-name ".desktop") appimage-app-dir)
                 (str:replace-all
                  "_APP_NAME_" app-name
                  (str:from-file (merge-pathnames "s/usr/linux/app.desktop" *calm-env-calm-home*))))

    (u:exec (str:concat
             (uiop:native-namestring appimage-tool-bin)
             " --appimage-extract-and-run "
             "\"" (uiop:native-namestring appimage-app-dir) "\""
             " \"" app-name ".AppImage\"")))

  (u:calm-log-fancy "~%AppImage created: ~A.AppImage~%" app-name))

(make-appimage
 (u:get-from-env-or-ask 'app-name "Hello")
 (u:get-from-env-or-ask 'app-icon (namestring (merge-pathnames "build/app.png" *calm-env-calm-home*)))
 (u:get-from-env-or-ask 'dist-dir (uiop:native-namestring (uiop:merge-pathnames* "dist/" *calm-env-app-dir*))))
