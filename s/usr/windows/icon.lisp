#-calm
(ql:quickload :calm)
(in-package :calm)
(calm-config)

(defun set-icon (app-icon)

  (let ((rcedit-bin (merge-pathnames "s/usr/windows/rcedit.exe" *calm-env-calm-home*))
        (rcedit-url "https://github.com/electron/rcedit/releases/download/v1.1.1/rcedit-x64.exe")
        (old-sbcl (merge-pathnames "sbcl/bin/sbcl.exe" *calm-env-calm-home*))
        (new-sbcl (merge-pathnames "sbcl/bin/sbcl-with-icon.exe" *calm-env-calm-home*))
        (calm-no-console (merge-pathnames "calmNoConsole.exe" *calm-env-calm-home*)))

    (uiop:copy-file old-sbcl new-sbcl)

    (unless (probe-file rcedit-bin)
      (u:calm-log "downloading rcedit.exe from:~%~A~%" rcedit-url)
      (u:exec (str:concat "curl.exe -L -o " (uiop:native-namestring rcedit-bin) " --url " rcedit-url)))

    (u:exec (str:concat (uiop:native-namestring rcedit-bin)
                        " "
                        (uiop:native-namestring new-sbcl)
                        " --set-icon "
                        app-icon))

    (u:exec (str:concat (uiop:native-namestring rcedit-bin)
                        " "
                        (uiop:native-namestring calm-no-console)
                        " --set-icon "
                        app-icon)))

  (u:calm-log "~%ICON applied: ~A~%" app-icon))

(set-icon
 (u:get-from-env-or-ask 'app-icon (namestring (merge-pathnames "build/app.ico" *calm-env-calm-home*))))
