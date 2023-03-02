#-calm
(ql:quickload :calm)
(in-package :calm)
(calm-config)

(defun make-dmg (app-name dmg-icon)
  (uiop:chdir *calm-env-app-dir*)

  ;; clean old dmg
  (uiop:delete-file-if-exists (merge-pathnames (str:concat app-name ".dmg") *calm-env-app-dir*))

  (u:exec-if "command -v create-dmg"
             (str:concat
              "create-dmg --hdiutil-verbose --volname \"" app-name " - CALM\""
              " --volicon \"" dmg-icon "\""
              " --window-pos 200 120"
              " --window-size 800 280"
              " --icon-size 100"
              " --icon \"" app-name ".app\" 200 90"
              " --hide-extension \"" app-name ".app\""
              " --app-drop-link 600 85"
              " \"" app-name ".dmg\"" " \"" app-name ".app/\"")
             "brew install create-dmg" :re-exec-then t)
  (u:calm-log-fancy "~%DMG created: ~A.dmg~%" app-name))

(make-dmg
 (u:get-from-env-or-ask 'app-name "Hello")
 (u:get-from-env-or-ask 'dmg-icon (namestring (merge-pathnames "build/app-dmg.icns" *calm-env-calm-home*))))
