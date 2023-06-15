#-calm
(ql:quickload :calm)
(in-package :calm)
(calm-config)

(defun make-dmg (app-name dmg-icon)
  (uiop:chdir *calm-env-app-dir*)

  ;; clean old dmg
  (uiop:delete-file-if-exists (merge-pathnames (str:concat app-name ".dmg") *calm-env-app-dir*))

  (when (not (= (u:exec "command -v create-dmg" :ignore-error-status t) 0))
    (when (not (= (u:exec "command -v brew" :ignore-error-status t) 0))
      (u:exec "/bin/bash -c \"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\""))
    (u:exec "brew install create-dmg"))

  (u:exec
   (str:concat
    "create-dmg "
    (if (string= (uiop:getenv "CI_MATRIX_OS") "macos-13")
        " --skip-jenkins"  ;; macos-13 not working yet
        " ")
    " --hdiutil-verbose --volname \"" app-name " - CALM\""
    " --volicon \"" dmg-icon "\""
    " --window-pos 200 120"
    " --window-size 800 280"
    " --icon-size 100"
    " --icon \"" app-name ".app\" 200 90"
    " --hide-extension \"" app-name ".app\""
    " --app-drop-link 600 85"
    " \"" app-name ".dmg\"" " \"" app-name ".app/\""))

  (u:calm-log-fancy "~%DMG created: ~A.dmg~%" app-name))

(make-dmg
 (u:get-from-env-or-ask 'app-name "Hello")
 (u:get-from-env-or-ask 'dmg-icon (namestring (merge-pathnames "build/app-dmg.icns" *calm-env-calm-home*))))
