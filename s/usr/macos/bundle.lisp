#-calm
(ql:quickload :calm)
(in-package :calm)
(calm-config)

(defun make-bundle (app-name bundle-id app-version dist-dir app-icon)
  (let* ((ori-plist (str:from-file (merge-pathnames "s/usr/macos/app-Info.plist" *calm-env-calm-home*)))
         (app-dir (merge-pathnames (str:concat app-name ".app/") *calm-env-app-dir*))
         (app-content-dir (merge-pathnames "Contents/" app-dir))
         (app-receipt-dir (merge-pathnames "_MASReceipt/" app-content-dir))
         (app-resources-dir (merge-pathnames "Resources/" app-content-dir))
         (app-macos-dir (merge-pathnames "MacOS/" app-content-dir))
         (dist-dir-abs (or (uiop:absolute-pathname-p dist-dir)
                           (uiop:merge-pathnames* dist-dir *calm-env-app-dir*)))
         (app-icon-abs (or (uiop:absolute-pathname-p app-icon)
                           (uiop:merge-pathnames* app-icon *calm-env-app-dir*))))

    ;; clean old bunlde
    (uiop:delete-directory-tree app-dir :validate t :if-does-not-exist :ignore)

    (ensure-directories-exist app-content-dir)
    (ensure-directories-exist app-receipt-dir)
    (ensure-directories-exist app-resources-dir)

    ;; touch receipt
    ;;https://notes.alinpanaitiu.com/Making%20macOS%20apps%20uninstallable
    ;; the following trick makes the app "uninstallable"
    (u:touch-file (merge-pathnames "receipt" app-receipt-dir))
    ;; write Info.plist
    (str:to-file (merge-pathnames "Info.plist" app-content-dir)
                 (str:replace-using
                  (list "_APP_NAME_" app-name
                        "_BUNDLE_ID_" bundle-id
                        "_APP_VERSION_" app-version)
                  ori-plist))
    ;; copy dist
    (u:copy-dir
     dist-dir-abs
     app-macos-dir)
    ;; copy icon
    (u:copy-file app-icon-abs (merge-pathnames "icon.icns" app-resources-dir))
    ;; touch bundle indicator
    (u:touch-file (merge-pathnames ".calm-app-macos-bundle" app-macos-dir)))

  (u:calm-log-fancy "~%Application Bundle created: ~A.app~%" app-name))

(make-bundle
 (u:get-from-env-or-ask 'app-name "Hello")
 (u:get-from-env-or-ask 'bundle-id "com.jack.hello")
 (u:get-from-env-or-ask 'app-version "0.0.1")
 (u:get-from-env-or-ask 'dist-dir (uiop:native-namestring (uiop:merge-pathnames* "dist/" *calm-env-app-dir*)))
 (u:get-from-env-or-ask 'app-icon (namestring (merge-pathnames "build/app.icns" *calm-env-calm-home*))))