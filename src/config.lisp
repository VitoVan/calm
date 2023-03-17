(in-package :calm)

;; init config
(defparameter *calm-window-x* :centered)
(defparameter *calm-window-y* :centered)
(defparameter *calm-window-width* 600)
(defparameter *calm-window-height* 150)
(defparameter *calm-window-title* "CALM")
(defparameter *calm-window-flags* '(:shown :allow-highdpi))

;;
;; use OpenGL on Linux to avoid weird window flashing: (like it was closed and then opened again)
;;
#+linux
(push :opengl *calm-window-flags*)

;; on Wayland, this doesn't work, who should I blame?
(defparameter *calm-window-icon* nil)
(defparameter *calm-delay* 42)
(defparameter *calm-redraw* t
  "The canvas will be painted again and again by calling the `draw` function,
   setting it to `NIL` means you don't want the canvas to be painted again,
   setting it back to `t` will paint the canvas again and again by calling the `draw` function, again.
  ")
(defparameter *calm-music-format* sdl2-ffi:+audio-s32sys+)
(defparameter *calm-music-frequency* 44100)
(defparameter *calm-music-channels* 2)

;; runtime variables
(defparameter *calm-dpi-scale* 1)
(defparameter *calm-state-mouse-inside-window* nil)
(defparameter *calm-state-mouse-x* 0)
(defparameter *calm-state-mouse-y* 0)
(defparameter *calm-state-mouse-up* nil)
(defparameter *calm-state-mouse-down* nil)
(defparameter *calm-state-mouse-just-clicked* nil)

;; env
(defparameter *calm-env-calm-home* nil)
(defparameter *calm-env-calm-cmd* nil)
(defparameter *calm-env-app-dir* nil)
(defparameter *calm-env-host-lisp* nil)

;; other
(defvar *calm-version* (slot-value (asdf:find-system 'calm) 'asdf:version))

(pushnew :calm *features*)

(defun calm-config ()
  (setf *calm-env-calm-home* (uiop:getenv "CALM_HOME")
        *calm-env-app-dir* (uiop:getenv "CALM_APP_DIR")
        *calm-env-calm-cmd* (uiop:getenv "CALM_CMD")
        *calm-env-host-lisp* (uiop:getenv "CALM_HOST_LISP"))

  ;;
  ;; for macOS bundled CALM Application (not CALM itself)
  ;;
  ;; on macOS, if the CALM_HOME env contains ".app/Contents/MacOS",
  ;; and the user double clicked the application,
  ;; then CALM_APP_DIR won't be able to be set correctly,
  ;; since the `pwd` will be given as "/Users/jack/" instead of the real location,
  ;; so we should set CALM_APP_DIR to CALM_HOME
  ;;
  ;; but, we can't just detect this by `(str:contains? ".app/Contents/MacOS" (uiop:getenv "CALM_HOME"))`
  ;; because if we have packed CALM as an APP, then it will always find the canvas.lisp inside the app bundle,
  ;; instead of the current directory.
  ;;
  ;; so, let's create a dummy file (.calm-app-macos-bundle) inside the app bundle,
  ;; and if it exists, then we will pick it up.
  #+darwin
  (when
      (and
       (str:contains? ".app/Contents/MacOS" (namestring *calm-env-calm-home*))
       (probe-file (merge-pathnames ".calm-app-macos-bundle" *calm-env-calm-home*)))
    (setf (uiop:getenv "CALM_APP_DIR") *calm-env-calm-home*
          *calm-env-app-dir* *calm-env-calm-home*))

  ;;
  ;; for CALM itself
  ;; to know if CALM has already successfully started once
  ;;
  ;; touch file: .calm-initialised
  ;; thank you, Zach Beane
  ;; https://lisptips.com/post/11136367093/touching-a-file
  (when (and (not (probe-file ".calm-initialised")) (probe-file "calm.asd"))
    (u:touch-file ".calm-initialised")
    (format t "~A~%" "CALM initialised successfully.")))
