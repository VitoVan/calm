(in-package :calm)

;; init config
#-jscl
(defparameter *calm-window-x* :centered)
#-jscl
(defparameter *calm-window-y* :centered)
(defparameter *calm-window-width* 600)
(defparameter *calm-window-height* 150)
(defparameter *calm-window-title* "CALM")
#-jscl
(defparameter *calm-window-flags* '(:shown :allow-highdpi))
#-jscl
(defparameter *calm-renderer-flags* '(:accelerated :presentvsync))
#-jscl
(defparameter *calm-default-font-size* 80)
#-jscl
(defparameter *calm-default-font-family* "Arial")

;; debug variable
(defparameter *calm-debug-variable-a* nil)
(defparameter *calm-debug-variable-b* nil)
(defparameter *calm-debug-variable-c* nil)


;;
;; use OpenGL on Linux to avoid weird window flashing: (like it was closed and then opened again)
;;
#+(and linux (not jscl))
(push :opengl *calm-window-flags*)

;; on Wayland, this doesn't work, who should I blame?
#-jscl
(defparameter *calm-window-icon* nil)
#-jscl
(defparameter *calm-delay* 42)
#+jscl
(defparameter *calm-fps* 42)

(defparameter *calm-redraw* t
  "The canvas will be painted again and again by calling the `draw' function,
   setting it to `NIL' means you don't want the canvas to be painted again,
   setting it back to `t' will paint the canvas again and again by calling the `draw' function, again.
  ")
#-jscl
(defparameter *calm-music-format* sdl2-ffi:+audio-s32sys+)
#+jscl
(defparameter *calm-music-format* 32800)

(defparameter *calm-music-frequency* 44100)
(defparameter *calm-music-channels* 2)

;;
;; chunksize: audio buffer size in sample FRAMES (total samples divided by channel count).
;; I thought it should be: (* calm::*calm-music-channels* calm::*calm-music-frequency*)
;; but, it was delayed, I have to set it to `1024' according to this:
;; https://stackoverflow.com/questions/983997/i-have-an-unintended-delay-in-playing-a-mix-chunk
#-jscl
(defparameter *calm-music-chunksize* 1024)
;; but, when it is on the browser,
;; I have to set it to a very large value to make things a little more smooth,
;; it still suffers some wierd noises sometimes.
;; I don't know why, please enlight me if you know something about this
#+jscl
(defparameter *calm-music-chunksize* (* 4 1024))

;; runtime variables

(defparameter *calm-state-mouse-inside-window* nil)
(defparameter *calm-state-mouse-x* 0)
(defparameter *calm-state-mouse-y* 0)
(defparameter *calm-state-mouse-up* nil)
(defparameter *calm-state-mouse-down* nil)
(defparameter *calm-state-mouse-just-clicked* nil)

(defparameter *calm-state-finger-x* 0)
(defparameter *calm-state-finger-y* 0)
(defparameter *calm-state-finger-just-clicked* nil)

(defparameter *calm-state-audio-open* nil)
(defparameter *calm-state-loaded-audio* nil)

;; env
#-jscl
(defparameter *calm-env-calm-home* nil)
#-jscl
(defparameter *calm-env-calm-cmd* nil)
#-jscl
(defparameter *calm-env-app-dir* nil)
#-jscl
(defparameter *calm-env-host-lisp* nil)

(pushnew :calm *features*)

#+jscl
(defparameter *configured* nil)

#+jscl
(defun jscl-draw ()
  (unless *configured*
    ;; this has to be called in jscl-draw, since by then the wasm has finally initialised.
    (let ((title-ptr (#j:allocateUTF8 *calm-window-title*)))
      (#j:_config title-ptr)
      (#j:_free title-ptr))
    (setf *configured* t))
  ;; draw something
  (internal-draw))

#-jscl
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
  (when
      (and
       ;; why `featurep' instead of `#+darwin'?
       ;; track: https://github.com/jscl-project/jscl/issues/475
       (uiop:featurep :darwin)
       (str:contains? ".app/Contents/MacOS" (namestring *calm-env-calm-home*))
       (probe-file (merge-pathnames ".calm-app-macos-bundle" *calm-env-calm-home*)))
    (setf (uiop:getenv "CALM_APP_DIR") *calm-env-calm-home*
          *calm-env-app-dir* *calm-env-calm-home*))

  ;;
  ;; for CALM itself
  ;; to know if CALM has already successfully started once
  ;;
  ;; touch file: .calm-initialised
  (when (and (not (probe-file ".calm-initialised")) (probe-file "calm.asd"))
    (u:touch-file ".calm-initialised")
    (format t "~A~%" "CALM initialised successfully."))

  ;;
  ;; config libs
  ;;
  (setf cffi:*foreign-library-directories* nil)
  (pushnew (merge-pathnames "lib/" *calm-env-calm-home*) cffi:*foreign-library-directories*)

  ;;
  ;; config gir typelib
  ;;
  (gir:repository-prepend-search-path (uiop:native-namestring (merge-pathnames "lib/" *calm-env-calm-home*)))


  ;;
  ;; DPI awareness
  ;; https://github.com/libsdl-org/SDL/pull/5778
  (setf (uiop:getenv "SDL_WINDOWS_DPI_SCALING") "1")

  ;; let pango use fontconfig to get cross-platform font loading support
  (setf (uiop:getenv "PANGOCAIRO_BACKEND") "fontconfig")

  ;; set fontconfig config dir
  (setf (uiop:getenv "FONTCONFIG_PATH") (uiop:native-namestring (merge-pathnames "fonts/" *calm-env-app-dir*)))
  (setf (uiop:getenv "FONTCONFIG_FILE") (uiop:native-namestring (merge-pathnames "fonts/fonts.conf" *calm-env-app-dir*)))

  (format t "fontpath: ~A~%" (uiop:native-namestring (merge-pathnames "fonts/" *calm-env-app-dir*)))
  (format t "fontfile: ~A~%" (uiop:native-namestring (merge-pathnames "fonts/fonts.conf" *calm-env-app-dir*)))

  ;; (setf (uiop:getenv "FC_DEBUG") "1024")

  ;; init fc
  (fontconfig:fc-init)
  )

#+jscl
(defun get-calm-redraw () *calm-redraw*)

#+jscl
(defun calm-config ()
  (setf #j:draw #'jscl-draw)
  (when (fboundp 'think)
    (setf #j:think #'think))
  (setf #j:get_calm_redraw #'get-calm-redraw)
  (setf #j:on_mousewheel #'internal-on-mousewheel)

  (setf #j:on_mousemotion #'(lambda (x y) (internal-on-mousemotion :x x :y y)))
  (setf #j:on_mousebuttonup #'(lambda (button x y clicks) (internal-on-mousebuttonup :button button :x x :y y :clicks clicks)))
  (setf #j:on_mousebuttondown #'(lambda (button x y clicks) (internal-on-mousebuttondown :button button :x x :y y :clicks clicks)))

  (setf #j:on_fingermotion
        #'(lambda (x  y dx dy pressure finger-id) (internal-on-fingermotion :x x :y y :dx dx :dy dy :pressure pressure :finger-id finger-id)))
  (setf #j:on_fingerup
        #'(lambda (x  y dx dy pressure finger-id) (internal-on-fingerup  :x x :y y :dx dx :dy dy :pressure pressure :finger-id finger-id)))
  (setf #j:on_fingerdown
        #'(lambda (x  y dx dy pressure finger-id) (internal-on-fingerdown :x x :y y :dx dx :dy dy :pressure pressure :finger-id finger-id)))

  (setf #j:on_windowresized #'internal-on-windowresized)
  (setf #j:on_windowenter #'internal-on-windowenter)
  (setf #j:on_windowleave #'internal-on-windowleave)
  (setf #j:on_keydown #'internal-on-keydown)
  (setf #j:on_keyup #'internal-on-keyup)

  ;; create env
  (#j:window:eval "var JSCL_ENV = {};")

  (setf #j:JSCL_ENV:CALM_FPS *calm-fps*)

  (setf #j:JSCL_ENV:CALM_WINDOW_WIDTH *calm-window-width*)
  (setf #j:JSCL_ENV:CALM_WINDOW_HEIGHT *calm-window-height*)

  ;; https://experienceleague.adobe.com/docs/target/using/experiences/vec/mobile-viewports.html
  (if (or (<= #j:screen:width 800) (<= #j:screen:height 600))
      ;; scale canvas on mobile device
      (let ((scale (write-to-string
                    ;; track
                    ;; https://github.com/jscl-project/jscl/issues/481
                    (/ *calm-window-width* #j:screen:width)))
            (#j:canvas (#j:document:getElementById "canvas")))
        (setf #j:canvas:style:transformOrigin "center")
        (setf #j:canvas:style:transform (concatenate 'string  "scale(" scale ")")))
      ;; remove canvas scale
      (let ((#j:canvas (#j:document:getElementById "canvas")))
        (setf #j:canvas:style:transformOrigin "unset")
        (setf #j:canvas:style:transform "unset")))
  )
