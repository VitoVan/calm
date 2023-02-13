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
