(in-package :calm)

;; init config
(defparameter *calm-x* :centered)
(defparameter *calm-y* :centered)
(defparameter *calm-width* 600)
(defparameter *calm-height* 150)
(defparameter *calm-title* "CALM")
(defparameter *calm-flags* '(:shown :allow-highdpi))
(defparameter *calm-delay* 42)
(defparameter *calm-redraw* t
  "The canvas will be painted again and again by calling the `draw` function,
   setting it to `NIL` means you don't want the canvas to be painted again,
   setting it back to `t` will paint the canvas again and again by calling the `draw` function, again.
  ")

;; runtime variables
(defparameter *calm-dpi-scale* 1)
(defparameter *calm-state-mouse-inside-window* nil)
(defparameter *calm-state-mouse-x* 0)
(defparameter *calm-state-mouse-y* 0)
(defparameter *calm-state-mouse-up* nil)
(defparameter *calm-state-mouse-down* nil)
(defparameter *calm-state-mouse-just-clicked* nil)
