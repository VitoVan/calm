(in-package :calm)

;; init config
(defparameter *calm-x* 0)
(defparameter *calm-y* 0)
(defparameter *calm-width* 600)
(defparameter *calm-height* 150)
(defparameter *calm-title* "CALM")
(defparameter *calm-flags* '(:shown :allow-highdpi))
(defparameter *calm-delay* nil)

;; runtime variables
(defparameter *calm-dpi-scale* 1)
(defparameter *calm-state-mouse-x* 0)
(defparameter *calm-state-mouse-y* 0)
(defparameter *calm-state-mouse-up* nil)
(defparameter *calm-state-mouse-down* nil)
