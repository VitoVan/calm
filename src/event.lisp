(in-package :calm)

(defun on-textinput (text) (declare (ignore text)))
(defun on-keydown (key) (declare (ignore key)))
(defun on-keyup (key) (declare (ignore key)))
(defun on-mousewheel (x y direction) (declare (ignore x y direction)))
(defun on-mousemotion (&key x y) (declare (ignore x y)))
(defun on-mousebuttonup (&key button x y clicks) (declare (ignore button x y clicks)))
(defun on-mousebuttondown (&key button x y clicks) (declare (ignore button x y clicks)))
