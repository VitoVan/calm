(in-package :calm)

(defun internal-on-textinput (text)
  (on-textinput text))

(defun internal-on-keydown (key)
  (setf *calm-redraw* t)
  (on-keydown key))
(defun internal-on-keyup (key)
  (setf *calm-redraw* t)
  (on-keyup key))

(defun internal-on-mousewheel (x y direction)
  (setf *calm-redraw* t)
  (on-mousewheel x y direction))
(defun internal-on-mousemotion (&key x y)
  (setf *calm-state-mouse-x* x
        *calm-state-mouse-y* y
        *calm-redraw* t
        *calm-state-mouse-just-clicked* nil)
  (on-mousemotion :x x :y y))
(defun internal-on-mousebuttonup (&key button x y clicks)
  (setf
   *calm-state-mouse-up* button
   *calm-state-mouse-down* nil
   *calm-redraw* t
   *calm-state-mouse-just-clicked* t)
  (on-mousebuttonup :button button :x x :y y :clicks clicks))
(defun internal-on-mousebuttondown (&key button x y clicks)
  (setf
   *calm-state-mouse-up* nil
   *calm-state-mouse-down* button
   *calm-redraw* t
   *calm-state-mouse-just-clicked* nil)
  (on-mousebuttondown :button button :x x :y y :clicks clicks))
(defun internal-on-windowresized (width height)
  (setf *calm-redraw* t)
  (on-windowresized width height))
(defun internal-on-windowenter ()
  (setf *calm-state-mouse-inside-window* t
        *calm-redraw* t)
  (on-windowenter))
(defun internal-on-windowleave ()
  (setf *calm-state-mouse-inside-window* nil
        *calm-redraw* t)
  (on-windowleave))


(defun on-textinput (text) (declare (ignore text)))

(defun on-keydown (key) (declare (ignore key)))
(defun on-keyup (key) (declare (ignore key)))

(defun on-mousewheel (x y direction) (declare (ignore x y direction)))
(defun on-mousemotion (&key x y) (declare (ignore x y)))
(defun on-mousebuttonup (&key button x y clicks) (declare (ignore button x y clicks)))
(defun on-mousebuttondown (&key button x y clicks) (declare (ignore button x y clicks)))
(defun on-windowresized (width height) (declare (ignore width height)))
(defun on-windowenter ())
(defun on-windowleave ())
