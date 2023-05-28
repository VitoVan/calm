(in-package :calm)

(defun internal-on-textinput (text)
  (c:open-audio-if-not-yet)
  (when (fboundp 'on-textinput)
    (on-textinput text)))

(defun internal-on-keydown (key)
  (c:open-audio-if-not-yet)
  (setf *calm-redraw* t)
  (when (fboundp 'on-keydown)
    (on-keydown key)))
(defun internal-on-keyup (key)
  (c:open-audio-if-not-yet)
  (setf *calm-redraw* t)
  (when (fboundp 'on-keyup)
    (on-keyup key)))

(defun internal-on-mousewheel (x y direction)
  (c:open-audio-if-not-yet)
  (setf *calm-redraw* t)
  (when (fboundp 'on-mousewheel)
    (on-mousewheel x y direction)))
(defun internal-on-mousemotion (&key x y)
  (c:open-audio-if-not-yet)
  (setf *calm-state-mouse-x* x
        *calm-state-mouse-y* y
        *calm-redraw* t
        *calm-state-mouse-just-clicked* nil)
  (when (fboundp 'on-mousemotion)
    (on-mousemotion :x x :y y)))
(defun internal-on-mousebuttonup (&key button x y clicks)
  (c:open-audio-if-not-yet)
  (setf
   *calm-state-mouse-up* button
   *calm-state-mouse-down* nil
   *calm-redraw* t
   *calm-state-mouse-just-clicked* t)
  (when (fboundp 'on-mousebuttonup)
    (on-mousebuttonup :button button :x x :y y :clicks clicks)))
(defun internal-on-mousebuttondown (&key button x y clicks)
  (c:open-audio-if-not-yet)
  (setf
   *calm-state-mouse-up* nil
   *calm-state-mouse-down* button
   *calm-redraw* t
   *calm-state-mouse-just-clicked* nil)
  (when (fboundp 'on-mousebuttondown)
    (on-mousebuttondown :button button :x x :y y :clicks clicks)))

(defun internal-on-fingermotion (&key x  y dx dy pressure finger-id)
  (c:open-audio-if-not-yet)
  (setf *calm-state-finger-x* x
        *calm-state-finger-y* y
        *calm-redraw* t
        *calm-state-finger-just-clicked* nil)
  (format t "finger motion~%")
  (when (fboundp 'on-fingermotion)
    (on-fingermotion :x x :y y :dx dx :dy dy :pressure pressure :finger-id finger-id)))
(defun internal-on-fingerup (&key x  y dx dy pressure finger-id)
  (c:open-audio-if-not-yet)
  (setf
   *calm-state-finger-up* finger-id
   *calm-state-finger-down* nil
   *calm-redraw* t
   *calm-state-finger-just-clicked* t)
  (format t "finger up~%")
  (when (fboundp 'on-fingerup)
    (on-fingerup :x x :y y :dx dx :dy dy :pressure pressure :finger-id finger-id)))
(defun internal-on-fingerdown (&key x  y dx dy pressure finger-id)
  (c:open-audio-if-not-yet)
  (setf
   *calm-state-finger-up* nil
   *calm-state-finger-down* finger-id
   *calm-redraw* t
   *calm-state-finger-just-clicked* nil)
  (format t "finger down~%")
  (when (fboundp 'on-fingerdown)
    (on-fingerdown :x x :y y :dx dx :dy dy :pressure pressure :finger-id finger-id)))

(defun internal-on-windowresized (width height)
  (setf *calm-redraw* t)
  (when (fboundp 'on-windowresized)
    (on-windowresized width height)))
(defun internal-on-windowenter ()
  (setf *calm-state-mouse-inside-window* t
        *calm-redraw* t)
  (when (fboundp 'on-windowenter)
    (on-windowenter)))
(defun internal-on-windowleave ()
  (setf *calm-state-mouse-inside-window* nil
        *calm-redraw* t)
  (when (fboundp 'on-windowleave)
    (on-windowleave)))
