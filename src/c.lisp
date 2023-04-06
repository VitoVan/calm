(in-package #:c)

;;
;; drawing
;;

(defun rrectangle (x y width height &key (cr cl-cairo2:*context*) (radius 8))
  "rounded rectangle"
  (let ((degrees (/ pi 180)))
    (c:new-sub-path cr)
    (c:arc (- (+ x width) radius) (+ y radius) radius (* -90 degrees) (* 0 degrees) cr)
    (c:arc (- (+ x width) radius) (- (+ y height) radius) radius (* 0 degrees) (* 90 degrees) cr)
    (c:arc (+ x radius) (- (+ y height) radius) radius (* 90 degrees) (* 180 degrees) cr)
    (c:arc (+ x radius) (+ y radius) radius (* 180 degrees) (* 270 degrees) cr)
    (c:close-path)))

;;
;; text utilities
;;

(defun escape-char (char s)
  "char escaping for pango-markup"
  (case char
    (#\< (write-string "&lt;" s))
    (#\> (write-string "&gt;" s))
    (#\& (write-string "&amp;" s))
    (#\' (write-string "&#39;" s))
    (T (write-char char s))))

(defun escape-string (string)
  "string escaping for Pango Markup"
  (with-output-to-string (o)
    (loop for char across string
          do (escape-char char o))))

(defun markup->layout (markup &key
                                      (font-size calm::*calm-default-font-size*)
                                      (width calm::*calm-window-width*)
                                      (height calm::*calm-window-height*)
                                      (align :left)
                                      (ellipsize :end)
                                      (cr cl-cairo2:*context*))

  (let* ((ns-pango (gir:require-namespace "Pango"))
         (ns-pango-cairo (gir:require-namespace "PangoCairo"))
         (font-desc (gir:invoke (ns-pango "FontDescription" 'new)))
         (layout (gir:invoke (ns-pango-cairo 'create_layout) (slot-value cr 'cairo:pointer))))

    (gir:invoke (font-desc 'set_family) calm::*calm-default-font-family*)
    (gir:invoke (font-desc 'set_weight) (gir:nget ns-pango "Weight" :normal))
    (gir:invoke (font-desc 'set_absolute_size) (coerce (* font-size (gir:nget ns-pango "SCALE")) 'double-float))

    (gir:invoke (layout 'set_font_description) font-desc)
    (gir:invoke (layout 'set_width)
                (if (= width -1) -1 (gir:invoke (ns-pango 'units_from_double) (coerce width 'double-float))))
    (gir:invoke (layout 'set_height)
                (if (= height -1) -1 (gir:invoke (ns-pango 'units_from_double) (coerce height 'double-float))))
    (gir:invoke (layout 'set_markup) markup -1)
    (gir:invoke (layout 'set_ellipsize) (gir:nget ns-pango "EllipsizeMode" ellipsize))
    (gir:invoke (layout 'set_alignment) (gir:nget ns-pango "Alignment" align))
    (multiple-value-bind (_ w h)
        (gir:invoke (layout 'get_pixel_size))
      (declare (ignore _))
      (values layout w h))))

(defun show-layout (layout &key (cr cl-cairo2:*context*))
  "show Pango Layout"
  (let ((ns-pango-cairo (gir:require-namespace "PangoCairo")))
    (gir:invoke (ns-pango-cairo 'show_layout) (slot-value cr 'cairo:pointer) layout)))

(defun show-markup (markup &key
                             (font-size calm::*calm-default-font-size*)
                             (width calm::*calm-window-width*)
                             (height calm::*calm-window-height*)
                             (align :left)
                             (ellipsize :end)
                             (cr cl-cairo2:*context*))

  "show Pango Markup"
  (multiple-value-bind (layout w h)
      (markup->layout markup
                            :font-size font-size
                            :width width
                            :height height
                            :align align
                            :ellipsize ellipsize
                            :cr cr)
    (declare (ignore w h))
    (show-layout layout :cr cr)))


;;
;; cairo state
;;

(defmacro with-state ( &body body)
  `(progn
     (cl-cairo2:save)
     ,@body
     (cl-cairo2:restore)))
