(in-package :c)

;;
;; c enums
;; copied from https://github.com/rpav/cl-cairo2/blob/master/src/tables.lisp
;;

(defmacro exporting-table (name definition)
  `(progn
     (defparameter ,name ,definition)
     (export ',name)))

(exporting-table table-format
  '((:CAIRO_FORMAT_ARGB32 . :argb32)
    (:CAIRO_FORMAT_RGB24 . :rgb24)
    (:CAIRO_FORMAT_A8 . :a8)
    (:CAIRO_FORMAT_A1 . :a1)
    (:CAIRO_FORMAT_RGB16_565 . :rgb16_565)
    (:CAIRO_FORMAT_RGB30 . :rgb30)))

(exporting-table table-antialias
  '((:CAIRO_ANTIALIAS_DEFAULT . :default)
    (:CAIRO_ANTIALIAS_NONE . :none)
    (:CAIRO_ANTIALIAS_GRAY . :gray)
    (:CAIRO_ANTIALIAS_SUBPIXEL . :subpixel)
    (:CAIRO_ANTIALIAS_FAST . :fast)
    (:CAIRO_ANTIALIAS_GOOD . :good)
    (:CAIRO_ANTIALIAS_BEST . :best)))

(exporting-table table-fill-rule
  '((:CAIRO_FILL_RULE_WINDING . :winding)
    (:CAIRO_FILL_RULE_EVEN_ODD . :odd)))

(exporting-table table-line-cap
  '((:CAIRO_LINE_CAP_BUTT . :butt)
    (:CAIRO_LINE_CAP_ROUND . :round)
    (:CAIRO_LINE_CAP_SQUARE . :square)))

(exporting-table table-line-join
  '((:CAIRO_LINE_JOIN_MITER . :miter)
    (:CAIRO_LINE_JOIN_ROUND . :round)
    (:CAIRO_LINE_JOIN_BEVEL . :bevel)))

(exporting-table table-operator
  '((:CAIRO_OPERATOR_CLEAR . :clear)
    (:CAIRO_OPERATOR_SOURCE . :source)
    (:CAIRO_OPERATOR_OVER . :over)
    (:CAIRO_OPERATOR_IN . :in)
    (:CAIRO_OPERATOR_OUT . :out)
    (:CAIRO_OPERATOR_ATOP . :atop)
    (:CAIRO_OPERATOR_DEST . :dest)
    (:CAIRO_OPERATOR_DEST_OVER . :dest-over)
    (:CAIRO_OPERATOR_DEST_IN . :dest-in)
    (:CAIRO_OPERATOR_DEST_OUT . :dest-out)
    (:CAIRO_OPERATOR_DEST_ATOP . :dest-atop)
    (:CAIRO_OPERATOR_XOR . :xor)
    (:CAIRO_OPERATOR_ADD . :add)
    (:CAIRO_OPERATOR_SATURATE . :saturate)
    (:CAIRO_OPERATOR_MULTIPLY . :multiply)
    (:CAIRO_OPERATOR_SCREEN . :screen)
    (:CAIRO_OPERATOR_OVERLAY . :overlay)
    (:CAIRO_OPERATOR_DARKEN . :darken)
    (:CAIRO_OPERATOR_LIGHTEN . :lighten)
    (:CAIRO_OPERATOR_COLOR_DODGE . :dodge)
    (:CAIRO_OPERATOR_COLOR_BURN . :burn)
    (:CAIRO_OPERATOR_HARD_LIGHT . :hard-light)
    (:CAIRO_OPERATOR_SOFT_LIGHT . :soft-light)
    (:CAIRO_OPERATOR_DIFFERENCE . :difference)
    (:CAIRO_OPERATOR_EXCLUSION . :exclusion)
    (:CAIRO_OPERATOR_HSL_HUE . :hue)
    (:CAIRO_OPERATOR_HSL_SATURATION . :saturation)
    (:CAIRO_OPERATOR_HSL_COLOR . :color)
    (:CAIRO_OPERATOR_HSL_LUMINOSITY . :luminosity)))

(exporting-table table-font-slant
  '((:CAIRO_FONT_SLANT_NORMAL . :normal)
    (:CAIRO_FONT_SLANT_ITALIC . :italic)
    (:CAIRO_FONT_SLANT_OBLIQUE . :oblique)))

(exporting-table table-font-weight
  '((:CAIRO_FONT_WEIGHT_NORMAL . :normal)
    (:CAIRO_FONT_WEIGHT_BOLD . :bold)))

(exporting-table table-subpixel-order
  '((:CAIRO_SUBPIXEL_ORDER_DEFAULT . :default)
    (:CAIRO_SUBPIXEL_ORDER_RGB . :rgb)
    (:CAIRO_SUBPIXEL_ORDER_BGR . :bgr)
    (:CAIRO_SUBPIXEL_ORDER_VRGB . :vrgb)
    (:CAIRO_SUBPIXEL_ORDER_VBGR . :vbgr)))

(exporting-table table-hint-style
  '((:CAIRO_HINT_STYLE_DEFAULT . :default)
    (:CAIRO_HINT_STYLE_NONE . :none)
    (:CAIRO_HINT_STYLE_SLIGHT . :slight)
    (:CAIRO_HINT_STYLE_MEDIUM . :medium)
    (:CAIRO_HINT_STYLE_FULL . :full)))

(exporting-table table-hint-metrics
 '((:CAIRO_HINT_METRICS_DEFAULT . :default)
   (:CAIRO_HINT_METRICS_OFF . :off)
   (:CAIRO_HINT_METRICS_ON . :on)))

(exporting-table table-status
  '((:CAIRO_STATUS_SUCCESS . :success)
    (:CAIRO_STATUS_NO_MEMORY . :no-memory)
    (:CAIRO_STATUS_INVALID_RESTORE . :invalid-restore)
    (:CAIRO_STATUS_INVALID_POP_GROUP . :invalid-pop-group)
    (:CAIRO_STATUS_NO_CURRENT_POINT . :no-current-point)
    (:CAIRO_STATUS_INVALID_MATRIX . :invalid-matrix)
    (:CAIRO_STATUS_INVALID_STATUS . :invalid-status)
    (:CAIRO_STATUS_NULL_POINTER . :null-pointer)
    (:CAIRO_STATUS_INVALID_STRING . :invalid-string)
    (:CAIRO_STATUS_INVALID_PATH_DATA . :invalid-path-data)
    (:CAIRO_STATUS_READ_ERROR . :read-error)
    (:CAIRO_STATUS_WRITE_ERROR . :write-error)
    (:CAIRO_STATUS_SURFACE_FINISHED . :surface-finished)
    (:CAIRO_STATUS_SURFACE_TYPE_MISMATCH . :surface-type-mismatch)
    (:CAIRO_STATUS_PATTERN_TYPE_MISMATCH . :pattern-type-mismatch)
    (:CAIRO_STATUS_INVALID_CONTENT . :invalid-content)
    (:CAIRO_STATUS_INVALID_FORMAT . :invalid-format)
    (:CAIRO_STATUS_INVALID_VISUAL . :invalid-visual)
    (:CAIRO_STATUS_FILE_NOT_FOUND . :file-not-found)
    (:CAIRO_STATUS_INVALID_DASH . :invalid-dash)
    (:CAIRO_STATUS_INVALID_DSC_COMMENT . :invalid-dsc-comment)
    (:CAIRO_STATUS_INVALID_INDEX . :invalid-index)
    (:CAIRO_STATUS_CLIP_NOT_REPRESENTABLE . :clip-not-representable)
    (:CAIRO_STATUS_TEMP_FILE_ERROR . :temp-file-error)
    (:CAIRO_STATUS_INVALID_STRIDE . :invalid-stride)
    (:CAIRO_STATUS_FONT_TYPE_MISMATCH . :font-type-mismatch)
    (:CAIRO_STATUS_USER_FONT_IMMUTABLE . :user-font-immutable)
    (:CAIRO_STATUS_USER_FONT_ERROR . :user-font-error)
    (:CAIRO_STATUS_NEGATIVE_COUNT . :negative-count)
    (:CAIRO_STATUS_INVALID_CLUSTERS . :invalid-clusters)
    (:CAIRO_STATUS_INVALID_SLANT . :invalid-slant)
    (:CAIRO_STATUS_INVALID_WEIGHT . :invalid-weight)
    (:CAIRO_STATUS_INVALID_SIZE . :invalid-size)
    (:CAIRO_STATUS_USER_FONT_NOT_IMPLEMENTED . :user-font-not-implemented)
    (:CAIRO_STATUS_DEVICE_TYPE_MISMATCH . :device-type-mismatch)
    (:CAIRO_STATUS_DEVICE_ERROR . :device-error)
    (:CAIRO_STATUS_INVALID_MESH_CONSTRUCTION . :invalid-mesh-construction)
    (:CAIRO_STATUS_DEVICE_FINISHED . :device-finished)

    (:CAIRO_STATUS_LAST_STATUS . :last-status)))

(exporting-table table-pattern-type
   '((:CAIRO_PATTERN_TYPE_SOLID . :solid)
     (:CAIRO_PATTERN_TYPE_SURFACE . :surface)
     (:CAIRO_PATTERN_TYPE_LINEAR . :linear)
     (:CAIRO_PATTERN_TYPE_RADIAL . :radial)))

(exporting-table table-extend
   '((:CAIRO_EXTEND_NONE . :none)
     (:CAIRO_EXTEND_REPEAT . :repeat)
     (:CAIRO_EXTEND_REFLECT . :reflect)
     (:CAIRO_EXTEND_PAD . :pad)))

(exporting-table table-filter
   '((:CAIRO_FILTER_FAST . :fast)
     (:CAIRO_FILTER_GOOD . :good)
     (:CAIRO_FILTER_BEST . :best)
     (:CAIRO_FILTER_NEAREST . :nearest)
     (:CAIRO_FILTER_BILINEAR . :bilinear)
     (:CAIRO_FILTER_GAUSSIAN . :gaussian)))

(exporting-table table-content
  '((:cairo_content_color . :color)
    (:cairo_content_alpha . :alpha)
    (:cairo_content_color_alpha . :color-alpha)))

(defun lookup-cairo-enum (cairo-enum table)
  (let ((enum (cdr (assoc cairo-enum table))))
    (unless enum
      (error "Could not find cairo-enum ~a in ~a." cairo-enum table))
    enum))

(defun lookup-enum (enum table)
  (let ((cairo-enum (car (rassoc enum table))))
    (unless cairo-enum
      (error "Could not find enum ~a in ~a." enum table))
    cairo-enum))

;; copy ended here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; init wasm functions
;;

(defun calljs (func-name &rest args)
  (let* ((allocated-string-pointers nil)
         (args-with-string-pointers
           (mapcar
            (lambda (x)
              (if (stringp x)
                  (let ((string-pointer (#j:allocateUTF8 x)))
                    (push string-pointer allocated-string-pointers)
                    string-pointer)
                  x))
            args)))
    ;; (format t "Calling ~A with args: ~A, args-with-string-pointers: ~A~%" func-name args args-with-string-pointers)
    (prog1 (apply (jscl::oget jscl::*root* func-name) args-with-string-pointers)
      (mapcar #j:_free allocated-string-pointers))))

(defun calljs-cairo-with-raw-args (func-name with-cairo-ptr raw-args &rest runtime-args)
  (loop
    for arg in raw-args
    for i = 0 then (incf i)
    ;; extra `when' for JSCL: Uncaught Error: CAR called on non-list argument
    for name = (when (listp arg) (first arg))
    ;; extra `when' for JSCL: Uncaught Error: CAR called on non-list argument
    for table = (when (listp arg) (symbol-value (second arg)))
    for var = (nth i runtime-args)
    when (listp arg)
      do
         (setf (nth i runtime-args) (lookup-enum-value var table))
    )
  (if with-cairo-ptr
      (apply #'calljs (concatenate 'list (list func-name (jscl::oget jscl::*root* "_cr")) runtime-args))
      (apply #'calljs (concatenate 'list (list func-name) runtime-args))))

(defmacro defcairo (name &rest args)
  `(defcairo-raw ,name t nil ,@args))

(defmacro defcairo-raw (name with-cairo-ptr result-parser &rest args)
  "e.g.,

no enum:
  (defcairo set-antialias antialias)
call:
  (set-antialias 6)

with enum
(defcairo set-antialias (antialias table-antialias))
call:
  (set-antialias :best)

https://www.cairographics.org/manual/cairo-cairo-t.html#cairo-antialias-t

with or without custom Lisp function name:
(defcairo set-source-rgba red green blue alpha)
(defcairo (set-source-rgba my-set-source-rgba) red green blue alpha)
"
  (let* ((js-func-name
           (concatenate 'string
                        "_cairo_"
                        (substitute #\_ #\-
                                    (string-downcase
                                     (write-to-string
                                      (if (listp name) (first name) name))))))
         (lisp-func-name (if (listp name) (second name) name))
         (args-without-enum-table
           (mapcar (lambda (x) (if (listp x) (first x) x)) args)))
    (if result-parser
        `(progn
           (defun ,lisp-func-name ,args-without-enum-table
             ;; (format t "lisp-func-name ~A, args-without-enum-table ~A~%" ',lisp-func-name ',args-without-enum-table)
             (funcall ,result-parser (calljs-cairo-with-raw-args ,js-func-name ,with-cairo-ptr ',args ,@args-without-enum-table)))
           (export ',lisp-func-name))
        `(progn
           (defun ,lisp-func-name ,args-without-enum-table
             ;; (format t "lisp-func-name ~A, args-without-enum-table ~A~%" ',lisp-func-name ',args-without-enum-table)
             (calljs-cairo-with-raw-args ,js-func-name ,with-cairo-ptr ',args ,@args-without-enum-table))
           (export ',lisp-func-name))
        )))

;; cairo_t
;; https://www.cairographics.org/manual/cairo-cairo-t.html
(defcairo create target)
(defcairo reference)
(defcairo destroy)
(defcairo status)
(defcairo save)
(defcairo restore)
(defcairo get-target)
(defcairo push-group)
(defcairo push-group-with-content content)
(defcairo pop-group)
(defcairo pop-group-to-source)
(defcairo get-group-target)
(defcairo set-source-rgb red green blue)
(defcairo set-source-rgba red green blue alpha)
(defcairo set-source source)
(defcairo set-source-surface surface x y)
(defcairo get-source)
(defcairo set-antialias (antialias table-antialias))
(defcairo get-antialias)
(defcairo set-dash const dashes int num-dashes offset)
(defcairo get-dash-count)
(defcairo get-dash dashes offset)
(defcairo set-fill-rule (fill-rule table-fill-rule))
(defcairo get-fill-rule)
(defcairo set-line-cap (line-cap table-line-cap))
(defcairo get-line-cap)
(defcairo set-line-join (line-join table-line-join))
(defcairo get-line-join)
(defcairo set-line-width width)
(defcairo get-line-width)
(defcairo set-miter-limit limit)
(defcairo get-miter-limit)
(defcairo set-operator (op table-operator))
(defcairo get-operator)
(defcairo set-tolerance tolerance)
(defcairo get-tolerance)
(defcairo clip)
(defcairo clip-preserve)
(defcairo clip-extents x1 y1 x2 y2)
(defcairo in-clip x y)
(defcairo reset-clip)
(defcairo rectangle-list-destroy rectangle-list)
(defcairo copy-clip-rectangle-list)
(defcairo (fill fill-path))
(defcairo fill-preserve)
(defcairo fill-extents x1 y1 x2 y2)
(defcairo-raw in-fill t #'(lambda (x) (not (zerop x))) x y)
(defcairo mask pattern)
(defcairo mask-surface surface surface-x surface-y)
(defcairo paint)
(defcairo paint-with-alpha alpha)
(defcairo stroke)
(defcairo stroke-preserve)
(defcairo stroke-extents x1 y1 x2 y2)
(defcairo in-stroke x y)
(defcairo copy-page)
(defcairo show-page)
(defcairo get-reference-count)
(defcairo set-user-data destroy)
(defcairo get-user-data key)

;; paths
;; https://www.cairographics.org/manual/cairo-Paths.html

(defcairo copy-path)
(defcairo copy-path-flat)
(defcairo path-destroy path)
(defcairo append-path path)
(defcairo has-current-point)
(defcairo get-current-point x y)
(defcairo new-path)
(defcairo new-sub-path)
(defcairo close-path)
(defcairo arc xc yc radius angle1 angle2)
(defcairo arc-negative xc yc radius angle1 angle2)
(defcairo curve-to x1 y1 x2 y2 x3 y3)
(defcairo line-to x y)
(defcairo move-to x y)
(defcairo rectangle x y width height)
(defcairo glyph-path glyphs int num-glyphs)
(defcairo text-path const char *utf8)
(defcairo rel-curve-to dx1 dy1 dx2 dy2 dx3 dy3)
(defcairo rel-line-to dx dy)
(defcairo rel-move-to dx dy)
(defcairo path-extents x1 y1 x2 y2)

;; Transformations
;; https://www.cairographics.org/manual/cairo-Transformations.html

(defcairo translate tx ty)
(defcairo scale sx sy)
(defcairo rotate angle)
(defcairo transform matrix)
(defcairo set-matrix matrix)
(defcairo get-matrix matrix)
(defcairo identity-matrix)
(defcairo user-to-device x y)
(defcairo user-to-device-distance dx dy)
(defcairo device-to-user x y)
(defcairo device-to-user-distance dx dy)

;; text
;; https://www.cairographics.org/manual/cairo-text.html

(defcairo select-font-face family slant weight)
(defcairo set-font-size size)
(defcairo set-font-matrix matrix)
(defcairo get-font-matrix matrix)
(defcairo set-font-options options)
(defcairo get-font-options options)
(defcairo set-font-face font-face)
(defcairo get-font-face)
(defcairo set-scaled-font scaled-font)
(defcairo get-scaled-font)
(defcairo show-text utf8)
(defcairo show-glyphs glyphs num-glyphs)
(defcairo show-text-glyphs utf8 utf8-len glyphs num-glyphs clusters num-clusters cluster-flags)
(defcairo font-extents extents)
(defcairo text-extents utf8 extents)
(defcairo glyph-extents glyphs num-glyphs extents)
(defcairo toy-font-face-create family slant weight)
(defcairo toy-font-face-get-family font-face)
(defcairo toy-font-face-get-slant font-face)
(defcairo toy-font-face-get-weight font-face)
(defcairo glyph-allocate num-glyphs)
(defcairo glyph-free glyphs)
(defcairo text-cluster-allocate num-clusters)
(defcairo text-cluster-free clusters)

;; Image Surfaces
;; https://www.cairographics.org/manual/cairo-Image-Surfaces.html
(defcairo-raw image-surface-get-width nil nil surface)
(defcairo-raw image-surface-get-height nil nil surface)

;; PNG Support
;; https://www.cairographics.org/manual/cairo-PNG-Support.html

(defcairo-raw image-surface-create-from-png nil nil filename)

(defun lookup-enum-value (enum table)
  (let ((enum-position (position enum table :test #'(lambda (a b) (eq a (cdr b))))))
    (unless enum-position
      (error "Could not find enum ~a in ~a." enum table))
    enum-position))
