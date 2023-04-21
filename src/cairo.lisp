(in-package #:cl-cairo2)

(cffi:defcfun ("cairo_ft_font_face_create_for_pattern" cairo_ft_font_face_create_for_pattern) :pointer
  (pattern :pointer))

(define-flexible (select-font-face-fc pointer family slant weight)
  (let ((pattern (fontconfig:fc-create-pattern)))
    (fontconfig:fc-pattern-add-string pattern "family" family)
    (fontconfig:fc-pattern-add-integer pattern "slant" (fontconfig:maybe-enum 'fontconfig:slant slant))
    (fontconfig:fc-pattern-add-integer pattern "weight" (fontconfig:maybe-enum 'fontconfig:weight weight))
    (let ((font-face (cairo_ft_font_face_create_for_pattern pattern)))
      (cairo_set_font_face pointer font-face)
      (cairo_font_face_destroy font-face))
    (fontconfig:destroy-pattern pattern)))
