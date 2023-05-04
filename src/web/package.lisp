(defpackage :common-lisp-user)

(defpackage :c
  ;; we do not need to write all those exports again and again,
  ;; but something seem to be not feasible in JSCL.
  ;; track: https://github.com/jscl-project/jscl/issues/471
  ;; so for now, I have to write exports in the defpackage again
  (:use cl)
  (:export
   ;; cairo_t

   :create
   :reference
   :destroy
   :status
   :save
   :restore
   :get-target
   :push-group
   :push-group-with-content
   :pop-group
   :pop-group-to-source
   :get-group-target
   :set-source-rgb
   :set-source-rgba
   :set-source
   :set-source-surface
   :get-source
   :set-antialias
   :get-antialias
   :set-dash
   :get-dash-count
   :get-dash
   :set-fill-rule
   :get-fill-rule
   :set-line-cap
   :get-line-cap
   :set-line-join
   :get-line-join
   :set-line-width
   :get-line-width
   :set-miter-limit
   :get-miter-limit
   :set-operator
   :get-operator
   :set-tolerance
   :get-tolerance
   :clip
   :clip-preserve
   :clip-extents
   :in-clip
   :reset-clip
   :rectangle-list-destroy
   :copy-clip-rectangle-list
   :fill-path
   :fill-preserve
   :fill-extents
   :in-fill
   :mask
   :mask-surface
   :paint
   :paint-with-alpha
   :stroke
   :stroke-preserve
   :stroke-extents
   :in-stroke
   :copy-page
   :show-page
   :get-reference-count
   :set-user-data
   :get-user-data

   ;; paths

   :copy-path
   :copy-path-flat
   :path-destroy
   :append-path
   :has-current-point
   :get-current-point
   :new-path
   :new-sub-path
   :close-path
   :arc
   :arc-negative
   :curve-to
   :line-to
   :move-to
   :rectangle
   :glyph-path
   :text-path
   :rel-curve-to
   :rel-line-to
   :rel-move-to
   :path-extents

   ;; transformations

   :translate
   :scale
   :rotate
   :transform
   :set-matrix
   :get-matrix
   :identity-matrix
   :user-to-device
   :user-to-device-distance
   :device-to-user
   :device-to-user-distance

   ;; text

   :select-font-face
   :set-font-size
   :set-font-matrix
   :get-font-matrix
   :set-font-options
   :get-font-options
   :set-font-face
   :get-font-face
   :set-scaled-font
   :get-scaled-font
   :show-text
   :show-glyphs
   :show-text-glyphs
   :font-extents
   :text-extents
   :glyph-extents
   :toy-font-face-create
   :toy-font-face-get-family
   :toy-font-face-get-slant
   :toy-font-face-get-weight
   :glyph-allocate
   :glyph-free
   :text-cluster-allocate
   :text-cluster-free

   :image-surface-create-from-png

   :image-surface-get-width
   :image-surface-get-height


   :defcairo

   :rrectangle
   :open-audio-if-not-yet
   :playing
   :play-music
   :play-wav
   :halt-music
   :get-ticks
   :keq
   :show-png
   :select-font-family
   :with-state

   ))

(defpackage :calm
  (:use cl)
  (:export
   :draw
   :_draw
   :config
   ))
