(uiop:define-package :c
    (:use :cl)
  (:use-reexport :cl-cairo2)
  (:export
   :rrectangle
   :open-audio-if-not-yet
   :play-music
   :play-wav
   :playing
   :halt-music
   :get-ticks
   :keq
   :show-png
   :select-font-family
   :with-state
   
   :markup->layout
   :show-layout
   :show-markup
   ))

(defpackage :calm-utils
  (:use :cl)
  (:export
   :load-from-app
   :load-from-calm
   :get-from-env-or-ask
   :set-cursor
   :exec
   :exec-if
   :touch-file
   :copy-file
   :copy-dir
   :calm-log
   :calm-log-fancy))

(defpackage :calm
  (:use :cl)
  (:local-nicknames
   (:u :calm-utils))
  (:export
   :calm-init
   :calm-start
   :calm-load-and-start))

(defpackage :fontconfig
  (:use :cl)
  (:export
   :maybe-enum
   :weight
   :slant
   :fc-init
   :fc-create-pattern
   :fc-pattern-add-string
   :fc-pattern-add-integer
   :destroy-pattern))
