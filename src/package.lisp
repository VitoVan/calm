(uiop:define-package :c
    (:use :cl)
  (:use-reexport :cl-cairo2)
  (:export
   :with-state
   :rrectangle
   :markup->layout
   :show-layout
   :show-markup))

(defpackage :calm-utils
  (:use :cl)
  (:export
   :load-from-app
   :load-from-calm
   :get-from-env-or-ask
   :exec
   :exec-if
   :touch-file
   :copy-file
   :copy-dir
   :calm-log
   :calm-log-fancy
   :open-audio-if-not-yet
   :play-music
   :play-wav
   :halt-music
   :set-cursor))

(defpackage :calm
  (:use :cl)
  (:local-nicknames
   (:u :calm-utils))
  (:export
   :calm-init
   :calm-start
   :calm-load-and-start))
