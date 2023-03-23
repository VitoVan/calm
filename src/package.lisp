(defpackage :calm-utils
  (:use :cl)
  (:local-nicknames
   (:c :cl-cairo2))
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
   (:c :cl-cairo2)
   (:u :calm-utils))
  (:export
   :calm-init
   :calm-start
   :calm-load-and-start))
