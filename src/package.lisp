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
   :play-wav
   :halt-wav
   :play-music
   :halt-music
   :audio-is-playing
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
