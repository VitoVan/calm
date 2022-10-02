(defpackage :calm-utils
  (:use :cl)
  (:local-nicknames
   (:c :cl-cairo2))
  (:export
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
   :calm-start))
