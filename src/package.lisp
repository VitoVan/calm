(defpackage :calm-utils
  (:use :cl)
  (:local-nicknames
   (:c :cl-cairo2))
  (:export
   :wav-play
   :wav-halt
   :wav-is-playing
   :set-cursor))

(defpackage :calm
  (:use :cl)
  (:local-nicknames
   (:c :cl-cairo2)
   (:u :calm-utils))
  (:export
   :calm-start))
