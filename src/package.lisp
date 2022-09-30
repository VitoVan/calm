(defpackage :calm-utils
  (:use :cl)
  (:local-nicknames
   (:c :cl-cairo2))
  (:export
   :mix-play
   :mix-halt
   :mix-is-playing
   :set-cursor))

(defpackage :calm
  (:use :cl)
  (:local-nicknames
   (:c :cl-cairo2)
   (:u :calm-utils))
  (:export
   :calm-start))
