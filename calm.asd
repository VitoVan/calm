(asdf:defsystem #:calm
    :description "CALM - Canvas And Lisp Magic"
    :version "0.0.32"
    :author "Vito Van"
    :license "GNU General Public License, version 2"
    :depends-on (
                 #:sdl2
                 #:sdl2-mixer
                 #:sdl2-image
                 #:str
                 #:swank
                 #:cl-cairo2)
    :pathname "./src/"
    :serial t
    :components ((:file "package")
                 (:file "config")
                 (:file "event")
                 (:file "utils")
                 (:file "calm")))
