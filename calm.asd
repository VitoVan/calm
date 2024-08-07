(asdf:defsystem #:calm
  :description "CALM - Canvas Aided Lisp Magic"
  :version "1.3.1"
  :author "Vito Van"
  :license "GNU General Public License, version 2"
  :depends-on (
               #:sdl2
               #:sdl2-mixer
               #:sdl2-image
               #:str
               #:swank
               #:bt-semaphore
               #:cl-cairo2
               #:cl-gobject-introspection)
  :pathname "./src/"
  :serial t
  :components ((:file "package")
               (:file "config")
               (:file "events")
               (:file "utils")
               (:file "c")
               (:file "cairo")
               (:file "fontconfig")
               (:file "calm")))
