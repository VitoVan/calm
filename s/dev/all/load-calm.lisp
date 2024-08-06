;; the existence of this file is to avoid the tricky shitty escaping of quotation marks
#-quicklisp
(load "./quicklisp/setup.lisp")
;; https://groups.google.com/g/quicklisp/c/wrULkRePVE4
#+win32
(quicklisp-client::make-system-index "./quicklisp/local-projects/")
#-calm
(load "./calm.asd")
#-calm
(ql:quickload :calm)
