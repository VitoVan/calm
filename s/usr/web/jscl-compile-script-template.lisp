(jscl:bootstrap)
;; track: https://github.com/jscl-project/jscl/issues/474
(pushnew :jscl *features*)
(jscl:compile-application
     (concatenate 'list
                  (list "__CALM_HOME__src/web/package.lisp"
                        "__CALM_HOME__src/web/cairo.lisp"
                        "__CALM_HOME__src/c.lisp"
                        "__CALM_HOME__src/calm.lisp"
                        "__CALM_HOME__src/config.lisp"
                        "__CALM_HOME__src/events.lisp")
                  '__LISP_FILES_LIST__
                  (list "__CALM_HOME__src/web/post.lisp"))
     "__APP_OUTPUT_FILE__")
(quit)
