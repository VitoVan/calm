(mapcar
 #'(lambda (lib)
     (let* ((lib-pathname (cffi:foreign-library-pathname lib))
            (lib-path-namestring (namestring lib-pathname)))
       (unless (str:starts-with-p "/System" lib-path-namestring)
         (uiop:run-program
          ;; this has to be done by `cp` because `uiop:copy-file` has some weird behaviours on MSYS2
          (str:concat "cp "
                      (namestring
                       #+linux
                       (uiop:merge-pathnames* lib-pathname "/usr/lib64/")
                       #+darwin
                       (uiop:merge-pathnames* lib-pathname "/usr/local/lib/")
                       #+win32
                       (uiop:merge-pathnames* lib-pathname "/mingw64/bin/"))
                      " ./lib/" )))))
 (cffi:list-foreign-libraries :loaded-only t))

(quit)
