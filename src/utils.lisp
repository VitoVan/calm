(in-package #:calm-utils)

(defmacro calm-log-fancy (control-string &rest rest)
  `(progn
     (format t "~%")
     (format t "
  ____      _      _       __  __
 / ___|    / \\    | |     |  \\/  |
| |       / _ \\   | |     | |\\/| |
| |___   / ___ \\  | |___  | |  | |
 \\____| /_/   \\_\\ |_____| |_|  |_|

CALM: ~A, ~A: ~A~%
" (slot-value (asdf:find-system 'calm) 'asdf:version) (lisp-implementation-type) (lisp-implementation-version))
     (format t ,control-string ,@rest)
     (format t "~%~%")))

(defmacro calm-log (control-string &rest rest)
  `(format t ,control-string ,@rest))

(defun load-from-app (pathname)
  "load lisp files from the `CALM_APP_DIR' directory,
e.g. use (load-from-app \"config.lisp\") to load your custom configuration file"
  (load (or
         (uiop:absolute-pathname-p pathname)
         (uiop:merge-pathnames* pathname (uiop:getenv "CALM_APP_DIR")))))

(defun load-from-calm (pathname)
  "load lisp files from the `CALM_HOME' directory,
e.g. use (load-from-calm \"config.lisp\") to load your custom configuration file"
  (load (or
         (uiop:absolute-pathname-p pathname)
         (uiop:merge-pathnames* pathname (uiop:getenv "CALM_HOME")))))

;;
;; command utilities
;;

(defun exec (command &key (expected-exit-code 0) (ignore-error-status nil))
  (format t "EXECUTING CMD: ~A~%" command)
  (let ((exit-code
          (nth-value 2 (uiop:run-program command :output :interactive :input :interactive :error-output t :ignore-error-status t))))
    (if (= exit-code expected-exit-code)
        exit-code
        (if ignore-error-status
            exit-code
            (error (format nil "exec failed: ~A, exit code: ~A~%" command exit-code))))))

(defun exec-if (test then else &key (re-exec-then nil) (expected-exit-code 0) (then-expected-exit-code 0) (else-expected-exit-code 0))
  "exec `test', if the exit-code is as expected, then exec `then', else run `else'.
if `re-exec-then' is T, then exec `then' again after the exec of `else'
"
  (let ((exit-code
          (nth-value 2 (uiop:run-program test :output t :error-output t :ignore-error-status t))))
    (if (= exit-code expected-exit-code)
        (exec then :expected-exit-code then-expected-exit-code)
        (progn
          (exec else :expected-exit-code else-expected-exit-code)
          (when re-exec-then (exec then :expected-exit-code then-expected-exit-code))))))

;;
;; file utilities
;;

;; thank you, Zach Beane
;; https://lisptips.com/post/11136367093/touching-a-file
(defun touch-file (filename)
  (open filename :direction :probe :if-does-not-exist :create))

(defun copy-file (from to)
  "`uiop:copy-file' will remove some file attributes,
so let's use the platform command"
  (setf from (if (stringp from) from (uiop:native-namestring from)))
  (setf to (if (stringp to) to (uiop:native-namestring to)))
  (format t "copying file from ~A to ~A~%" from to)
  (if (probe-file from)
      (exec
       #-win32
       (str:concat "cp \"" from "\" \"" to "\"")
       #+win32
       (str:concat "copy /y \"" from "\" \"" to "\""))
      (format t "File NOT Found: ~A~%" from)))

(defun copy-dir (from to)
  "Let's just cut the shit of loading `cl-fad' and writing
yet another clumsy common lisp utility function
to recursively copy file system directories and lost all the file attributes.
Just use the `cp' command for whoever's sake"
  (setf from (if (stringp from) from (uiop:native-namestring from)))
  (setf to (if (stringp to) to (uiop:native-namestring to)))
  (format t "copying dir from ~A to ~A~%" from to)
  (if (probe-file from)
      (exec
       #-win32
       (str:concat "cp -R \"" from "\" \"" to "\"")
       #+win32
       (str:concat "robocopy /mir \"" from " \" \"" to " \"") ;; extra space to avoid escaping \"
       ;; I don't know why those Windows fuckers decide to
       ;; make the `exit-code' to 1 when `robocopy' ran successfully.
       ;; I won't argue with anyone, but this worth a comment.
       ;;
       ;; https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/robocopy
       ;;
       ;;    0 No files were copied.
       ;;    0 No failure was encountered.
       ;;    0 No files were mismatched.
       ;;    0 The files already exist in the destination directory; therefore, the copy operation was skipped.
       ;;
       ;;    1 All files were copied successfully.
       ;;
       #+win32
       :expected-exit-code
       #+win32 1
       :ignore-error-status t)
      (format t "Directory NOT Found: ~A~%" from)))

#-jscl
(defun set-cursor (type)
  (cond
    ((equal type :hand) (sdl2-ffi.functions:sdl-set-cursor (sdl2-ffi.functions:sdl-create-system-cursor sdl2-ffi:+sdl-system-cursor-hand+)))
    ((equal type :arrow) (sdl2-ffi.functions:sdl-set-cursor (sdl2-ffi.functions:sdl-create-system-cursor sdl2-ffi:+sdl-system-cursor-arrow+)))))

;;
;; env utils
;;
(defun get-from-env-or-ask (var-name &optional default-value)
  "`var-name' should be a string, or something could be fed to (string x)
this function will:
1. (uiop:getenv \"VAR_NAME\"), return the value if it were found
2. ask user to input a new value for this, and return"
  (let* ((env-name (str:replace-all "-" "_" (str:upcase (string var-name))))
         (env-value (uiop:getenv env-name))
         (current-value (or env-value default-value)))

    ;; if there exists default value or env value,
    ;; and the user don't want to be bothered (by default, we don't bother them),
    (if (and (or default-value env-value) (not (uiop:getenv "CALM_ASK_ME")))
        ;; the current value will be used, no question will be asked
        current-value
        ;; ask the user, and provide the current value
        (or env-value
            (progn
              (format t "Please set value for \"~A\" [default: ~A]: ~%" (str:replace-all "-" " " (string var-name)) current-value)
              (let* ((input-line (read-line))
                     (result (if (str:empty? input-line)
                                 current-value
                                 input-line)))
                ;; update ENV to match the user input
                (setf (uiop:getenv env-name) (princ-to-string result))
                result))))))
