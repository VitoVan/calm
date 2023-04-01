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
" calm::*calm-version* (lisp-implementation-type) (lisp-implementation-version))
     (format t ,control-string ,@rest)
     (format t "~%~%")))

(defmacro calm-log (control-string &rest rest)
  `(format t ,control-string ,@rest))

(defun set-cursor (type)
  (cond
    ((equal type :hand) (sdl2-ffi.functions:sdl-set-cursor (sdl2-ffi.functions:sdl-create-system-cursor sdl2-ffi:+sdl-system-cursor-hand+)))
    ((equal type :arrow) (sdl2-ffi.functions:sdl-set-cursor (sdl2-ffi.functions:sdl-create-system-cursor sdl2-ffi:+sdl-system-cursor-arrow+)))))

(sdl2-mixer:init)

(defun open-audio-if-not-yet ()
  (unless calm::*calm-state-audio-open*
    ;;
    ;; if we put the following code outside of this function,
    ;; it will open-audio right after the library is loaded,
    ;; which will cause problem for save-lisp-and-die
    ;;
    (sdl2-mixer:open-audio
     calm::*calm-music-frequency*
     calm::*calm-music-format*
     ;; channels: number of channels (1 is mono, 2 is stereo, etc).
     calm::*calm-music-channels*
     ;;
     ;; chunksize: audio buffer size in sample FRAMES (total samples divided by channel count).
     ;; I thought it should be: (* calm::*calm-music-channels* calm::*calm-music-frequency*)
     ;; but, it was delayed, I have to set it to `1024' according to this:
     ;; https://stackoverflow.com/questions/983997/i-have-an-unintended-delay-in-playing-a-mix-chunk
     calm::*calm-music-chunksize*
     )))

(defun play-music (pathname &optional (loops 0))
  (open-audio-if-not-yet)
  (let* ((music-pathname (or
                          (uiop:absolute-pathname-p pathname)
                          (uiop:merge-pathnames* pathname (uiop:getenv "CALM_APP_DIR"))))
         (music-object-cache (cdr (assoc music-pathname calm::*calm-state-loaded-audio*)))
         (music-object (or music-object-cache (sdl2-mixer:load-music music-pathname))))
    (unless music-object-cache
      (push (cons music-pathname music-object) calm::*calm-state-loaded-audio*))
    (sdl2-mixer:play-music music-object loops)))

(defun play-wav (pathname &optional (loops 0))
  (open-audio-if-not-yet)
  (let* ((wav-pathname (or
                          (uiop:absolute-pathname-p pathname)
                          (uiop:merge-pathnames* pathname (uiop:getenv "CALM_APP_DIR"))))
         (wav-object-cache (cdr (assoc wav-pathname calm::*calm-state-loaded-audio*)))
         (wav-object (or wav-object-cache (sdl2-mixer:load-wav wav-pathname))))
    (unless wav-object-cache
      (push (cons wav-pathname wav-object) calm::*calm-state-loaded-audio*))
    (sdl2-mixer:play-channel -1 wav-object loops)))

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

(defun halt-music ()
  (sdl2-mixer:halt-music))

;;
;; cairo utilities
;;

(defmacro with-cairo-state (&body body)
  `(progn
     (c:save)
     ,@body
     (c:restore)))

;;
;; text utilities
;;


(defun escape-char (char s)
  (case char
    (#\< (write-string "&lt;" s))
    (#\> (write-string "&gt;" s))
    (#\& (write-string "&amp;" s))
    (#\' (write-string "&#39;" s))
    (T (write-char char s))))

(defun escape-string (string)
  (with-output-to-string (o)
    (loop for char across string
          do (escape-char char o))))

(defun create-markup-layout (markup &key
                                      (font-size calm::*calm-default-font-size*)
                                      (width calm::*calm-window-width*)
                                      (height calm::*calm-window-height*)
                                      (align :left)
                                      (ellipsize :end)
                                      (cr cl-cairo2:*context*))

  (let* ((ns-pango (gir:require-namespace "Pango"))
         (ns-pango-cairo (gir:require-namespace "PangoCairo"))
         (font-desc (gir:invoke (ns-pango "FontDescription" 'new)))
         (layout (gir:invoke (ns-pango-cairo 'create_layout) (slot-value cr 'cairo:pointer))))

    (gir:invoke (font-desc 'set_family) calm::*calm-default-font-family*)
    (gir:invoke (font-desc 'set_weight) (gir:nget ns-pango "Weight" :normal))
    (gir:invoke (font-desc 'set_absolute_size) (coerce (* font-size (gir:nget ns-pango "SCALE")) 'double-float))

    (gir:invoke (layout 'set_font_description) font-desc)
    (gir:invoke (layout 'set_width)
                (if (= width -1) -1 (gir:invoke (ns-pango 'units_from_double) (coerce width 'double-float))))
    (gir:invoke (layout 'set_height)
                (if (= height -1) -1 (gir:invoke (ns-pango 'units_from_double) (coerce height 'double-float))))
    (gir:invoke (layout 'set_markup) markup -1)
    (gir:invoke (layout 'set_ellipsize) (gir:nget ns-pango "EllipsizeMode" ellipsize))
    (gir:invoke (layout 'set_alignment) (gir:nget ns-pango "Alignment" align))
    (multiple-value-bind (_ w h)
        (gir:invoke (layout 'get_pixel_size))
      (declare (ignore _))
      (values layout w h))))

(defun show-layout (layout &key (cr cl-cairo2:*context*))
  (let ((ns-pango-cairo (gir:require-namespace "PangoCairo")))
    (gir:invoke (ns-pango-cairo 'show_layout) (slot-value cr 'cairo:pointer) layout)))

(defun show-markup (markup &key
                             (font-size calm::*calm-default-font-size*)
                             (width calm::*calm-window-width*)
                             (height calm::*calm-window-height*)
                             (align :left)
                             (ellipsize :end)
                             (cr cl-cairo2:*context*))

  (multiple-value-bind (layout w h)
      (create-markup-layout markup
                            :font-size font-size
                            :width width
                            :height height
                            :align align
                            :ellipsize ellipsize
                            :cr cr)
    (declare (ignore w h))
    (show-layout layout :cr cr)))

;;
;; drawing
;;

(defun rrectangle (x y width height &key (cr cl-cairo2:*context*) (radius 8))
  (let ((degrees (/ pi 180)))
    (c:new-sub-path cr)
    (c:arc (- (+ x width) radius) (+ y radius) radius (* -90 degrees) (* 0 degrees) cr)
    (c:arc (- (+ x width) radius) (- (+ y height) radius) radius (* 0 degrees) (* 90 degrees) cr)
    (c:arc (+ x radius) (- (+ y height) radius) radius (* 90 degrees) (* 180 degrees) cr)
    (c:arc (+ x radius) (+ y radius) radius (* 180 degrees) (* 270 degrees) cr)
    (c:close-path)))

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
                (setf (uiop:getenv env-name) result)
                result))))))
