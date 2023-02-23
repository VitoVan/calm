(in-package #:calm-utils)

(defmacro calm-log (control-string &rest rest)
  `(format t ,control-string ,@rest))

(defun set-cursor (type)
  (cond
    ((equal type :hand) (sdl2-ffi.functions:sdl-set-cursor (sdl2-ffi.functions:sdl-create-system-cursor sdl2-ffi:+sdl-system-cursor-hand+)))
    ((equal type :arrow) (sdl2-ffi.functions:sdl-set-cursor (sdl2-ffi.functions:sdl-create-system-cursor sdl2-ffi:+sdl-system-cursor-arrow+)))))

(sdl2-mixer:init)

(defun play-music (pathname &optional (loops 0))

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
   ;; chunksize: audio buffer size in sample FRAMES (total samples divided by channel count).
   (* calm::*calm-music-channels* calm::*calm-music-frequency*))

  (let ((music
          (sdl2-mixer:load-music
           (or
            (uiop:absolute-pathname-p pathname)
            (uiop:merge-pathnames* pathname (uiop:getenv "APP_DIR"))))))
    (sdl2-mixer:play-music music loops)))

(defun halt-music ()
  (sdl2-mixer:halt-music))
