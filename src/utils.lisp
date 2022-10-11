(in-package #:calm-utils)

(defmacro calm-log (control-string &rest rest)
  `(format t ,control-string ,@rest))

(defun set-cursor (type)
  (cond
    ((equal type :hand) (sdl2-ffi.functions:sdl-set-cursor (sdl2-ffi.functions:sdl-create-system-cursor sdl2-ffi:+sdl-system-cursor-hand+)))
    ((equal type :arrow) (sdl2-ffi.functions:sdl-set-cursor (sdl2-ffi.functions:sdl-create-system-cursor sdl2-ffi:+sdl-system-cursor-arrow+)))))

(sdl2-mixer:init)
(defun play-wav (pathname &optional (loops 0))
  (let ((abs-pathname (or
                       (uiop:absolute-pathname-p pathname)
                       (uiop:merge-pathnames* pathname (uiop:getenv "APP_DIR")))))
    (if (string= (str:downcase (pathname-type abs-pathname)) "wav")
        (progn (sdl2-mixer:open-audio sdl2-ffi:+mix-default-frequency+ sdl2-ffi:+mix-default-format+ 2 4096)
               (sdl2-mixer:play-channel -1 (sdl2-mixer:load-wav abs-pathname) loops))
        (format t "Only WAV supported, try sdl2-mixer."))))

(defun halt-wav ()
  (sdl2-mixer:halt-channel -1))

(defun play-music (pathname &optional (loops 0))
  (sdl2-mixer:open-audio sdl2-ffi:+mix-default-frequency+ sdl2-ffi:+mix-default-format+ 2 4096)
  (let ((music
          (sdl2-mixer:load-music
           (or
            (uiop:absolute-pathname-p pathname)
            (uiop:merge-pathnames* pathname (uiop:getenv "APP_DIR"))))))
    (sdl2-mixer:play-music music loops)))

(defun halt-music ()
  (sdl2-mixer:halt-music))

(defun audio-is-playing ()
  (not (= 0 (sdl2-mixer:playing -1))))
