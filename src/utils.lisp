(in-package #:calm-utils)

(defun set-cursor (type)
  (cond
    ((equal type :hand) (sdl2-ffi.functions:sdl-set-cursor (sdl2-ffi.functions:sdl-create-system-cursor sdl2-ffi:+sdl-system-cursor-hand+)))
    ((equal type :arrow) (sdl2-ffi.functions:sdl-set-cursor (sdl2-ffi.functions:sdl-create-system-cursor sdl2-ffi:+sdl-system-cursor-arrow+)))))

(sdl2-mixer:init)
(defun mix-play (pathname &optional (loops 0))
  (if (string= (str:downcase (pathname-type pathname)) "wav")
      (progn (sdl2-mixer:open-audio sdl2-ffi:+mix-default-frequency+ sdl2-ffi:+mix-default-format+ 2 4096)
             (sdl2-mixer:play-channel -1 (sdl2-mixer:load-wav pathname) loops))
      (format t "Only WAV supported, try sdl2-mixer.")))

(defun mix-halt ()
  (sdl2-mixer:halt-channel -1))

(defun mix-is-playing ()
  (not (= 0 (sdl2-mixer:playing -1))))
