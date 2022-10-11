(in-package :calm)

(defun calm-quit ()
  ;; on Linux & Windows, uiop:quit will hang a while
  ;; don't know why, fix it recklessly.
  #-darwin
  (sb-ext:quit :recklessly-p t)
  #+darwin
  (uiop:quit))

(defun draw ()
  "default drawing function, user should override this"
  (c:set-source-rgb (/ 12 255) (/ 55 255) (/ 132 255))
  (c:paint)
  (c:set-source-rgb 1 1 1)
  (c:move-to 30 100)
  (c:set-font-size 84)
  (c:show-text "DON'T PANIC"))

(defun think ()
  "default thinking function, user should override this.
   You may not be drawing something, but you are always thinking."
  nil)

(defun calm-init ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (calm-window :title *calm-title* :x *calm-x* :y *calm-y* :w *calm-width* :h *calm-height* :flags *calm-flags*)
      (sdl2:with-renderer (calm-renderer calm-window)
        (sdl2:with-event-loop (:method :poll)
          (:quit () (calm-quit))
          (:mousewheel (:x x :y y :direction direction) (on-mousewheel x y direction))
          (:textinput (:text text) (on-textinput text))
          (:textediting (:text text :start start :length length) (u:calm-log "~%TEXT-EDITING: ~A ~A ~A~%" text start length))
          (:keydown (:keysym k :state s) (on-keydown (sdl2:scancode k)))
          (:keyup (:keysym k :state s) (on-keyup (sdl2:scancode k)))
          (:windowevent (:event e)
                        (u:calm-log "SDL2 Window EVENT: ~A ~%" e)
                        (cond
                          ((equal e sdl2-ffi:+sdl-windowevent-minimized+)
                           (setf *calm-redraw* nil)
                           (u:calm-log "Window has been minimized, redraw stopped. E: ~A~%" e))
                          ((equal e sdl2-ffi:+sdl-windowevent-hidden+)
                           (setf *calm-redraw* nil)
                           (u:calm-log "Window has been hidden , redraw stopped. E: ~A~%" e))

                          ((equal e sdl2-ffi:+sdl-windowevent-restored+)
                           (setf *calm-redraw* t)
                           (u:calm-log "Windows restored, resume redraw. E: ~A~%" e))
                          ((equal e sdl2-ffi:+sdl-windowevent-shown+)
                           (setf *calm-redraw* t)
                           (u:calm-log "Windows shown, calling redraw. E: ~A~%" e))

                          ((equal e sdl2-ffi:+sdl-windowevent-enter+)
                           (setf *calm-state-mouse-inside-window* t))
                          ((equal e sdl2-ffi:+sdl-windowevent-leave+)
                           (setf *calm-state-mouse-inside-window* nil))

                          ((equal e sdl2-ffi:+sdl-windowevent-close+)
                           (calm-quit))))
          (:mousemotion (:x x :y y)
                        (setf *calm-state-mouse-x* x
                              *calm-state-mouse-y* y
                              *calm-state-mouse-just-clicked* nil)
                        (on-mousemotion :x x :y y))
          (:mousebuttonup (:button button :x x :y y :clicks clicks)
                          (setf *calm-state-mouse-up* button
                                *calm-state-mouse-down* nil
                                *calm-state-mouse-just-clicked* t)
                          (on-mousebuttonup :button button :x x :y y :clicks clicks))
          (:mousebuttondown (:button button :x x :y y :clicks clicks)
                            (setf *calm-state-mouse-up* nil
                                  *calm-state-mouse-down* button
                                  *calm-state-mouse-just-clicked* nil)
                            (on-mousebuttondown :button button :x x :y y :clicks clicks))
          (:idle ()
                 (think)
                 (when *calm-redraw*
                   (multiple-value-bind (calm-renderer-width calm-renderer-height)
                       (sdl2:get-renderer-output-size calm-renderer)
                     (setf *calm-dpi-scale* (/ calm-renderer-width *calm-width*))
                     (sdl2:render-clear calm-renderer)
                     (let ((texture (sdl2:create-texture
                                     calm-renderer
                                     sdl2:+pixelformat-argb8888+
                                     sdl2-ffi:+sdl-textureaccess-streaming+
                                     calm-renderer-width
                                     calm-renderer-height)))
                       (unwind-protect
                            (progn
                              (sdl2:set-texture-blend-mode texture sdl2-ffi:+sdl-blendmode-blend+)
                              (let* ((pixels-and-pitch (multiple-value-list (sdl2:lock-texture texture)))
                                     (cr-surface
                                       (cl-cairo2:create-image-surface-for-data
                                        (car pixels-and-pitch)
                                        :argb32
                                        calm-renderer-width
                                        calm-renderer-height
                                        (cadr pixels-and-pitch)))
                                     (cr-context (cl-cairo2:create-context cr-surface)))
                                (unwind-protect
                                     (progn
                                       (cl-cairo2:with-context (cr-context)
                                         ;; set current context
                                         (setf cl-cairo2:*context* cr-context)
                                         ;; set current surface
                                         (setf cl-cairo2:*surface* cr-surface)

                                         (cl-cairo2:scale *calm-dpi-scale* *calm-dpi-scale*)
                                         (cl-cairo2:set-antialias :BEST)
                                         (cl-cairo2:font-options-set-antialias (cl-cairo2:get-font-options) :CAIRO_ANTIALIAS_BEST)
                                         ;; default background color
                                         (cl-cairo2:set-source-rgb 1 1 1)
                                         (cl-cairo2:paint)
                                         ;; default font size
                                         (cl-cairo2:set-font-size 80)
                                         ;; default color
                                         (cl-cairo2:set-source-rgb 0 0 0)
                                         ;; default position
                                         (cl-cairo2:move-to 200 150)
                                         (draw)
                                         ))
                                  (sdl2:unlock-texture texture)
                                  (sdl2:render-copy
                                   calm-renderer
                                   texture)
                                  (cl-cairo2:destroy cr-surface)
                                  (cl-cairo2:destroy cr-context))))
                         (sdl2:destroy-texture texture)))
                     (sdl2:render-present calm-renderer)))
                 (when *calm-delay*
                   (sdl2:delay *calm-delay*))))))))

(defun calm-config ()
  "This is needed by the DIST mode"
  ;; if the CALM_DIR env was not set, set it to the folder contains the core
  ;; this will happen in DIST mode on Windows
  (unless (uiop:getenv "CALM_DIR")
    (setf (uiop:getenv "CALM_DIR") (namestring (uiop:pathname-parent-directory-pathname sb-ext:*core-pathname*))))
  ;; if the APP_DIR env was not set, set it to the current dir
  ;; this will happen in DIST mode on Windows
  (unless (uiop:getenv "APP_DIR")
    (setf (uiop:getenv "APP_DIR") (namestring (uiop:getcwd))))

  ;; switch to the APP_DIR
  (uiop:chdir (uiop:getenv "APP_DIR")))

(defun calm-eval ()
  "Eval lisp code from env CALM_EVAL"
  (let ((calm-eval-str (uiop:getenv "CALM_EVAL")))
    (when calm-eval-str
      (u:calm-log "EVALing: ~%~A~%" calm-eval-str)
      (in-package #:calm)
      (eval (read-from-string calm-eval-str)))))

(defun calm-start ()
  "Start the window"
  (calm::calm-config)
  (calm-eval)
  #+linux (calm::calm-init)
  #+(or win32 darwin) (sdl2:make-this-thread-main #'calm::calm-init))

(defun calm-load-and-start ()
  "Load canvas.lisp and then start"
  (calm::calm-config)
  (load "canvas.lisp")
  (calm-eval)
  #+linux (calm::calm-init)
  #+(or win32 darwin) (sdl2:make-this-thread-main #'calm::calm-init))
