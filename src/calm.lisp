(in-package :calm)

#-jscl
(defun calm-quit ()
  (sdl2-mixer:close-audio)
  ;; on Linux & Windows, uiop:quit will hang a while
  ;; don't know why, fix it recklessly.
  (if (uiop:featurep :darwin)
      (uiop:quit)
      (uiop:quit 0 nil)))

(defun internal-draw ()
  "default drawing function, user should defun `draw'"
  (cond
    ((fboundp 'draw) (funcall 'draw) (setf *calm-redraw* nil))
    ((fboundp 'draw-once) (funcall 'draw-once) (setf *calm-redraw* nil))
    ((fboundp 'draw-forever) (funcall 'draw-forever))
    (t
     (c:set-source-rgb (/ 12 255) (/ 55 255) (/ 132 255))
     (c:paint)
     (c:set-source-rgb 1 1 1)
     (c:move-to 30 100)
     (c:set-font-size 84)
     (c:show-text "DON'T PANIC"))))

(defun internal-think ()
  "default thinking function, user should defun `think'
   You may not be drawing something, but you are always thinking."
  (when (fboundp 'think)
    (funcall 'think)))

#-jscl
(defun calm-init ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (calm-window :title *calm-window-title* :x *calm-window-x* :y *calm-window-y* :w *calm-window-width* :h *calm-window-height* :flags *calm-window-flags*)
      ;; default window icon
      (if *calm-window-icon*
          (setf *calm-window-icon*
                (or
                 (uiop:absolute-pathname-p *calm-window-icon*)
                 (uiop:merge-pathnames* *calm-window-icon* (uiop:getenv "CALM_APP_DIR"))))
          (setf *calm-window-icon* (str:concat (uiop:getenv "CALM_HOME") "build/app.png")))
      (when (probe-file *calm-window-icon*)
        (sdl2-ffi.functions:sdl-set-window-icon ;; on Wayland, this doesn't work, who should I blame?
         calm-window
         (sdl2-image:load-image *calm-window-icon*)))

      (sdl2:with-renderer (calm-renderer calm-window :flags *calm-renderer-flags*)
        (multiple-value-bind (calm-renderer-width calm-renderer-height)
            (sdl2:get-renderer-output-size calm-renderer)
          (let ((cairo-x-multiplier (/ calm-renderer-width *calm-window-width*))
                (cairo-y-multiplier (/ calm-renderer-height *calm-window-height*))
                (texture (sdl2:create-texture
                          calm-renderer
                          sdl2:+pixelformat-argb8888+
                          sdl2-ffi:+sdl-textureaccess-streaming+
                          calm-renderer-width
                          calm-renderer-height)))
            (sdl2:with-event-loop (:method :poll)
              (:quit () (sdl2:destroy-texture texture) (calm-quit))
              (:mousewheel (:x x :y y :direction direction) (internal-on-mousewheel x y direction))
              (:textinput (:text text) (internal-on-textinput text))
              (:textediting (:text text :start start :length length) (u:calm-log "~%TEXT-EDITING: ~A ~A ~A~%" text start length))
              (:keydown (:keysym k :state s) (internal-on-keydown (sdl2:scancode k)))
              (:keyup (:keysym k :state s) (internal-on-keyup (sdl2:scancode k)))
              (:windowevent (:event e)
                            (u:calm-log "SDL2 Window EVENT: ~A ~%" e)
                            (cond
                              ((equal e sdl2-ffi:+sdl-windowevent-minimized+)
                               (setf *calm-redraw* nil))
                              ((equal e sdl2-ffi:+sdl-windowevent-hidden+)
                               (setf *calm-redraw* nil))

                              ((equal e sdl2-ffi:+sdl-windowevent-restored+)
                               (setf *calm-redraw* t))

                              ((equal e sdl2-ffi:+sdl-windowevent-shown+)
                               (setf *calm-redraw* t))

                              ((equal e sdl2-ffi:+sdl-windowevent-enter+)
                               (internal-on-windowenter))
                              ((equal e sdl2-ffi:+sdl-windowevent-leave+)
                               (internal-on-windowleave))

                              ((equal e sdl2-ffi:+sdl-windowevent-resized+)
                               (multiple-value-bind (new-window-width new-window-height) (sdl2:get-window-size calm-window)
                                 (internal-on-windowresized new-window-width new-window-height)
                                 (multiple-value-bind (new-renderer-width new-renderer-height)
                                     (sdl2:get-renderer-output-size calm-renderer)
                                   (let ((new-x-multiplier (/ new-renderer-width new-window-width))
                                         (new-y-multiplier (/ new-renderer-height new-window-height))
                                         (new-texture (sdl2:create-texture
                                                       calm-renderer
                                                       sdl2:+pixelformat-argb8888+
                                                       sdl2-ffi:+sdl-textureaccess-streaming+
                                                       new-renderer-width
                                                       new-renderer-height)))
                                     (sdl2:destroy-texture texture)
                                     (setf calm-renderer-width new-renderer-width
                                           calm-renderer-height new-renderer-height
                                           cairo-x-multiplier new-x-multiplier
                                           cairo-y-multiplier new-y-multiplier
                                           texture new-texture)))))
                              ((equal e sdl2-ffi:+sdl-windowevent-close+) (sdl2:destroy-texture texture) (calm-quit))))
              (:mousemotion (:x x :y y)
                            (internal-on-mousemotion :x x :y y))
              (:mousebuttonup (:button button :x x :y y :clicks clicks)
                              (internal-on-mousebuttonup :button button :x x :y y :clicks clicks))
              (:mousebuttondown (:button button :x x :y y :clicks clicks)
                                (internal-on-mousebuttondown :button button :x x :y y :clicks clicks))
              (:idle ()
                     (internal-think)
                     (when *calm-redraw*
                       (unwind-protect
                            (progn
                              (sdl2:set-texture-blend-mode texture sdl2-ffi:+sdl-blendmode-blend+)
                              (let* ((pixels-and-pitch (multiple-value-list (sdl2:lock-texture texture)))
                                     (cr-surface
                                       (c:create-image-surface-for-data
                                        (car pixels-and-pitch)
                                        :argb32
                                        calm-renderer-width
                                        calm-renderer-height
                                        (cadr pixels-and-pitch)))
                                     (cr-context (c:create-context cr-surface)))
                                (unwind-protect
                                     (progn
                                       (c:with-context (cr-context)
                                         ;; set current context
                                         (setf c:*context* cr-context)
                                         ;; set current surface
                                         (setf c:*surface* cr-surface)
                                         (c:scale cairo-x-multiplier cairo-y-multiplier)
                                         (c:set-antialias :BEST)
                                         (c:font-options-set-antialias (c:get-font-options) :CAIRO_ANTIALIAS_BEST)
                                         ;; default background
                                         (c:set-source-rgb 1 1 1)
                                         (c:paint)
                                         ;; default font size
                                         (c:set-font-size *calm-default-font-size*)
                                         ;; default font face
                                         (c:select-font-face *calm-default-font-family* :normal :normal)
                                         ;; default color
                                         (c:set-source-rgb 0 0 0)
                                         ;; default position
                                         (c:move-to 200 150)
                                         (internal-draw)
                                         ))
                                  (sdl2:unlock-texture texture)
                                  (sdl2:render-copy calm-renderer texture)
                                  (sdl2:render-present calm-renderer)
                                  (c:destroy cr-surface)
                                  (c:destroy cr-context))))))
                     (when *calm-delay* (sdl2:delay *calm-delay*))))))))))

#-jscl
(defun calm-start ()
  "Start the window"
  (calm::calm-config)
  (cond
    ((uiop:featurep :linux) (calm::calm-init))
    ((uiop:featurep '(:or :darwin :win32)) (sdl2:make-this-thread-main #'calm::calm-init))
    (t "unsupported system?")))

#-jscl
(defun calm-load-and-start ()
  "Load canvas.lisp and then start"
  (calm::calm-config)
  (let ((canvas-file (merge-pathnames "canvas.lisp" (uiop:getcwd))))
    (if (probe-file canvas-file)
        (load canvas-file)
        (u:calm-log "canvas.lisp NOT FOUND: ~A~%" canvas-file)))
  (cond
    ((uiop:featurep :linux) (calm::calm-init))
    ((uiop:featurep '(:or :darwin :win32)) (sdl2:make-this-thread-main #'calm::calm-init))
    (t "unsupported system?")))
