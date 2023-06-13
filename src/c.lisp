(in-package #:c)

;;
;; cairo state
;;
(defmacro with-state (&body body)
  `(progn
     (c:save)
     ,@body
     (c:restore)))


;;
;; drawing
;;

(defun rrectangle (x y width height &key (radius 8))
  "rounded rectangle"
  (let ((degrees (/ pi 180)))
    (c:new-sub-path)
    (c:arc (- (+ x width) radius) (+ y radius) radius (* -90 degrees) (* 0 degrees))
    (c:arc (- (+ x width) radius) (- (+ y height) radius) radius (* 0 degrees) (* 90 degrees))
    (c:arc (+ x radius) (- (+ y height) radius) radius (* 90 degrees) (* 180 degrees))
    (c:arc (+ x radius) (+ y radius) radius (* 180 degrees) (* 270 degrees))
    (c:close-path)))

(defun show-png (pathname x y width height)
  (with-state
    (let* ((png-pathname
             #+jscl
             (concatenate 'string "/usr/share/" pathname) ;; /usr/share/assets/ will be embedded
             #-jscl
             (or (uiop:absolute-pathname-p pathname)
                 (uiop:merge-pathnames* pathname (uiop:getenv "CALM_APP_DIR"))))
           (surface (c:image-surface-create-from-png png-pathname))
           (img-width (c:image-surface-get-width surface))
           (img-height (c:image-surface-get-height surface))
           (x-multiplier (/ width img-width))
           (y-multiplier (/ height img-height)))
      (format t "showing png: ~A surface: ~A w:~A iw:~A~%" png-pathname surface width img-width)
      (c:move-to x y)
      (c:rectangle x y width height)
      (when (and (<= x-multiplier 1)  (<= y-multiplier 1))
        ;; (format t "~A~%" z)
        (c:scale x-multiplier y-multiplier)
        (c:translate (* (/ (- 1 x-multiplier) x-multiplier) x) (* (/ (- 1 y-multiplier) y-multiplier) y)))
      (c:set-source-surface surface x y)
      (c:fill-path))))

;;
;; text utilities
;;

#-jscl
(defun escape-char (char s)
  "char escaping for pango-markup"
  (case char
    (#\< (write-string "&lt;" s))
    (#\> (write-string "&gt;" s))
    (#\& (write-string "&amp;" s))
    (#\' (write-string "&#39;" s))
    (T (write-char char s))))

#-jscl
(defun escape-string (string)
  "string escaping for Pango Markup"
  (with-output-to-string (o)
    (loop for char across string
          do (escape-char char o))))

#-jscl
(defun markup->layout (markup &key
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

#-jscl
(defun show-layout (layout &key (cr cl-cairo2:*context*))
  "show Pango Layout"
  (let ((ns-pango-cairo (gir:require-namespace "PangoCairo")))
    (gir:invoke (ns-pango-cairo 'show_layout) (slot-value cr 'cairo:pointer) layout)))

#-jscl
(defun show-markup (markup &key
                             (font-size calm::*calm-default-font-size*)
                             (width calm::*calm-window-width*)
                             (height calm::*calm-window-height*)
                             (align :left)
                             (ellipsize :end)
                             (cr cl-cairo2:*context*))

  "show Pango Markup"
  (multiple-value-bind (layout w h)
      (markup->layout markup
                            :font-size font-size
                            :width width
                            :height height
                            :align align
                            :ellipsize ellipsize
                            :cr cr)
    (declare (ignore w h))
    (show-layout layout :cr cr)))

(defun open-audio-if-not-yet ()
  ;; (format t "open-audio-if-not-yet, already opened?: ~A~%" calm::*calm-state-audio-open*)
  (unless calm::*calm-state-audio-open*
    ;;
    ;; if we put the following code outside of this function,
    ;; it will open-audio right after the library is loaded,
    ;; which will cause problem for save-lisp-and-die
    ;;
    ;; init is optional
    ;; https://wiki.libsdl.org/SDL2_mixer/Mix_Init
    ;; (sdl2-mixer:init)
    #+jscl
    (#j:_Mix_OpenAudio
     calm::*calm-audio-frequency*
     calm::*calm-audio-format*
     calm::*calm-audio-channels*
     calm::*calm-audio-chunksize*)
    #-jscl
    (sdl2-mixer:open-audio
     calm::*calm-audio-frequency*
     calm::*calm-audio-format*
     ;; channels: number of channels (1 is mono, 2 is stereo, etc).
     calm::*calm-audio-channels*
     calm::*calm-audio-chunksize*)
    #+jscl
    (#j:_Mix_AllocateChannels calm::*calm-audio-numchans*)
    #-jscl
    (sdl2-mixer:allocate-channels calm::*calm-audio-numchans*)
    (setf calm::*calm-state-audio-open* t)))

(defun play-music (pathname &key (loops 0))
  ;; (format t "playing music: ~A~%" pathname)
  (open-audio-if-not-yet)
  (let* ((music-pathname
           #+jscl
           (concatenate 'string "/usr/share/" pathname) ;; /usr/share/assets/ will be embedded
           #-jscl
           (or (uiop:absolute-pathname-p pathname)
               (uiop:merge-pathnames* pathname (uiop:getenv "CALM_APP_DIR"))))
         (music-object-cache
           (cdr (assoc music-pathname calm::*calm-state-loaded-audio*
                       #+jscl :test #+jscl #'string=)))
         (music-object (or music-object-cache
                           #+jscl
                           (let ((music-pathname-ptr (#j:allocateUTF8 music-pathname)))
                             (prog1 (#j:_Mix_LoadMUS music-pathname-ptr)
                               (#j:_free music-pathname-ptr)))
                           #-jscl
                           (sdl2-mixer:load-music music-pathname))))
    (unless music-object-cache
      (push (cons music-pathname music-object) calm::*calm-state-loaded-audio*))
    #+jscl
    (#j:_Mix_PlayMusic music-object loops)
    #-jscl
    (sdl2-mixer:play-music music-object loops)))

(defun play-wav (pathname &key (loops 0) (channel -1))
  ;; (format t "playing wav: ~A~%" pathname)
  (open-audio-if-not-yet)
  (let* ((wav-pathname
          #+jscl
           (concatenate 'string "/usr/share/" pathname) ;; /usr/share/assets/ will be embedded
           #-jscl
           (or (uiop:absolute-pathname-p pathname)
               (uiop:merge-pathnames* pathname (uiop:getenv "CALM_APP_DIR"))))
         (wav-object-cache
          (cdr (assoc wav-pathname calm::*calm-state-loaded-audio*
                      #+jscl :test #+jscl #'string=)))
         (wav-object (or wav-object-cache
                         #+jscl
                         (let* ((wav-pathname-allocated-ptr (#j:allocateUTF8 wav-pathname))
                                (rb-allocated-ptr (#j:allocateUTF8 "rb"))
                                (rwops (#j:_SDL_RWFromFile wav-pathname-allocated-ptr rb-allocated-ptr))
                                (spec (#j:_Mix_LoadWAV_RW rwops 1 nil nil nil)))
                           (#j:_free wav-pathname-allocated-ptr)
                           (#j:_free rb-allocated-ptr)
                           spec)
                         #-jscl
                         (sdl2-mixer:load-wav wav-pathname))))
    (unless wav-object-cache
      (push (cons wav-pathname wav-object) calm::*calm-state-loaded-audio*))

    ;; only play when there is/are available channel(s)
    (when (< (playing) calm::*calm-audio-numchans*)
      #+jscl
      (#j:_Mix_PlayChannelTimed channel wav-object loops -1)
      #-jscl
      (sdl2-mixer:play-channel channel wav-object loops))))

#+jscl
(defun play-audio (audio-url &key (loop-audio-p nil) (volume 1))
  (format t "playing audio: ~A~%" audio-url)
  (let* ((audio-object-cache
           (cdr (assoc audio-url calm::*calm-state-loaded-audio* :test #'string=)))
         (audio-object
           (or audio-object-cache
               ;; https://github.com/jscl-project/jscl/wiki/JSCL-and-manipulations-with-JS-objects
               (#j:window:eval (concatenate 'string "new Audio('" audio-url "')"))
               )))
    (unless audio-object-cache
      (push (cons audio-url audio-object) calm::*calm-state-loaded-audio*))
    (when loop-audio-p
      (setf (jscl::oget audio-object "loop") t))
    (setf (jscl::oget audio-object "volume") volume)
    ((jscl::oget audio-object "play"))))

(defun playing ()
  #+jscl
  (#j:_Mix_Playing -1)
  #-jscl
  (sdl2-mixer:playing -1))

(defun volume-music (music-volume)
  #+jscl
  (#j:_Mix_VolumeMusic music-volume)
  #-jscl
  (sdl2-mixer:volume-music music-volume))

(defun volume (channel volume)
  #+jscl
  (#j:_Mix_Volume channel volume)
  #-jscl
  (sdl2-mixer:volume channel volume))

(defun halt-music ()
  #+jscl
  (#j:_Mix_HaltMusic)
  #-jscl
  (sdl2-mixer:halt-music))

(defun halt-wav (&optional (channel -1))
  #+jscl
  (#j:_Mix_HaltChannel channel)
  #-jscl
  (sdl2-mixer:halt-channel channel))

#+jscl
(defun halt-audio (&optional (url nil))
  (loop
    for object in calm::*calm-state-loaded-audio*
    when (or (null url) (string= (car object) url))
    do
       (format t "halting: ~A~%" object)
       ((jscl::oget (cdr object) "pause"))))

(defun get-ticks ()
  #+jscl
  (#j:_SDL_GetTicks)
  #-jscl
  (sdl2:get-ticks))

#+jscl
(eval-when (:compile-toplevel)
  (ql:quickload "sdl2"))

(defmacro keq (key &rest scancodes)
  "Key Event in JSCL + WebAssembly returns scancode value, since it has to be primitive values.
But on desktop, we use SCANCODE directly, so here is a macro to unify this behaviour.
"
  (let ((scancode-value-list (mapcar #'sdl2:scancode-key-to-value scancodes)))
    `(member ,key (if (keywordp ,key) ',scancodes ',scancode-value-list))))

(defun select-font-family (family slant weight)
  #+jscl
  (c:select-font-face family slant weight)
  #-jscl
  (cl-cairo2::select-font-face-fc family slant weight))
