;;
;; Fan © 2023 by Vito Van
;; is licensed under CC BY-NC-SA 4.0.
;; To view a copy of this license, visit
;;        http://creativecommons.org/licenses/by-nc-sa/4.0/
;; To obtain a commercial license, visit:
;;        https://www.buymeacoffee.com/vitovan/e/119069
;;

(in-package #:calm)

(defparameter *fan-version* "0.0.1")

(setf *calm-window-width* 600)
(setf *calm-window-height* 500)

#-jscl
(unless (str:starts-with? "dist" (uiop:getenv "CALM_CMD")) (swank:create-server))

;; reduce the delay (default is 42), make the fan more smooth
#-jscl
(setf *calm-delay* 10)
#+jscl
(setf *calm-fps* 0)

(setf *calm-window-title* "Fan")

(defparameter *fan-speed-level* 0)

(defparameter *fan-speed-level-list* '(0 12 16 20))

(defparameter *fan-blade-angle-degree-default-list*
  '(0 90 180 270))

(defparameter *fan-blade-angle-degree-list*
  *fan-blade-angle-degree-default-list*)

(defun on-fan-switch-click ()
  (setf *calm-redraw* t)
  (let ((i (position *fan-speed-level* *fan-speed-level-list*)))
    (when (>= i 3) (setf i -1))
    (setf *fan-speed-level* (nth (1+ i) *fan-speed-level-list*))))

(defun draw-blade (&optional (degree 0))
  (c:save)
  (c:translate 300 170)
  (c:rotate (* degree (/ pi 180)))
  (c:move-to 0 -15)
  (c:line-to 0 -100)
  (c:curve-to 0 -110 100 -65 15 0)
  (c:stroke-preserve)

  (c:set-source-rgba (/ 12 255) (/ 55 255) (/ 132 255) 0.1)
  (c:fill-path)
  (c:restore))

(defun draw-switch ()
  (c:new-path)
  (c:set-source-rgb (/ 12 255) (/ 55 255) (/ 132 255))
  (c:set-line-width 3)
  (c:arc 550 50 15 0 (* 2 pi))
  (if (c:in-fill *calm-state-mouse-x* *calm-state-mouse-y*)
      (progn
        (c:stroke-preserve)
        (c:fill-path)
        #-jscl
        (u:set-cursor :hand)
        (when *calm-state-mouse-just-clicked*
          (setf *calm-state-mouse-just-clicked* nil)
          (on-fan-switch-click)))
      (progn (c:stroke)
             #-jscl
             (u:set-cursor :arrow)
             )))

(defun rotate-blades ()
  (when (> (apply #'max *fan-blade-angle-degree-list*) 360)
    (setf *fan-blade-angle-degree-list*
          *fan-blade-angle-degree-default-list*))
  (if (= *fan-speed-level* 0)
      (progn
        (unless (equal *fan-blade-angle-degree-list* *fan-blade-angle-degree-default-list*)
          (setf *fan-blade-angle-degree-list* (mapcar #'(lambda (x)  (+ x 3)) *fan-blade-angle-degree-list*))))
      (progn
        (setf *fan-blade-angle-degree-list* (mapcar #'(lambda (x)  (+ x *fan-speed-level*)) *fan-blade-angle-degree-list*)))))

(defun draw-forever ()
  (setf *calm-redraw*
        (or
         (> *fan-speed-level* 0)
         (not (= (car *fan-blade-angle-degree-list*) 0))))

  (c:set-source-rgb (/ 12 255) (/ 55 255) (/ 132 255))
  (c:new-path)
  (c:set-line-width 6)
  (c:set-line-cap :round)

  (c:arc 300 170 120 (* 1.25 pi) (* 3.2 pi))
  (c:stroke)

  ;; (format t "X,Y: ~A,~A~%" *calm-state-mouse-x* *calm-state-mouse-y*)

  ;; neck

  (c:move-to 270 290)
  (c:line-to 255 380)

  (c:move-to 330 290)
  (c:line-to 345 380)

  ;; base

  (c:move-to 220 420)
  (c:line-to 380 420)

  (c:move-to 300 380)
  (c:curve-to 370 375 400 420 380 420)

  (c:move-to 280 380)
  (c:curve-to 220 380 200 420 220 420)
  (c:stroke)

  ;; blade
  (mapcar 'draw-blade *fan-blade-angle-degree-list*)

  ;; turbo
  (c:arc 300 170 10 0 (* 2 pi))
  (c:stroke)

  ;; switch
  (draw-switch)

  ;; rotate
  (rotate-blades))
