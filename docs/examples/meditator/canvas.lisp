;;
;; Meditator © 2023 by Vito Van
;; is licensed under CC BY-NC-SA 4.0.
;; To view a copy of this license, visit
;;        http://creativecommons.org/licenses/by-nc-sa/4.0/
;; To obtain a commercial license, visit:
;;        https://www.buymeacoffee.com/vitovan/e/119067
;;

(in-package #:calm)

(defparameter *meditator-version* "0.0.1")

(setf *calm-window-width* 600)
(setf *calm-window-height* 500)
(setf *calm-window-title* "Meditator")

(defun draw-button ()
  (c:new-path)
  (c:set-source-rgb (/ 210 255) (/ 0 255) (/ 22 255))
  (c:set-line-width 2)
  (c:arc 550 50 15 0 (* 2 pi))
  (if (c:in-fill *calm-state-mouse-x* *calm-state-mouse-y*)
      (progn
        (c:fill-path)
             #-jscl
             (u:set-cursor :hand)
             (when *calm-state-mouse-just-clicked*
               (setf *calm-state-mouse-just-clicked* nil)
               (c:play-music "assets/bowl.wav"))
             (c:arc 370 105 5 (* 1.7 pi) (* 0.3 pi))
             (c:stroke)
             (c:arc 375 105 10 (* 1.7 pi) (* 0.3 pi))
             (c:stroke)
             (c:arc 380 105 15 (* 1.7 pi) (* 0.3 pi))
             (c:stroke))
      (progn (c:stroke)
             #-jscl
             (u:set-cursor :arrow)
             )))

(defun draw ()
  (c:set-source-rgb (/ 12 255) (/ 55 255) (/ 132 255))
  (c:new-path)
  (c:set-line-width 5)
  (c:set-line-cap :round)

  (c:arc 300 110 60 0 (* 2 pi))
  (c:stroke)

  (c:move-to 250 150)
  (c:curve-to 150 200 120 350 200 400)
  (c:stroke)

  (c:move-to 350 150)
  (c:curve-to 450 200 480 350 400 400)
  (c:stroke)

  (c:move-to 380 170)
  (c:curve-to 340 260 240 280 180 280)
  (c:stroke)

  (c:move-to 210 180)
  (c:curve-to 230 220 280 230 330 230)
  (c:stroke)

  (c:move-to 170 380)
  (c:curve-to 100 460 250 420 300 380)
  (c:stroke)

  (c:move-to 430 380)
  (c:curve-to 470 460 350 420 280 360)
  (c:stroke)

  (draw-button))
