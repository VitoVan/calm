(in-package #:calm)

;;
;; CALM version check
;; version check won't work on JSCL, since the lack of ASDF
;;
#-jscl
(let ((required-version "1.3.0")
      (calm-version (slot-value (asdf:find-system 'calm) 'asdf:version)))
  (when (uiop:version< calm-version required-version)
    (format t
            "Sorry, this is built on CALM ~A, older version (current: ~A) of CALM won't work.~%"
            required-version calm-version)
    (uiop:quit)))


;; DEBUGGING, please uncomment the correspoding line
;;
;; for Emacs - SLIME
;;        https://slime.common-lisp.dev/
;;
;; (unless (str:starts-with? "dist" (uiop:getenv "CALM_CMD")) (swank:create-server :dont-close t))
;;
;; for LEM - micros
;;        https://github.com/lem-project/micros
;;
;; (unless (str:starts-with? "dist" (uiop:getenv "CALM_CMD")) (micros:create-server :dont-close t))
;;
;; for Alive - Visual Studio Code
;;        https://github.com/nobody-famous/alive
;; please config `alive.lsp.startCommand':
;;
;;        {
;;            "alive.lsp.startCommand": [
;;                "calm",
;;                "alive"
;;            ]
;;        }



;;
;; by default, the screensaver is disabled,
;; if you want to enable screensaver,
;; please uncomment the following line
;;
;; (setf (uiop:getenv "SDL_VIDEO_ALLOW_SCREENSAVER") "1")

;;
;; setting window properties, for others, please check
;;      https://github.com/VitoVan/calm/blob/main/src/config.lisp
;;
(setf *calm-window-width* 600)
(setf *calm-window-height* 150)
(setf *calm-window-title* "CALM")

(defun draw ()
  (c:set-source-rgb (/ 12 255) (/ 55 255) (/ 132 255))
  (c:paint)
  (c:set-source-rgb 1 1 1)
  (c:move-to 30 100)
  (c:set-font-size 84)
  (c:show-text "DON'T PANIC"))
