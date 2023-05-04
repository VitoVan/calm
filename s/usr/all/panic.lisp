(in-package #:calm)

;;
;; CALM version check
;;
#-jscl
(let ((required-version "0.0.42"))
  (unless (string>= *calm-version* required-version)
    (format t "Sorry, CALM ~A is needed, older version (current: ~A) of CALM won't work.~%"
            required-version *calm-version*)
    (uiop:quit 42)))


;;
;; the swank server is for debugging, for usage please check
;; Emacs:
;;        https://slime.common-lisp.dev/
;; Visual Studio Code
;;        https://lispcookbook.github.io/cl-cookbook/vscode-alive.html
;;
;; uncomment the following line to enable SWANK Server
;; (unless (str:starts-with? "dist" (uiop:getenv "CALM_CMD")) (swank:create-server))

;;
;; by default, the screensaver is disabled,
;; if you want to enable screensaver,
;; please uncomment the following line
;;
;; (setf (uiop:getenv "SDL_VIDEO_ALLOW_SCREENSAVER") "1")

;;
;; setting window properties, for more of this, please check
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
