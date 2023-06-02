;; some of the code borrowed from: https://github.com/Shinmera/font-discovery
(in-package #:fontconfig)

(cffi:define-foreign-library :fontconfig
  (:darwin (:or "libfontconfig.1.dylib" "libfontconfig.dylib"))
  (:unix (:or "libfontconfig.so.1" "libfontconfig.so"))
  (:windows (:or "libfontconfig-1.dll" "libfontconfig.dll"))
  (t (:default "libfontconfig")))

(cffi:load-foreign-library :fontconfig)

(cffi:defcenum weight
  (:thin 0)
  (:extra-light 40)
  (:ultra-light 40)
  (:light 50)
  (:demi-light 55)
  (:semi-light 55)
  (:book 75)
  (:regular 80)
  (:normal 80)
  (:medium 100)
  (:demi-bold 180)
  (:semi-bold 180)
  (:bold 200)
  (:extra-bold 205)
  (:black 210)
  (:heavy 210))

(cffi:defcenum slant
  (:normal 0)
  (:roman 0)
  (:italic 100)
  (:oblique 110))

(defun maybe-enum (type thing)
  (etypecase thing
    (keyword (cffi:foreign-enum-value type thing))
    (integer thing)))

(cffi:defcfun (fc-init "FcInit") :pointer)

(cffi:defcfun (fc-init-reinitialize "FcInitReinitialize") :pointer)

(cffi:defcfun (fc-create-pattern "FcPatternCreate") :pointer)

(cffi:defcfun (fc-pattern-add-string "FcPatternAddString") :bool
  (pattern :pointer)
  (object :string)
  (string :string))

(cffi:defcfun (fc-pattern-add-integer "FcPatternAddInteger") :bool
  (pattern :pointer)
  (object :string)
  (i :int))

(cffi:defcfun (destroy-pattern "FcPatternDestroy") :void
  (pattern :pointer))
