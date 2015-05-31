;;; -*- Mode: Lisp -*-

(eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem #:inotify
  :depends-on (cffi iolib)
  :author "Stas Boukarev"
  :license "Public Domain"
  :description "Interface to linux inotify(7)"
  :serial t
  :components ((:file "packages")
               (cffi-grovel:grovel-file "grovel")
               (:file "inotify")))
