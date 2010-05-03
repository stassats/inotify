;;; -*- Mode: Lisp -*-

(eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem #:inotify
  :depends-on (cffi)
  :serial t
  :components ((:file "packages")
               (cffi-grovel:grovel-file "grovel")
               (:file "inotify")))
