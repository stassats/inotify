;;; -*- Mode: Lisp -*-

(defpackage #:inotify
  (:use #:cl #:cffi)
  (:export
   #:with-inotify
   
   #:make-inotify
   #:close-inotify

   #:add-watch
   #:watch
   #:watch
   #:watch-pathname
   #:watch-inotify
   #:watch-mask

   #:inotify
   #:inotify-watches

   #:read-event
   #:event
   #:event-name
   #:event-mask
   #:event-watch
   #:event-cookie))
