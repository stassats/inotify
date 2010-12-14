;;; -*- Mode: Lisp -*-

(defpackage #:inotify
  (:use #:cl #:cffi)
  (:export
   #:with-inotify
   
   #:make-inotify
   #:close-inotify

   #:add-watch
   #:remove-watch
   #:watch
   #:watch
   #:watch-pathname
   #:watch-inotify
   #:watch-mask

   #:inotify
   #:inotify-watches

   #:read-events
   #:event
   #:event-name
   #:event-mask
   #:event-watch
   #:event-cookie
   #:event-full-name

   #:in-access
   #:in-modify
   #:in-attrib
   #:in-close-write
   #:in-close-nowrite
   #:in-close
   #:in-open
   #:in-moved-from
   #:in-moved-to
   #:in-move
   #:in-create
   #:in-delete
   #:in-delete-self
   #:in-move-self
   #:in-unmount
   #:in-q-overflow
   #:in-ignored
   #:in-close
   #:in-move
   #:in-onlydir
   #:in-dont-follow
   #:in-mask-add
   #:in-isdir
   #:in-oneshot
   #:in-all-events
   #:find-watch))
