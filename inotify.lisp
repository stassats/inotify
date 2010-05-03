;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:inotify)

(defcfun "inotify_init" :int)

(defcfun ("close" c-close) :int (fd :int))
(defcfun ("read" c-read) :ssize-t
  (fd :int)
  (buffer :pointer)
  (count :size-t))

(defcfun "inotify_add_watch" :int
  (fd :int)
  (path :string)
  (flags :uint32))

(defcfun "inotify_rm_watch" :int
  (fd :int)
  (flags :uint32))

(defcstruct (inotify-event :size 16) 
  (watch-descritor :int)
  (mask :uint32)
  (cookie :uint32)
  (name-length :uint32)
  (name :char))
