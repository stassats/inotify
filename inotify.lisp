;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:inotify)

(isys:defsyscall "inotify_init"
    :int)

(isys:defsyscall "inotify_add_watch"
    :int
  (fd :int)
  (path :string)
  (flags :uint32))

(isys:defsyscall "inotify_rm_watch"
    :int
  (fd :int)
  (watch-descriptor :uint32))

(defcstruct inotify-event
  (watch :int)
  (mask :uint32)
  (cookie :uint32)
  (name-length :uint32)
  (name :char))

(defstruct (inotify (:constructor %make-inotify))
  fd
  buffer
  watches)

(defun make-inotify ()
  (%make-inotify
   :fd (inotify-init)
   :buffer (foreign-alloc :char :count +event-size+)))

(defun close-inotify (inotify)
  (isys:close (inotify-fd inotify))
  (foreign-free (inotify-buffer inotify))
  (setf (inotify-buffer inotify) nil))

(defstruct watch
  id
  inotify
  pathname
  mask)

(defmethod print-object ((watch watch) stream)
  (print-unreadable-object (watch stream :type t)
    (format stream "pathname: ~s mask: ~a"
            (watch-pathname watch)
            (watch-mask watch))))

(defun add-watch (inotify pathname mask)
  (let* ((pathname (namestring pathname))
         (watch (make-watch :inotify inotify
                            :pathname (parse-namestring pathname)
                            :id (inotify-add-watch (inotify-fd inotify)
                                                   pathname
                                                   mask)
                            :mask mask)))
    (push watch (inotify-watches inotify))
    watch))

(defun find-watch (inotify id)
  (find id (inotify-watches inotify) :key #'watch-id))

(defstruct event
  watch
  mask
  cookie
  name)

(defun event-full-name (event)
  (if (event-name event)
      (merge-pathnames (event-name event)
                       (watch-pathname (event-watch event)))
      (watch-pathname (event-watch event))))

(defun read-event (inotify)
  (let ((buffer (inotify-buffer inotify)))
    (isys:repeat-upon-eintr
      (isys:read (inotify-fd inotify) buffer +event-size+))
    (with-foreign-slots ((watch mask cookie name-length)
                         buffer inotify-event)
      (let ((event (make-event :watch (find-watch inotify watch)
                               :mask mask
                               :cookie cookie)))
        (unless (zerop name-length)
          (setf (event-name event)
                (foreign-string-to-lisp
                 (foreign-slot-pointer buffer 'inotify-event 'name)
                 :max-chars name-length)))
        event))))

(defun make-inotify-with-watches (path-with-masks)
  (let ((inotify (make-inotify)))
    (loop for (path mask) in path-with-masks
          do (add-watch inotify path mask))
    inotify))

(defmacro with-inotify ((name paths-with-masks) &body body)
  `(let ((,name (make-inotify-with-watches ,paths-with-masks)))
     (unwind-protect (progn ,@body)
       (close-inotify ,name))))
