;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:inotify)

(isys:defsyscall (inotify-init "inotify_init")
    :int)

(isys:defsyscall (inotify-add-watch "inotify_add_watch")
    :int
  (fd :int)
  (path :string)
  (flags :uint32))

(isys:defsyscall (inotify-rm-watch "inotify_rm_watch")
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
  (fd nil)
  buffer-size
  buffer
  (watches nil))

(defun make-inotify (&optional (buffer-count 100))
  (let ((buffer-size (* +event-max-size+ buffer-count)))
    (%make-inotify
     :fd (inotify-init)
     :buffer-size buffer-size
     :buffer (foreign-alloc :char :count buffer-size))))

(defun close-inotify (inotify)
  (unless (null-pointer-p (inotify-buffer inotify))
    (foreign-free (inotify-buffer inotify))
    (setf (inotify-buffer inotify) (null-pointer)))
  (when (inotify-fd inotify)
    (unwind-protect (isys:close (inotify-fd inotify))
      (setf (inotify-fd inotify) nil))))

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

(defgeneric find-watch (inotify id))

(defmethod find-watch (inotify (id integer))
  (find id (inotify-watches inotify) :key #'watch-id))

(defmethod find-watch (inotify (pathname pathname))
  (find pathname (inotify-watches inotify)
	:key #'watch-pathname
	:test #'equal))

(defmethod find-watch (inotify (pathname string))
  (find-watch inotify (parse-namestring pathname)))

(defgeneric remove-watch (intofy watch))

(defmethod remove-watch (inotify id)
  (let ((watch (find-watch inotify id)))
    (if watch
        (remove-watch inotify watch)
        (error "no watch with id ~a was found in ~a"
               id inotify))))

(defmethod remove-watch (inotify (watch watch))
  (unless (eql (watch-inotify watch) inotify)
    (error "~a is not from ~a" watch inotify))
  (setf (inotify-watches inotify)
        (remove watch (inotify-watches inotify)))
  (inotify-rm-watch (inotify-fd inotify) (watch-id watch)))

(defstruct event
  watch
  mask
  cookie
  name)

(defun parse-event-name (event)
  (let* ((name (event-name event))
         (dot (position #\. name :from-end t)))
    (if dot
        (make-pathname :name (subseq name 0 dot)
                       :type (subseq name (1+ dot)))
        (make-pathname :name name))))

(defun event-full-name (event)
  (if (event-name event)
      (merge-pathnames (parse-event-name event)
                       (watch-pathname (event-watch event)))
      (watch-pathname (event-watch event))))

(defun read-event (inotify buffer)
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
      (values event
              (+ +event-size+ name-length)))))

(defun read-events (inotify)
  (let* ((buffer (inotify-buffer inotify))
	 (bytes-read
           (isys:repeat-upon-eintr
             (isys:read (inotify-fd inotify) 
                        buffer
                        (inotify-buffer-size inotify)))))
    (loop with event and event-length
          for offset = 0 then (+ offset event-length)
          while (< offset bytes-read)
          do (setf (values event event-length)
                   (read-event inotify (inc-pointer buffer offset)))
          collect event)))

(defun make-inotify-with-watches (paths-with-masks)
  (let ((inotify (make-inotify)))
    (loop for (path mask) in paths-with-masks
          do (add-watch inotify path mask))
    inotify))

(defmacro with-inotify ((name paths-with-masks) &body body)
  `(let ((,name (make-inotify-with-watches ,paths-with-masks)))
     (unwind-protect (progn ,@body)
       (close-inotify ,name))))
