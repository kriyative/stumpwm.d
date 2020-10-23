(in-package :stumpwm)

(ql:quickload :bt-semaphore)
(find-package :bt)

(defun mbsync-parse (output)
  (let (mboxes mbox)
    (loop for row in (split-string output)
          do
             (multiple-value-bind (s m)
                 (cl-ppcre:scan-to-strings "Opening master box ([^.]+)..." row)
               (declare (ignore s))
               (if m
                   (setq mbox (aref m 0))
                   (multiple-value-bind (s m)
                       (cl-ppcre:scan-to-strings "master: ([0-9]+) messages, ([0-9]+) recent"
                                                 row)
                     (declare (ignore s))
                     (when m
                       (push (list mbox
                                   (parse-integer (aref m 0))
                                   (parse-integer (aref m 1)))
                             mboxes))))))
    mboxes))

(defun mbsync (mb)
  (dformat 0 "~&syncing ~a~%" mb)
  (handler-case
      (let ((output (sh< "mbsync" "-V" mb)))
        (destructuring-bind (mbox total unread)
            (assoc "INBOX" (mbsync-parse output) :test 'equal)
          (dformat 0 "~&synced ~a (~a): ~d/~d~%" mb mbox unread total)))
    (condition (c)
      (dformat 0 "~&sync error ~a: ~s~%" mb c))))

(defvar *mbsync-threads* nil)
(defvar *mbsync-state* :exit)

(defun mbsync-stop ()
  (setq *mbsync-state* :exit))

;; (mbsync-stop)

(defun mbsync-start (mb-specs)
  (mbsync-stop)
  (setq *mbsync-state* :running)
  (dolist (mb-spec mb-specs)
    (destructuring-bind (mb timeout)
        mb-spec
      (push (bt:make-thread (lambda ()
                              (loop
                                until (eq :exit *mbsync-state*)
                                do
                                   (mbsync mb)
                                   (sleep timeout))
                              (dformat 0 "~&exiting ~a~%" (bt:current-thread)))
                            :name (concat "mbsync-thread-" mb))
            *mbsync-threads*))))
