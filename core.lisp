(in-package :stumpwm)

(setq *print-case* :downcase)

(defmacro bind (clauses &body body)
  "This macro combines the behaviour of the forms `let*',
`destructuring-bind', and `multiple-value-bind', permitting the
following style of binding form:

  (bind (((:values m n) (values 10 20))
         ((a b _c &key (d 10)) '(1 2 3))
         (x 5))
    (+ x a b d m n))
  => 48

Note in the destructuring form (a b _c &key (d 10)), _c is a short form
for declaring it as ignorable.

This is a more limited and lightweight implementation of some ideas from
metabang-bind (http://common-lisp.net/project/metabang-bind/)."
  (labels
      ((parse-arglist (args)
         (loop
           for arg in args
           collect arg into args
           when (and (symbolp arg) (eq (aref (symbol-name arg) 0) #\_))
             collect arg into ignorables
           finally (return (values args ignorables))))
       (cons-form (form args clauses body)
         (multiple-value-bind (arglist ignorables)
             (parse-arglist args)
           `(,form ,arglist
                   ,@(cdar clauses)
                   ,@(when ignorables `((declare ,(list* 'ignore ignorables))))
                   (bind ,(cdr clauses) ,@body)))))
    (cond
      ((null clauses) `(progn ,@body))
      ((listp (caar clauses))
       (cond
         ((eq (caaar clauses) :values)
          (cons-form 'multiple-value-bind (cdaar clauses) clauses body))
         ((eq (caaar clauses) :slots)
          `(with-slots ,(cdaar clauses) ,@(cdar clauses)
             (bind ,(cdr clauses) ,@body)))
         (t
          (cons-form 'destructuring-bind (caar clauses) clauses body))))
      (t
       `(let (,(car clauses))
          (bind ,(cdr clauses) ,@body))))))

(defmacro -> (x &rest args)
  "A Common-Lisp implementation of the Clojure `thrush` operator."
  (destructuring-bind (form &rest more)
      args
    (cond
      (more `(-> (-> ,x ,form) ,@more))
      ((and (consp form)
            (or (eq (car form) 'lambda)
                (eq (car form) 'function)))
       `(funcall ,form ,x))
      ((consp form) `(,(car form) ,x ,@(cdr form)))
      (form `(,form ,x))
      (t x))))

(defmacro ->> (x &rest args)
  "A Common-Lisp implementation of the Clojure `thrush` operator."
  (destructuring-bind (form &rest more)
      args
    (cond
      (more `(->> (->> ,x ,form) ,@more))
      ((and (consp form)
            (or (eq (car form) 'lambda)
                (eq (car form) 'function)))
       `(funcall ,form ,x))
      ((consp form) `(,(car form) ,@(cdr form) ,x))
      (form `(,form ,x))
      (t x))))

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

(defun get-unix-time ()
  (universal-to-unix-time (get-universal-time)))

(defun process-live-p (p)
  (and p (sb-ext:process-alive-p p)))

(defmacro push-max-stack (stack val max-depth)
  `(setq ,stack
         (cons ,val
               (if (<= ,max-depth (length ,stack))
                   (subseq ,stack 0 (1- ,max-depth))
                   ,stack))))

(defun scrunch (s &key maxlen)
  (let* ((len (- (or maxlen 6) 2))
         (half-len (floor (/ len 2))))
    (concat (subseq s 0 half-len)
            ".."
            (subseq s (- (length s) half-len)))))

(defun setenv (kvs)
  (dolist (kv kvs)
    (sb-posix:putenv
     (concatenate 'string (first kv) "=" (second kv)))))

(defvar *sh-echo-console* nil)
;; (setq *sh-echo-console* t)

(defun sh* (command args &optional collect-output-p wait-p)
  (when *sh-echo-console*
    (let ((cli (format nil "exec ~a ~{\"~a\"~^ ~}" command args)))
      (dformat 0 "sh*: ~a~%" cli)
      (force-output)))
  (if collect-output-p
      (with-output-to-string (s)
        (sb-ext:run-program command
                            args
                            :search t
                            :wait t
                            :output s
                            :error nil))
      (sb-ext:run-program command args :search t :wait wait-p)))

(defun sh (command &rest args)
  (sh* command args))

(defun sh< (command &rest args)
  (sh* command args t t))

(define-stumpwm-type :shell (input prompt)
  (declare (ignore prompt))
  (let ((prompt (format nil "~A -c exec " *shell-program*)))
    (or (argument-pop-rest input)
        (completing-read (current-screen) prompt 'complete-program))))

(defun mksym (&rest parts)
  (intern
   (apply 'concat
          (mapcar (lambda (p) (string-upcase (princ-to-string p))) parts))))

(defvar *cache-sentinel-value* (gensym "STUMPWM-SENTINEL"))
(defmacro defcached (name opts args &body body)
  (let ((last-time# (gensym))
        (val# (gensym))
        (now# (gensym)))
    (destructuring-bind (&key timeout)
        opts
      (let ((timeout (or timeout 15)))
        `(let ((,last-time# 0)
               (,val# nil))
           (defun ,name ,args
             (let ((,now# (get-unix-time)))
               (when (or (eq *cache-sentinel-value* ,val#)
                         (<= ,timeout (- ,now# ,last-time#)))
                 (dformat 1 "~&~S cache expired~%" ',name)
                 (setq ,last-time# ,now#
                       ,val# (progn ,@body)))
               ,val#))
           (defun ,(mksym name "-reset!") ()
             (setq ,val# *cache-sentinel-value*)))))))

(defvar *after-funs* nil)
(defmacro defafter (fname arglist &body body)
  `(let ((fdef (symbol-function (quote ,fname))))
     (push (cons (quote ,fname) fdef) *after-funs*)
     (defun ,fname (&rest args)
       (labels ((call-next-function (&rest args)
                  (apply fdef args)))
         (destructuring-bind ,arglist
             args
           ,@body)))))

(defun reset-defafter (fsym)
  (setf (symbol-function fsym) (cdr (assoc fsym *after-funs*))))

(defun bind-keys (keymap keydefs)
  (dolist (keydef keydefs)
    (destructuring-bind (keyseq command)
        keydef
      (define-key keymap (kbd keyseq) command))))


(defun set-window-background-color (win color)
  (setf (xlib:window-background win) color)
  (xlib:clear-area win))

(defun set-root-window-background-color (color)
  (set-window-background-color (screen-root (current-screen)) color))

;; (set-root-window-background-color *default-bg-color*)
;; (set-root-window-background-color 0)

(defun time-dow-shortname-2ch ()
  (subseq (time-dow-name) 0 2))

(defun string-not-empty (s)
  (when (and (stringp s) (< 0 (length s)))
    s))
