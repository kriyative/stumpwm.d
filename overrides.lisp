(in-package :stumpwm)

;; head.lisp

(defvar *enable-randr-extension-for-screens-query* nil
  "Set this to `t` to use the RANDR extension for querying the
  screens/heads available to the current display. This config is being
  introduced since the RANDR extension doesn't seem to work
  consistently under all X11 setups.")

(defun make-screen-heads (screen root)
  (declare (ignore screen))
  (cond ((and (xlib:query-extension *display* "RANDR")
              *enable-randr-extension-for-screens-query*)
         (make-screen-randr-heads root))
        ((and (xlib:query-extension *display* "XINERAMA")
              (xinerama:xinerama-is-active *display*))
         (mapcar 'screen-info-head
                 (xinerama:xinerama-query-screens *display*)))
        (t (list (make-head :number 0 :x 0 :y 0
                            :width (xlib:drawable-width root)
                            :height (xlib:drawable-height root)
                            :window nil)))))

;; input.lisp

(defun read-one-line (screen prompt &key completions (initial-input "") require-match password)
  "Read a line of input through stumpwm and return it. Returns nil if the user aborted."
  (let ((*input-last-command* nil)
        (*input-completions* completions)
        (input (make-input-line :string (make-input-string initial-input)
                                :position (length initial-input)
                                :history -1
                                :password password)))
    (labels ((match-input ()
               (let* ((in (string-trim " " (input-line-string input)))
                      (compls (input-find-completions in completions)))
                 (and (consp compls)
                      (string= in (if (consp (car compls))
                                      (caar compls)
                                      (car compls))))))
             (key-loop ()
               (loop for key = (read-key-or-selection) do
                    (cond ((stringp key)
                           ;; handle selection
                           (input-insert-string input key)
                           (draw-input-bucket screen prompt input))
                          ;; skip modifiers
                          ((is-modifier (car key)))
                          ((process-input screen prompt input (car key) (cdr key))
                           (if (or (not require-match)
                                   (match-input))
                               (return (input-line-string input))
                               (draw-input-bucket screen prompt input "[No match]" t)))))))
      (setup-input-window screen prompt input)
      (catch :abort
        (unwind-protect
             (with-focus (screen-input-window screen)
               (key-loop))
          (shutdown-input-window screen))))))

(defun draw-input-bucket (screen prompt input &optional (tail "") errorp)
  "Draw to the screen's input window the contents of input."
  (let* ((gcontext (screen-message-gc screen))
         (win (screen-input-window screen))
         (font (screen-font screen))
         (prompt-lines (ppcre:split #\Newline prompt))
         (prompt-lines-length (length prompt-lines))
         (prompt-width (loop :for line :in prompt-lines
                             :maximize (text-line-width font line :translate #'translate-id)))
         (prompt-offset (text-line-width font
                                         (first (last prompt-lines))
                                         :translate #'translate-id))
         (line-content (input-line-string input))
         (string (if (input-line-password input)
                     (make-string (length line-content) :initial-element #\*)
                     line-content))
         (string-width (loop for char across string
                          summing (text-line-width (screen-font screen)
                                                   (string char)
                                                   :translate #'translate-id)))
         (space-width  (text-line-width (screen-font screen) " "    :translate #'translate-id))
         (tail-width   (text-line-width (screen-font screen) tail   :translate #'translate-id))
         (full-string-width (+ string-width space-width))
         (pos (input-line-position input))
         (width (max prompt-width
                     (+ prompt-offset
                        (max 100 (+ full-string-width space-width tail-width))))))
    (when errorp (rotatef (xlib:gcontext-background gcontext)
                          (xlib:gcontext-foreground gcontext)))
    (xlib:with-state (win)
      (xlib:with-gcontext (gcontext :foreground (xlib:gcontext-background gcontext))
        (xlib:draw-rectangle win gcontext 0 0
                             (xlib:drawable-width win)
                             (xlib:drawable-height win) t))
      (setf (xlib:drawable-width win) (+ width (* *message-window-padding* 2)))
      (setf (xlib:drawable-height win) (+ (* prompt-lines-length (font-height font))
                                          (* *message-window-y-padding* 2)))
      (setup-win-gravity screen win *input-window-gravity*)

      ;; Display the input window text.
      (loop for i from 0 below prompt-lines-length
         if (< i prompt-lines-length)
         do (draw-image-glyphs win gcontext font
                               *message-window-padding*
                               (prompt-text-y i font *message-window-y-padding*)
                               (nth i prompt-lines)
                               :translate #'translate-id
                               :size 16))
      ;; Pad the input to the left.
      (loop with x = (+ *message-window-padding* prompt-offset)
         for char across string
         for i from 0 below (length string)
         for char-width = (text-line-width (screen-font screen) (string char) :translate #'translate-id)
         if (= pos i)
         do (xlib:with-gcontext (gcontext :foreground (xlib:gcontext-background gcontext)
                                          :background (xlib:gcontext-foreground gcontext))
              (draw-image-glyphs win gcontext (screen-font screen)
                                 x
                                 (prompt-text-y (1- prompt-lines-length)
                                                font
                                                *message-window-y-padding*)
                                 (string char)
                                 :translate #'translate-id
                                 :size 16))
         else
         do (draw-image-glyphs win gcontext (screen-font screen)
                               x
                               (prompt-text-y (1- prompt-lines-length)
                                              font
                                              *message-window-y-padding*)
                               (string char)
                               :translate #'translate-id
                               :size 16)
         end
         do (incf x char-width)
         finally (when (>= pos (length string))
                   (xlib:with-gcontext (gcontext :foreground (xlib:gcontext-background gcontext)
                                                 :background (xlib:gcontext-foreground gcontext))
                     (draw-image-glyphs win gcontext (screen-font screen)
                                        x
                                        (prompt-text-y (1- prompt-lines-length)
                                                       font
                                                       *message-window-y-padding*)
                                        " "
                                        :translate #'translate-id
                                        :size 16))))
      (draw-image-glyphs win gcontext (screen-font screen)
                         (+ *message-window-padding* prompt-offset full-string-width space-width)
                         (prompt-text-y (1- prompt-lines-length)
                                        font
                                        *message-window-y-padding*)
                         tail
                         :translate #'translate-id
                         :size 16))
    (when errorp
      (sleep 0.05)
      (rotatef (xlib:gcontext-background gcontext)
               (xlib:gcontext-foreground gcontext))
      (draw-input-bucket screen prompt input tail))))

;; wrappers.lisp

(defun run-prog (prog &rest opts &key args output (wait t) error &allow-other-keys)
  "Common interface to shell. Does not return anything useful."
  (remf opts :args)
  (remf opts :output)
  (remf opts :wait)
  (let ((env (sb-ext:posix-environ)))
    (when (current-screen)
      (setf env (cons (screen-display-string (current-screen) t)
                      (remove-if (lambda (str)
                                   (string= "DISPLAY=" str
                                            :end2 (min 8 (length str))))
                                 env))))
    (sb-posix:putenv "_JAVA_AWT_WM_NONREPARENTING=1")
    (apply #'sb-ext:run-program prog args :output (if output output t) :search t
           :error (if error error t) :wait wait :environment env opts)))


;; mode-line.lisp

(defvar *mode-line-brightness* 0.25)

(defun update-mode-line-color-context (ml)
  (let* ((cc (mode-line-cc ml))
         (screen (mode-line-screen ml))
         (bright (lookup-color screen *mode-line-foreground-color*)))
    (adjust-color bright *mode-line-brightness*)
    (setf (ccontext-default-bright cc) (alloc-color screen bright))))
