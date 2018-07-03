(in-package :stumpwm)

(setq *print-case* :downcase)

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

(defun string-maxlen (s maxlen)
  (let ((s1 (subseq s 0 (min maxlen (length s)))))
    (if (string-equal s1 s)
        s1
        (concat s1 " ..."))))

(define-stumpwm-type :shell (input prompt)
  (declare (ignore prompt))
  (let ((prompt (format nil "~A -c exec " *shell-program*)))
    (or (argument-pop-rest input)
        (completing-read (current-screen) prompt 'complete-program))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar audio-profile-choices
  '(("built-in" "built-in")
    ("bt-a2dp" "bt-a2dp")
    ("bt-headset" "bt-headset")
    ("usb-headset" "usb-headset")
    ("dp" "dp")))

(defcommand set-audio-profile () ()
  "Prompt with the list of audio profiles"
  (let ((profile (second
                  (select-from-menu (current-screen)
                                    audio-profile-choices
                                    nil))))
    (when profile
      (run-shell-command (format nil "set-audio-profile ~a" profile)))))

(define-key *root-map* (kbd "C-a") "set-audio-profile")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-remapped-keys
    '(("(Firefox|Chrome|Chromium|[Kk]eepass)"
       ("C-n"   . "Down")
       ("C-p"   . "Up")
       ("C-f"   . "Right")
       ("C-b"   . "Left")
       ("C-v"   . "Next")
       ("M-v"   . "Prior")
       ("M-w"   . "C-c")
       ("C-w"   . "C-x")
       ("C-y"   . "C-v")
       ("M-<"   . "Home")
       ("M->"   . "End")
       ("C-M-b" . "M-Left")
       ("C-M-f" . "M-Right")
       ("C-k"   . ("S-End" "C-x")))))

(define-key *root-map* (kbd "C-q") "send-raw-key")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcommand emacs () ()
  "Start emacs unless it is already running, in which case focus it."
  (run-or-raise "exec emacs" '(:class "Emacs")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-module "clipboard-history")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *group-undo-stack* nil)
(defvar *group-redo-stack* nil)
(defvar *group-stack-max-depth* 10)

(defun reset-group-stacks ()
  (setq *group-undo-stack* nil
        *group-redo-stack* nil))

;; (reset-group-stacks)
;; (length *group-undo-stack*)
;; (length *group-redo-stack*)

(defun push-group ()
  "Save current group placement"
  (push-max-stack *group-undo-stack*
                  (dump-group (current-group))
                  *group-stack-max-depth*))

(defun group-stack-pre-command-hook (command)
  (when (member command '(hsplit vsplit only) :test #'eq)
    (push-group)))

(add-hook *pre-command-hook* 'group-stack-pre-command-hook)

(defun pop-group ()
  "Restore last group placement"
  (let ((gdump (pop *group-undo-stack*)))
    (if gdump
        (progn
          (push-max-stack *group-redo-stack*
                          (dump-group (current-group))
                          *group-stack-max-depth*)
          (ignore-errors
            (restore-group (current-group) gdump)))
        (message "Can't go back anymore"))))

(defcommand frame-undo () ()
  "Undo frame changes"
  (pop-group))

(define-key *root-map* (kbd "C-b") "frame-undo")
(define-key *root-map* (kbd "C-y") "show-clipboard-history")

(defun redo-group ()
  "Redo last undone group change"
  (let ((gdump (pop *group-redo-stack*)))
    (if gdump
        (progn
          (push-group)
          (ignore-errors
            (restore-group (current-group) gdump)))
        (message "Can't go forward anymore"))))

(defcommand frame-redo () ()
  "Redo recently undone frame changes"
  (redo-group))

(define-key *root-map* (kbd "C-f") "frame-redo")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resize-width-pct (pct)
  (let* ((swidth (screen-width (current-screen)))
         (twidth (round (* pct swidth)))
         (group (current-group))
         (frame (tile-group-current-frame group)))
    (resize-frame group
                  frame
                  (- twidth (frame-width frame))
                  :width)))

(defcommand resize-33%-width () ()
  "Resize width of current frame to 33% of screen-width"
  (resize-width-pct 0.33))

(defcommand resize-66%-width () ()
  "Resize width of current frame to 66% of screen-width"
  (resize-width-pct 0.66))

(defcommand resize-75%-width () ()
  "Resize width of current frame to 75% of screen-width"
  (resize-width-pct 0.75))

(defcommand (fprev tile-group) () ()
  "Cycle through the frame tree to the next frame."
  (focus-prev-frame (current-group)))

(defvar ctlx-map (make-sparse-keymap))
(define-key ctlx-map (kbd "0") "remove")
(define-key ctlx-map (kbd "1") "only")
(define-key ctlx-map (kbd "2") "vsplit")
(define-key ctlx-map (kbd "3") "hsplit")
(define-key ctlx-map (kbd "6") "resize-66%-width")
(define-key ctlx-map (kbd "7") "resize-33%-width")
(define-key ctlx-map (kbd "8") "resize-75%-width")
(define-key ctlx-map (kbd "+") "balance-frames")
(define-key ctlx-map (kbd "C-y") "show-clipboard-history")
(define-key ctlx-map (kbd "p") "fprev")
(define-key ctlx-map (kbd "n") "fnext")

(define-key *root-map* (kbd "C-x") 'ctlx-map)

(define-key *root-map* (kbd "p") "fprev")
(define-key *root-map* (kbd "n") "fnext")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *screensaver-proc* nil)

(defun start-screen-saver* (&optional verbosep)
  (run-shell-command "exec xset +dpms")
  (unless (process-live-p *screensaver-proc*)
    (when verbosep
      (message "Starting screen saver..."))
    (setq *screensaver-proc*
          (run-shell-command "exec xscreensaver -no-splash"))))

(defun stop-screen-saver* (&optional verbosep)
  (run-shell-command "exec xset -dpms")
  (when (process-live-p *screensaver-proc*)
    (when verbosep
      (message "Stopping screen saver..."))
    (run-shell-command "exec xscreensaver-command -exit")
    (setq *screensaver-proc* nil)))

(defcommand start-screen-saver () ()
  "Start screen saver"
  (start-screen-saver* :verbose))

(defcommand stop-screen-saver () ()
  "Stop screen saver"
  (stop-screen-saver* :verbose))

(defcommand activate-screen-saver () ()
  "Activate screen saver"
  (run-shell-command "exec xscreensaver-command -activate"))

(defcommand lock-screen () ()
  "Lock the screen, and power down the display"
  (run-shell-command "exec xset dpms force off")
  (sleep 1)
  (run-shell-command "exec xscreensaver-command -lock"))

(define-key *top-map* (kbd "s-Pause") "lock-screen")
(define-key *top-map* (kbd "s-F9") "lock-screen")

(defcommand suspend () ()
  "Suspend the system"
  (run-shell-command "exec systemctl suspend"))

(define-key *root-map* (kbd "z") "suspend")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; (reset-defafter 'activate-fullscreen)
;; (reset-defafter 'deactivate-fullscreen)

(defvar *fullscreen-hook* nil)

(defun apply-hook (hook &rest args)
  (when hook
    (dolist (fn hook)
      (apply fn args))))

(defafter activate-fullscreen (window)
  (call-next-function window)
  (apply-hook *fullscreen-hook* window :fullscreen))

(defafter deactivate-fullscreen (window)
  (call-next-function window)
  (apply-hook *fullscreen-hook* window :normal))

(defun toggle-screensaver-on-fullscreen (window state)
  (declare (ignore window))
  (cond
    ((eq :fullscreen state) (stop-screen-saver* :verbose))
    ((eq :normal state) (start-screen-saver* :verbose))))

(add-hook *fullscreen-hook* 'toggle-screensaver-on-fullscreen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-module "amixer")
(in-package :amixer)
(defvolcontrol amixer-Master-10- "Master" "10%-")
(defvolcontrol amixer-Master-10+ "Master" "10%+")

(in-package :stumpwm)

;; ;; load Swank so we can connect with SLIME
(load ".emacs.d/el-get/slime/swank-loader.lisp")
(swank-loader:init)
(defvar *swank-p* nil)

;; ;; define swank command to start swank server on port 4005
(defcommand swank () ()
  "Starts a swank server on port 4005 and notifies the user."
  (setf stumpwm:*top-level-error-action* :break)
  (if *swank-p*
      (message "Swank server already running.")
      (progn
        (swank:create-server :port 4005
                             :style swank:*communication-style*
                             :dont-close t)
        (setf *swank-p* t)
        (message "Starting swank on port 4005."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcommand term () ()
  "Launch or raise a terminal window"
  (run-or-pull "exec gnome-terminal" '(:class "Gnome-terminal")))

(define-key *root-map* (kbd "c") "term")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key *top-map* (kbd "XF86AudioLowerVolume") "amixer-Master-10- pulse")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "amixer-Master-10+ pulse")
(define-key *top-map* (kbd "S-XF86AudioLowerVolume") "amixer-Master-1- pulse")
(define-key *top-map* (kbd "S-XF86AudioRaiseVolume") "amixer-Master-1+ pulse")
(define-key *top-map* (kbd "XF86AudioMute") "amixer-Master-toggle pulse")

(define-key *top-map* (kbd "XF86AudioPlay") "exec emacsclient -e '(emms-pause)'")
(define-key *top-map* (kbd "XF86AudioStop") "exec emacsclient -e '(emms-stop)'")
(define-key *top-map* (kbd "XF86AudioPrev") "exec emacsclient -e '(emms-previous)'")
(define-key *top-map* (kbd "XF86AudioNext") "exec emacsclient -e '(emms-next)'")

(define-key *root-map* (kbd "SPC") "exec emacsclient -e '(emms-pause)'")
(define-key *root-map* (kbd "C-o") "fnext")
(define-key *root-map* (kbd "w") "windowlist")
(define-key *root-map* (kbd "C-w") "windowlist")

(defun change-brightness (amount)
  (run-shell-command (concat "exec xbacklight "
                             (if (< 0 amount) "-inc" "-dec")
                             " "
                             (prin1-to-string (abs amount))))
  (let* ((*read-eval* nil)
         (current (read-from-string (run-shell-command "exec xbacklight -get" t)))
         (percent (truncate current)))
    (message
     (concat "Brightness: "
             (format nil "~C^B~A%" #\Newline percent)
             (bar percent 50 #\# #\:) "^]]"))))

(defcommand lower-brightness () ()
  "Lower the brightness"
  (change-brightness -1.0))

(defcommand raise-brightness () ()
  "Raise the brightness"
  (change-brightness 1))

(define-key *top-map* (kbd "XF86MonBrightnessUp") "raise-brightness")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "lower-brightness")

(defvar *redshift-proc* nil)

(defcommand redshift-on () ()
  "Enable redshift"
  (unless (process-live-p *redshift-proc*)
    (setq *redshift-proc* (run-shell-command "exec redshift"))))

(defcommand redshift-off () ()
  "Enable redshift"
  (when (process-live-p *redshift-proc*)
    (sb-ext:process-kill *redshift-proc* sb-unix:sigint)
    (setq *redshift-proc* nil)))

(defcommand chromium () ()
  "Launch or raise chromium"
  (run-or-raise "exec chromium-browser" '(:class "Chromium-browser")))

(defcommand conkeror () ()
  "Launch or raise conkeror"
  (run-or-raise "exec conkeror" '(:class "Conkeror")))

(defcommand firefox () ()
  "Launch or raise firefox"
  (run-or-raise "exec /app/firefox/firefox" '(:class "Firefox")))

(defcommand pfirefox () ()
  "Launch or raise private mode firefox"
  (run-shell-command
   "exec /app/firefox/firefox -private -safe-mode -no-remote"
   '(:class "Firefox")))

(defun shell-commands (&rest commands)
  (dolist (command commands)
    (run-shell-command (concat "exec " command))))

(defun setup-touchpad ()
  "Configure touchpad"
  (shell-commands
   (concat
    "synclient"
    " VertTwoFingerScroll=1"
    " VertScrollDelta=-111"
    " HorizScrollDelta=-111"
    " HorizTwoFingerScroll=1"
    " TapButton1=1"
    " TapButton2=3"
    " TapButton3=2"
    " PalmDetect=1")))

(defun capslock-as-control ()
  "Make CapsLock a Control key"
  (run-shell-command "exec setxkbmap -option ctrl:nocaps"))

(defun init-mouse-pointer ()
  (shell-commands "xsetroot -cursor_name left_ptr"))

(defcommand gnome-settings-daemon () ()
  "Run the gnome-settings-daemon"
  (run-shell-command
   "exec /usr/lib/gnome-settings-daemon/gsd-xsettings"))

(defcommand enable-external-display () ()
  "Enable an external display"
  (run-shell-command
   "exec xrandr --output eDP1 --off --output DP2-2 --auto"))

(load-module "cpu")
(load-module "mem")
(load-module "wifi")

(load-module "battery-portable")
(in-package :battery-portable)
(export '(sysfs-int-field-or-nil))

(in-package :stumpwm)

(defun fmt-power-source (ml)
  (declare (ignore ml))
  (if (= 1
         (battery-portable:sysfs-int-field-or-nil
          "/sys/class/power_supply/AC/"
          "online"))
      "AC"
      "BAT"))
(add-screen-mode-line-formatter #\W #'fmt-power-source)

(defvar *mu-cmd* "./.emacs.d/el-get/mu4e/mu/mu")

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
    (apply #'sb-ext:run-program prog args :output (if output output t)
           :error (if error error t) :wait wait :environment env opts)))

(defun fmt-mail-biff (mb)
  (declare (ignore mb))
  (let* ((out (ignore-errors
                (let ((err (make-string-output-stream)))
                  (with-output-to-string (s)
                    (run-prog *mu-cmd*
                              :args '("find"
                                      "flag:unread"
                                      "-f" "m,f,s"
                                      "-u")
                              :error err
                              :output s
                              :wait t)))))
         (msgs (unless (string-equal "" out)
                 (split-string out)))
         (msgs1 (remove-if (lambda (msg)
                             (cl-ppcre:scan "All Mail" msg))
                           msgs))
         (cnt (length msgs1)))
    (format nil
            (if (positive-integer-p cnt)
                "^B^3*~A:~D^*^b"
                "~A:~D")
            "mu"
            cnt)))

;; (fmt-mail-biff 0)

(add-screen-mode-line-formatter #\U 'fmt-mail-biff)

;; startup
(setq *window-format* "%m%n%s%10t"
      *time-modeline-string* "%a %b %e %k:%M"
      *screen-mode-line-format* (concat "%3n | "
                                        "%v"
                                        "^>"
                                        " | %U"
                                        " | %c,%M"
                                        " | %W: %B"
                                        " | %I"
                                        " | %d")
      *mode-line-timeout* 10
      *message-window-gravity* :top
      *message-window-padding* 5
      *input-window-gravity* :top
      *window-border-style* :thin
      *normal-border-width* 1)

(set-normal-gravity :bottom)
(set-transient-gravity :top)
;; (set-fg-color "black")
;; (set-bg-color "black")
;; (set-border-color "black")
(set-win-bg-color "white")
(set-focus-color "green")
(set-msg-border-width 1)
(ql:quickload :clx-truetype)
(xft:cache-fonts)
(load-module "ttf-fonts")
;; (clx-truetype:get-font-families)
;; (clx-truetype:get-font-subfamilies "DejaVu Sans Mono")
(set-font
 (make-instance 'xft:font
                :family "DejaVu Sans Mono"
                :subfamily "Book"
                :size 12))

(add-hook *quit-hook* 'redshift-off)

(defun turn-on-mode-line-timer ()
  (when (timer-p *mode-line-timer*)
    (cancel-timer *mode-line-timer*))
  (setf *mode-line-timer*
        (run-with-timer (- 10 (mod (get-decoded-time) 10))
                        *mode-line-timeout*
                        'update-all-mode-lines)))

(toggle-mode-line (current-screen) (current-head))

(defvar title-remaps nil)
(setq title-remaps
      '(("Gnome-terminal" . "term")
        ("ubuntu-terminal-app" . "term")
        ("\"chromium-browser\"" . "chrom")
        ("\"Chromium-browser\"" . "chrom")
        ("\"chromium-browser\", \"Chromium-browser\"" . "chrom")
        ("Conkeror" . "conk")
        ("Firefox" . "fox")
        ("Emacs24" . "emacs")))

(defun new-window-customizations (win)
  (let ((title (clextn:-> win
                          window-class
                          (assoc title-remaps :test 'equal)
                          cdr)))
    (setf (window-user-title win)
          (or title
              (let ((title (window-title win)))
                (cond
                  ((and title (< 5 (length title)))
                   (subseq (string-downcase title) 0 5))

                  (title title)

                  (t "")))))))

(add-hook *new-window-hook* 'new-window-customizations)

(gnome-settings-daemon)
(capslock-as-control)
(shell-commands "xsetroot -bg black")
(init-mouse-pointer)
(setup-touchpad)
(redshift-on)
(start-screen-saver)
(clipboard-history:start-clipboard-manager)
;; (clipboard-history:stop-clipboard-manager)

(run-shell-command "exec dropbox start")

(emacs)
(firefox)
(chromium)
(swank)
