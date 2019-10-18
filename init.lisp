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

(defun sh* (command args &optional collect-output-p)
  (let ((cli (format nil "exec ~a ~{\"~a\"~^ ~}" command args)))
    (dformat 0 "sh*: ~a ~a~%" cli (if collect-output-p :collect-output))
    (force-output)
    (if collect-output-p
        (apply 'run-prog-collect-output command args)
        (run-prog command :args args :wait nil))))

(defun sh (command &rest args)
  (sh* command args))

(defun sh< (command &rest args)
  (sh* command args t))

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
      (sh "set-audio-profile" profile))))

;; (define-key *root-map* (kbd "C-a") "set-audio-profile")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *screensaver-proc* nil)

(defun start-screen-saver* (&optional verbosep)
  (sh "xset" "+dpms")
  (unless (process-live-p *screensaver-proc*)
    (when verbosep
      (message "Starting screen saver..."))
    (setq *screensaver-proc*
          (sh "xscreensaver" "-no-splash"))))

(defun stop-screen-saver* (&optional verbosep)
  (sh "xset" "-dpms")
  (when (process-live-p *screensaver-proc*)
    (when verbosep
      (message "Stopping screen saver..."))
    (sh "xscreensaver-command" "-exit")
    (setq *screensaver-proc* nil)))

(defcommand start-screen-saver () ()
  "Start screen saver"
  (start-screen-saver* :verbose))

(defcommand stop-screen-saver () ()
  "Stop screen saver"
  (stop-screen-saver* :verbose))

(defcommand activate-screen-saver () ()
  "Activate screen saver"
  (sh "xscreensaver-command" "-activate"))

(defcommand lock-screen () ()
  "Lock the screen, and power down the display"
  (sh "xset" "dpms" "force" "off")
  (sleep 1)
  (sh "xscreensaver-command" "-lock"))

(defcommand suspend () ()
    "Suspend the system"
  (lock-screen)
  (sleep 1)
  (sh "systemctl" "suspend"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (run-or-pull "exec urxvt" '(:class "URxvt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcommand audio-pause () ()
  (sh "emacsclient" "-e" "(emms-pause)"))
(defcommand audio-stop () ()
  (sh "emacsclient" "-e" "(emms-stop)"))
(defcommand audio-previous () ()
  (sh "emacsclient" "-e" "(emms-previous)"))
(defcommand audio-next () ()
  (sh "emacsclient" "-e" "(emms-next)"))

(defun get-brightness ()
  (let* ((*read-eval* nil)
         (current (read-from-string
                   (sh< "xbacklight" "-get"))))
    (round current)))

(defcommand change-brightness (amount) ((:number "Amount: "))
  (let* ((cur (get-brightness))
         (next (min 100 (max 0 (+ cur amount)))))
    (sh "xbacklight" "-set" next)
    (message
     (concat "Brightness: "
             (format nil "~C^B~A%" #\Newline next)
             (bar next 50 #\# #\:) "^]]"))))

(defvar *redshift-proc* nil)

(defcommand redshift-on () ()
  "Enable redshift"
  (unless (process-live-p *redshift-proc*)
    (setq *redshift-proc* (sh "redshift"))))

(defcommand redshift-off () ()
  "Enable redshift"
  (when (process-live-p *redshift-proc*)
    (sb-ext:process-kill *redshift-proc* sb-unix:sigint)
    (setq *redshift-proc* nil)))

(defcommand chromium () ()
  "Launch or raise chromium"
  (run-or-raise "exec chromium-browser" '(:class "Chromium-browser")))

(defcommand firefox () ()
  "Launch or raise firefox"
  (run-or-raise "exec firefox" '(:class "Firefox")))

(defcommand pfirefox () ()
  "Launch or raise private mode firefox"
  (run-or-raise
   "firefox -private -safe-mode -no-remote"
   '(:class "Firefox")))

(defun setup-touchpad ()
  "Configure touchpad"
  (sh "synclient"
      "VertTwoFingerScroll=1"
      "VertScrollDelta=-111"
      "HorizScrollDelta=-111"
      "HorizTwoFingerScroll=1"
      "TapButton1=1"
      "TapButton2=3"
      "TapButton3=2"
      "PalmDetect=1")
  (sb-posix:putenv "GDK_CORE_DEVICE_EVENTS=1"))

(defcommand capslock-as-hyper () ()
  "Make CapsLock a Hyper key"
  (sh "setxkbmap" "-option" "")
  ;; (sh "setxkbmap" "-option" "ctrl:ralt_rctrl")
  (sh "xmodmap"
      "-e" "remove mod4 = Hyper_L"
      "-e" "keycode 66 = Hyper_L"
      "-e" "clear mod2"
      "-e" "clear mod3"
      "-e" "clear lock"
      "-e" "add mod3 = Hyper_L"))

;; (capslock-as-hyper)

(defcommand capslock-as-control () ()
  "Make CapsLock a Control key"
  (sh "setxkbmap" "-option" "ctrl:nocaps"))

;; (capslock-as-control)

(defun init-mouse-pointer ()
  (sh "xsetroot" "-cursor_name" "left_ptr")
  (sh "xinput" "-set-ptr-feedback" "11" "0" "1" "16"))

(defcommand gnome-settings-daemon () ()
  "Run the gnome-settings-daemon"
  (sh "/usr/lib/gnome-settings-daemon/gsd-xsettings"))

(defcommand enable-external-display () ()
  "Enable an external display"
  (sh "xrandr" "--output" "eDP1" "--off" "--output" "DP2-2" "--auto"))

(defcommand internal-display () ()
  (sh "xrandr" "--output" "eDP1" "--mode" "1920x1080" "--output" "DP2-2" "--off"))

(defcommand gnome-screenshot-screen () ()
  "Take a screenshot of whole screen"
  (sh "gnome-screenshot"))

(defcommand gnome-screenshot-screen-select () ()
  "Take a screenshot of selected portion of screen"
  (sh "gnome-screenshot" "-a"))

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
    (sb-posix:putenv "_JAVA_AWT_WM_NONREPARENTING=1")
    (apply #'sb-ext:run-program prog args :output (if output output t) :search t
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
(progn
  (setq *window-format* "%m%n%s%10t"
        *time-modeline-string* "%a %b %e %k:%M"
        *screen-mode-line-format* (concat "%3n | "
                                          "%v"
					  ;; "^B%v^b"
                                          "^>"
                                          " %U"
                                          " | %C| %M"
                                          "| %W: %B"
                                          " | %I"
                                          " | %d")
        cpu::*cpu-modeline-fmt* "%c"
        *hidden-window-color* "^7*"
        *mode-line-timeout* 10
        *mode-line-position* :top
        *message-window-gravity* :center
        *message-window-padding* 5
        *input-window-gravity* :center
        *window-border-style* :thick
        *normal-border-width* 1
        *timeout-wait* 2)
  (sync-all-frame-windows (current-group)))

(set-normal-gravity :top)
(set-transient-gravity :center)
(set-fg-color "white")
(set-bg-color "black")
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

;; set window-class property of mode-lines - used by compton
(dolist (m *mode-lines*)
  (xlib:set-wm-class (mode-line-window m)
                     "StumpwmModeline"
                     "stumpwm-modeline"))

(defvar title-remaps nil)
(setq title-remaps
      '(("Gnome-terminal" . "term")
        ("ubuntu-terminal-app" . "term")
        ("urxvt" . "term")
        ("Chromium-browser" . "chromi")
        ("Google-chrome" . "gchrom")
        ("Conkeror" . "conk")
        ("Firefox" . "fox")
        ("Emacs26" . "emacs")
        ("Emacs" . "emacs")
        ("keepassxc" . "kpass")))

(defun new-window-customizations (win)
  (let ((title-max-len 10)
        (title (cdr
                (assoc (window-class win)
                       title-remaps
                       :test 'equal))))
    (setf (window-user-title win)
          (or title
              (let ((title (window-title win)))
                (cond
                  ((and title (< title-max-len (length title)))
                   (subseq (string-downcase title) 0 title-max-len))

                  (title title)

                  (t "")))))))

(add-hook *new-window-hook* 'new-window-customizations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *default-remapped-keys*
  '(("C-n"   . "Down")
    ("C-p"   . "Up")))

(define-remapped-keys
    `(("(.*[Cc]hrom|[Ff]irefox|[Ee]vince|keepassxc|libreoffice)"
       ,@*default-remapped-keys*
       ("C-a"   . "Home")
       ("C-e"   . "End")
       ("C-f"   . "Right")
       ("C-b"   . "Left")
       ("C-v"   . "Next")
       ("M-b"   . "C-Left")
       ("M-f"   . "C-Right")
       ("M-v"   . "Prior")
       ("M-w"   . "C-c")
       ("C-w"   . "C-x")
       ("C-y"   . "C-v")
       ("M-<"   . "C-Home")
       ("M->"   . "C-End")
       ("C-M-b" . "M-Left")
       ("C-M-f" . "M-Right")
       ("C-k"   . ("S-End" "C-x"))
       ("M-k"   . "C-k")
       ("C-d"   . "Delete"))))

(defun bind-keys (keymap keydefs)
  (dolist (keydef keydefs)
    (destructuring-bind (keyseq command)
        keydef
      (define-key keymap (kbd keyseq) command))))

(defun select-window-from-menu-1 (windows fmt &optional prompt
                                                (filter-pred *window-menu-filter*))
  "Allow the user to select a window from the list passed in @var{windows}.  The
@var{fmt} argument specifies the window formatting used.  Returns the window
selected."
  (second (select-from-menu (current-screen)
                            (mapcar (lambda (w)
                                      (list (format-expand *window-formatters* fmt w) w))
                                    windows)
                            prompt
                            (or (position (current-window) windows) 0) ; Initial selection
                            (let ((m (make-sparse-keymap)))
                              (define-key m (kbd "M-S-TAB") 'menu-up)
                              (define-key m (kbd "S-TAB") 'menu-up)
                              (define-key m (kbd "M-TAB") 'menu-down)
                              (define-key m (kbd "TAB") 'menu-down)
                              m)
                            filter-pred)))

(defcommand cycle-windowlist (&optional (fmt *window-format*)
                                        window-list) (:rest)
  "Allow the user to select a window from the list of windows and focus the
selected window. For information of menu bindings see @ref{Menus}. The optional
 argument @var{fmt} can be specified to override the default window formatting.
The optional argument @var{window-list} can be provided to show a custom window
list (see @command{windowlist-by-class}). The default window list is the list of
all window in the current group. Also note that the default window list is sorted
by number and if the @var{windows-list} is provided, it is shown unsorted (as-is)."
  ;; Shadowing the window-list argument.
  (if-let ((window-list (or window-list
                            (sort-windows-by-number
                             (group-windows (current-group))))))
          (if-let ((window (select-window-from-menu-1 window-list fmt)))
                  (group-focus-window (current-group) window)
                  (throw 'error :abort))
          (message "No Managed Windows")))

(bind-keys
 *top-map*
 '(("s-!" "exec")
   ("s-&" "exec")
   ("s-+" "balance-frames")
   ("s--" "remove")
   ("s-:" "eval")
   ("s-;" "colon")
   ;; ("s-a" "ratclick 1")
   ;; ("s-s" "ratclick 2")
   ("s-Q" "only")
   ("s-`" "only")
   ("s-S" "hsplit")
   ("s-h" "hsplit")
   ("s-d" "remove")
   ("s-\\" "hsplit")
   ("s-k" "delete")
   ("s-l" "lock-screen")
   ("s-n" "fnext")
   ("s-o" "fother")
   ("s-p" "fprev")
   ("s-q" "send-raw-key")
   ("s-r" "remove")
   ("s-s" "vsplit")
   ("s-u" "frame-undo")
   ("s-w" "windowlist")
   ("s-x" "colon")
   ("s-y" "show-clipboard-history")
   ("s-SPC" "audio-pause")
   ("XF86AudioLowerVolume" "amixer-Master-10- pulse")
   ("S-XF86AudioLowerVolume" "amixer-Master-1- pulse")
   ("XF86AudioRaiseVolume" "amixer-Master-10+ pulse")
   ("S-XF86AudioRaiseVolume" "amixer-Master-1+ pulse")
   ("XF86AudioMute" "amixer-Master-toggle pulse")
   ("XF86AudioPlay" "audio-pause")
   ("XF86AudioStop" "audio-stop")
   ("XF86AudioPrev" "audio-previous")
   ("XF86AudioNext" "audio-next")
   ("XF86MonBrightnessUp" "change-brightness 10")
   ("S-XF86MonBrightnessUp" "change-brightness 1")
   ("XF86MonBrightnessDown" "change-brightness -10")
   ("S-XF86MonBrightnessDown" "change-brightness -1")
   ("F9" "change-brightness -10")
   ("S-F9" "change-brightness -1")
   ("F10" "change-brightness 10")
   ("S-F10" "change-brightness 1")

   ("s-Left" "ratrelwarp -10 0")
   ("s-Right" "ratrelwarp 10 0")
   ("s-Up" "ratrelwarp 0 -10")
   ("s-Down" "ratrelwarp 0 10")
   ("s-F9" "lock-screen")
   ("s-Pause" "audio-pause")
   ("M-TAB" "cycle-windowlist")
   ("Print" "gnome-screenshot-screen")
   ("C-Print" "gnome-screenshot-screen-select")))

(bind-keys
 *top-map*
 (loop
    for i from 0 to 9
    collect (list
             (format nil "s-~d" i)
             (format nil "select-window-by-number ~d" i))))
(bind-keys
 *top-map*
 (loop
    for i from 0 to 9
    collect (list
             (format nil "s-C-~d" i)
             (format nil "pull ~d" i))))

(defvar *audio-map* (make-sparse-keymap))
(bind-keys
 *audio-map*
 '(("n" "audio-next")
   ("p" "audio-previous")
   ("d" "set-audio-profile")
   ("C-d" "set-audio-profile")
   ("SPC" "audio-pause")))

(defvar *ctlx-map* (make-sparse-keymap))
(bind-keys
 *ctlx-map*
 '(("0" "remove")
   ("1" "only")
   ("2" "vsplit")
   ("3" "hsplit")
   ("6" "resize-66%-width")
   ("7" "resize-33%-width")
   ("8" "resize-75%-width")
   ("+" "balance-frames")
   ("C-y" "show-clipboard-history")
   ("p" "fprev")
   ("n" "fnext")))

(bind-keys
 *root-map*
 '(("C-q" "send-raw-key")
   ("SPC" "audio-pause")
   ("C-o" "fother")
   ("w" "windowlist")
   ("C-w" "windowlist")
   ("C-a" *audio-map*)
   ("C-b" "frame-undo")
   ("C-y" "show-clipboard-history")
   ("C-f" "frame-redo")
   ("C-x" *ctlx-map*)
   ("c" "term")
   ("C-c" "term")
   ("l" "lock-screen")
   ("n" "fnext")
   ("p" "fprev")
   ("z" "suspend")))

;; (setq *debug-level* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun default-startup ()
  (gnome-settings-daemon)
  (capslock-as-control)
  ;; (capslock-as-hyper)
  (sh "xsetroot" "-bg" "black")
  (init-mouse-pointer)
  (setup-touchpad)
  (redshift-on)
  (start-screen-saver)
  (clipboard-history:start-clipboard-manager)
  ;; (clipboard-history:stop-clipboard-manager)

  (sh "dropbox" "start")
  ;; (sh "compton" "-b")
  ;; (sh "xwrits" "+breakclock" "typetime=27" "breaktime=3")

  (sb-posix:putenv
   (format nil "EMACS_SERVER_FILE=~a/.emacs.d/server/server"
           (sb-posix:getenv "HOME")))
  (emacs)
  (firefox)
  (chromium)
  )

(swank)
(default-startup)
