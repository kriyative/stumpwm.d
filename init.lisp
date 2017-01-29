(setq *print-case* :downcase)

(in-package :stumpwm)

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

(defcommand term () ()
  "Launch or raise a terminal window"
  (run-or-raise "exec terminator" '(:class "terminator")))

(define-key *root-map* (kbd "c") "term")

(defcommand lock-screen () ()
  "Lock the screen, and power down the display"
  ;; (run-shell-command "exec xset dpms force off")
  (run-shell-command "exec xscreensaver-command -lock"))

(define-key *root-map* (kbd "l") "lock-screen")

(defun process-live-p (p)
  (and p (sb-ext:process-alive-p p)))

(defvar *screensaver-proc* nil)

(defcommand start-screen-saver () ()
  "Start screen saver"
  (unless (process-live-p *screensaver-proc*)
    (message "Starting screen saver...")
    (run-shell-command "exec xset +dpms")
    (setq *screensaver-proc*
          (run-shell-command "exec xscreensaver -no-splash"))))

(defcommand stop-screen-saver () ()
  "Stop screen saver"
  (run-shell-command "exec xset -dpms")
  (when (process-live-p *screensaver-proc*)
    (message "Stopping screen saver...")
    (run-shell-command "exec xscreensaver-command -exit")
    (setq *screensaver-proc* nil)))

(defcommand activate-screen-saver () ()
  "Activate screen saver"
  (run-shell-command "exec xscreensaver-command -activate"))

(defcommand suspend () ()
  "Suspend the system"
  (run-shell-command "exec systemctl suspend"))

(define-key *root-map* (kbd "z") "suspend")

(define-key *top-map* (kbd "XF86AudioLowerVolume") "amixer-Master-10- pulse")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "amixer-Master-10+ pulse")
(define-key *top-map* (kbd "S-XF86AudioLowerVolume") "amixer-Master-1- pulse")
(define-key *top-map* (kbd "S-XF86AudioRaiseVolume") "amixer-Master-1+ pulse")
(define-key *top-map* (kbd "XF86AudioMute") "amixer-Master-toggle pulse")

(define-key *top-map* (kbd "XF86AudioPlay") "exec emacsclient -e '(emms-pause)'")
(define-key *top-map* (kbd "XF86AudioStop") "exec emacsclient -e '(emms-stop)'")
(define-key *top-map* (kbd "XF86AudioPrev") "exec emacsclient -e '(emms-previous)'")
(define-key *top-map* (kbd "XF86AudioNext") "exec emacsclient -e '(emms-next)'")

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
    (sb-ext:process-kill *redshift-proc* sb-unix:sighup)
    (setq *redshift-proc* nil)
    (run-shell-command "exec redshift -o -x")))

(defcommand chromium () ()
  "Launch or raise chromium"
  (run-or-raise "exec chromium" '(:class "Chromium")))

(defcommand conkeror () ()
  "Launch or raise conkeror"
  (run-or-raise "exec conkeror" '(:class "Conkeror")))

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
  (run-shell-command "exec gnome-settings-daemon"))

(load-module "cpu")
(load-module "mem")
(load-module "wifi")

(load-module "battery-portable")
(in-package :battery-portable)
(export '(sysfs-field sysfs-field-exists? sysfs-int-field-or-nil))

(in-package :stumpwm)

(defun fmt-power-source (ml)
  (declare (ignore ml))
  (if (= 1
         (battery-portable:sysfs-int-field-or-nil "/sys/class/power_supply/AC/"
                                                  "online"))
      "AC"
      "BAT"))
(add-screen-mode-line-formatter #\W #'fmt-power-source)

;; startup
(gnome-settings-daemon)
(capslock-as-control)
(init-mouse-pointer)
(setup-touchpad)
(redshift-on)
(start-screen-saver)

(setq *window-format* "%m%n%s%10t"
      *time-modeline-string* "%a %b %e %k:%M"
      *screen-mode-line-format* (concat "[%3n] "
                                        "^B%v^b"
                                        "^>"
                                        " [^B%c^b,^B%M^b]"
                                        " [^B%W: %B^b]"
                                        " [^B%I^b]"
                                        " ^B%d^b")
      *mode-line-timeout* 10)

(defun turn-on-mode-line-timer ()
  (when (timer-p *mode-line-timer*)
    (cancel-timer *mode-line-timer*))
  (setf *mode-line-timer* (run-with-timer (- 10 (mod (get-decoded-time) 10))
                                          *mode-line-timeout*
                                          'update-all-mode-lines)))

(toggle-mode-line (current-screen) (current-head))

(emacs)
(conkeror)
(chromium)

;; (run-shell-command "gnome-screensaver-command -a")
