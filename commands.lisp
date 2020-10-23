(in-package :stumpwm)

(defcommand term () ()
  "Launch or raise a terminal window"
  (run-or-pull "exec urxvt" '(:class "URxvt")))

(defcommand emacs () ()
  "Start emacs unless it is already running, in which case focus it."
  (sh "emacs" "-f" "rk-start-emacs-6"))

(defcommand 9emacs () ()
  "Start a 9emacs profile."
  (sh "emacs" "-title" "9emacs" "-f" "rk-start-9emacs"))

(defun rk-customize-9emacs (win1)
  (let* ((nt 9)
         (nf (window-number win1))
         (win (find-if #'(lambda (win)
                           (= (window-number win) nt))
                       (group-windows (current-group)))))
    ;; Is it already taken?
    (when win
      (setf (window-number win) nf))
    (setf (window-number win1) nt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcommand show-message-window-messages () ()
  "Display a list of message-window messages"
  (let* ((*record-last-msg-override* t)
         (screen (current-screen))
         (sel (select-from-menu screen
                                (screen-last-msg screen)
                                nil)))
    (when sel
      (echo-string-list screen sel)
      (set-x-selection (format nil "~{~a~^~%~}" sel) :clipboard))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resize-frame-width-pct (group frame pct)
  (let* ((swidth (screen-width (current-screen)))
         (twidth (round (* pct swidth))))
    (resize-frame group
                  frame
                  (- twidth (frame-width frame))
                  :width)))

(defun resize-width-pct (pct)
  (let ((group (current-group)))
    (resize-frame-width-pct group
                            (tile-group-current-frame group)
                            pct)))

(defcommand resize-25%-width () ()
  "Resize width of current frame to 25% of screen-width"
  (resize-width-pct 0.25))

(defcommand resize-33%-width () ()
  "Resize width of current frame to 33% of screen-width"
  (resize-width-pct 0.33))

(defcommand resize-66%-width () ()
  "Resize width of current frame to 66% of screen-width"
  (resize-width-pct 0.66))

(defcommand resize-75%-width () ()
  "Resize width of current frame to 75% of screen-width"
  (resize-width-pct 0.75))

(defcommand resize-3way () ()
  "Resize 3 frames as 25% | 50% | 25%"
  (let* ((group (current-group))
         (frames (group-frames group)))
    (resize-frame-width-pct group (first frames) 0.25)
    (resize-frame-width-pct group (first (last frames)) 0.25)))

(defcommand (fprev tile-group) () ()
  "Cycle through the frame tree to the next frame."
  (focus-prev-frame (current-group)))

(defun find-window-by-property (pval &key property test)
  (find-if (lambda (win)
             (funcall (or test 'cl-ppcre:scan)
                      pval
                      (funcall (or property 'window-name) win)))
           (group-windows (current-group))))

(defcommand (swap-or-pull tile-group)
    (n &optional (group (current-group)))
    ((:number "Number: "))
  "Swap current window with another in current group"
  (let* ((cwin (group-current-window group))
         (cframe (if cwin
                     (window-frame cwin)
                   (tile-group-current-frame group)))
         (owin (find-window-by-property n
                                        :property 'window-number
                                        :test '=))
         (oframe (when owin (window-frame owin))))
    (when (and oframe
               cwin
               (not (equal oframe cframe))
               (equal (frame-window oframe) owin))
      (pull-window cwin oframe))
    (if owin
        (pull-window owin cframe)
      (message "No such window"))))

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
  (stop-screen-saver*))

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

;; (reset-defafter 'activate-fullscreen)
;; (reset-defafter 'deactivate-fullscreen)

(defun toggle-screensaver-on-fullscreen (window state)
  (declare (ignore window))
  (cond
    ((eq :fullscreen state) (stop-screen-saver*))
    ((eq :normal state) (start-screen-saver*))))

(add-hook *fullscreen-hook* 'toggle-screensaver-on-fullscreen)

(defun toggle-current-mode-line (&rest args)
  (declare (ignore args))
  (toggle-mode-line (current-screen) (current-head)))

;; (toggle-current-mode-line)

;; (add-hook *fullscreen-hook* 'toggle-current-mode-line)
;; (remove-hook *fullscreen-hook* 'toggle-current-mode-line)

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
    (sh "xbacklight" "-set" (format nil "~d" next))
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

(defcommand chrome () ()
  "Launch Google Chrome"
  (sh "firejail" "google-chrome"))

(defcommand chromium () ()
  "Launch Chromium"
  (let ((browser (or (string-not-empty
                      (string-trim '(#\space #\newline)
                                   (sh< "which" "chromium")))
                     (string-not-empty
                      (string-trim '(#\space #\newline)
                                   (sh< "which" "chromium-browser"))))))
    (if browser
        (sh browser)
      (message "No candidate found for chromium browser"))))

(defcommand firefox () ()
  "Launch or raise firefox"
  (run-or-raise "exec firefox" '(:class "Firefox")))

(defcommand pfirefox () ()
  "Launch or raise private mode firefox"
  (run-or-raise
   "firefox -private -safe-mode -no-remote"
   '(:class "Firefox")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  "Take a screenshot of whole screen with gnome-screenshot"
  (sh "gnome-screenshot"))

(defcommand gnome-screenshot-screen-select () ()
  "Take a screenshot of selected portion of screen with gnome-screenshot"
  (sh "gnome-screenshot" "-a"))

(defcommand scrot-screenshot-screen () ()
  "Take a screenshot of whole screen with scrot"
  (sh "scrot" "-e" "mv $f ~/Pictures/"))

(defcommand scrot-screenshot-screen-select () ()
  "Take a screenshot of selected portion of screen with scrot"
  (sh "scrot" "-s" "-e" "mv $f ~/Pictures/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun turn-on-mode-line-timer ()
  (when (timer-p *mode-line-timer*)
    (cancel-timer *mode-line-timer*))
  (setf *mode-line-timer*
        (run-with-timer (- 10 (mod (get-decoded-time) 10))
                        *mode-line-timeout*
                        'update-all-mode-lines)))


(defvar *title-remaps* nil)

(defun remap-title (s)
  (cdr (assoc s *title-remaps* :test 'string-match)))

(defun new-window-customizations (win)
  (let ((title-max-len 8)
        (title (or (remap-title (window-title win))
                   (remap-title (window-class win)))))
    (if (and title (or (symbolp title) (functionp title)))
        (funcall title win)
      (setf (window-user-title win)
            (or title
                (let ((title (window-title win)))
                  (cond
                    ((and title (< title-max-len (length title)))
                     (scrunch (string-downcase title) :maxlen title-max-len))

                    (title title)

                    (t ""))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
