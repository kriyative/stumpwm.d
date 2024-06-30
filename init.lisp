#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(setq *load-path* nil
      cl-user::*on-package-variance* nil
      *module-dir* (merge-pathnames ".stumpwm.d/modules" (user-homedir-pathname)))
(init-load-path *module-dir*)

(require "asdf")
(require "sb-posix")

;; (let ((contrib (merge-pathnames ".nix-profile/lib/sbcl/contrib/"
;;                                 (user-homedir-pathname))))
;;   (push contrib asdf:*central-registry*)
;;   (load (merge-pathnames "sb-rotate-byte.asd" contrib)))
(require "sb-rotate-byte")

(push (merge-pathnames "common-lisp/" (user-homedir-pathname))
      asdf:*central-registry*)

(load ".stumpwm.d/overrides.lisp")
(load ".stumpwm.d/core.lisp")
(load ".stumpwm.d/swank.lisp")
(load ".stumpwm.d/audio.lisp")
(load ".stumpwm.d/commands.lisp")
(load ".stumpwm.d/windmove.lisp")
(load ".stumpwm.d/modules.lisp")
(load ".stumpwm.d/notify.lisp")
;; (load ".stumpwm.d/fonts.lisp")
(load ".stumpwm.d/ctrlx.lisp")

(in-package :stumpwm)

(add-hook *quit-hook* 'redshift-off)

(setq *title-remaps*
      '(("Gnome-terminal" . "term")
        ("[Cc]onsole" . "cons")
        ("ubuntu-terminal-app" . "term")
        ("urxvt" . "term")
        ("[Cc]hromium-browser" . "chromi")
        ("[Cc]hromium" . "chromi")
        ("[Gg]oogle-chrome" . "chrome")
        ("[Cc]onkeror" . "conk")
        ("[Ff]irefox.*" . "fox")
        ("9emacs" . rk-customize-9emacs)
        ("Emacs" . "emacs")
        ("keepassxc" . "kpass")
        ("KeePassXC" . "kpass")
        ("Volume Control" . "mixer")))

(add-hook *new-window-hook* 'new-window-customizations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *default-remapped-keys*
  '(("C-n"   . "Down")
    ("C-p"   . "Up")))

(defun concat-re (&rest exprs)
  (format nil "(~{~A~^|~})" exprs))

(define-remapped-keys
    `((,(concat-re
         ".*[Cc]hrom"
         "[Ff]irefox"
         "[Ee]vince"
         "keepassxc|KeePassXC"
         "libreoffice"
         "Signal")
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
        ("C-d"   . "Delete")
        ("M-d"   . ("S-C-Right" "Delete")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun safe-sh (&rest args)
  (handler-case
      (apply 'sh args)
    (condition (c)
      (message "^B^1sh error: ~a" c))))

(defun start-apps ()
  ;; (redshift-on)
  (start-screen-saver)
  (clipboard-history:start-clipboard-manager)
  ;; (clipboard-history:stop-clipboard-manager)

  (safe-sh "xsettingsd")
  (safe-sh "fjdropbox" "start")
  (safe-sh "syncthing" "-no-browser" "-logfile=var/log/syncthing.log")
  (safe-sh "run_keybase" "-g")
  ;; this fixes screen tearing (at least on debian buster)
  ;; (sh "compton" "-b")
  ;; (sh "xwrits" "+breakclock" "typetime=27" "breaktime=3")
  ;; (emacs)
  ;; (chrome)
  ;; (chromium)
  ;; (firefox)
  )

(defun init-vars ()
  (pushnew '(#\q time-dow-shortname-2ch)
           *time-format-string-alist*)
  (setq *window-format* "%m%n%s%10t"
        *window-menulist-format* "%m%n%s %c (%20t)"
        ;; *time-modeline-string* "%a %b %e %H:%M"
        *time-modeline-string* "%q %m-%e %H:%M"
        *screen-mode-line-format* (concat "%3n |"
                                          "%v"
					  ;; "^B%v^b"
                                          "^>"
                                          "%T"
                                          (if (ignore-errors (truename "Mail"))
                                              "| %U"
                                              "")
                                          " | %a"
                                          " | %C| %M"
                                          " | %W: %B"
                                          " | %I"
                                          " | %d")
        cpu::*cpu-modeline-fmt* "%c"
        mem::*mem-modeline-fmt* "MEM: %a"
        *hidden-window-color* "^7*"
        *mode-line-timeout* 10
        *mode-line-position* :bottom
        *mode-line-foreground-color* "white"
        *mode-line-brightness* 0.75
        *message-window-gravity* :center
        *message-window-padding* 5
        *input-window-gravity* :center
        *input-completion-style* (make-input-completion-style-cyclic)
        *window-border-style* :thin
        *normal-border-width* 1
        *timeout-wait* 5)
  (sync-all-frame-windows (current-group))
  (setenv
   `(("EMACS_SERVER_FILE" ,(format nil
                                   "~a/.emacs.d/server/server"
                                   (sb-posix:getenv "HOME")))
     ;; workaround for keyboard not working in firejail'ed Chrome (or
     ;; other gtk apps)
     ;; https://github.com/netblue30/firejail/issues/1810#issuecomment-382586391
     ("GTK_IM_MODULE" "xim"))))

(defun init-keybindings ()
  (bind-keys
   *top-map*
   '(("s-!" "exec")
     ("s-&" "exec")
     ("s-+" "balance-frames")
     ("s-=" "balance-frames")
     ("s--" "remove")
     ("s-:" "eval")
     ("s-;" "colon")
     ;; ("s-a" "ratclick 1")
     ("s-b" "frame-undo")
     ("s-f" "frame-redo")
     ;; ("s-s" "ratclick 2")
     ("s-Q" "only")
     ("s-`" "only")
     ("s-S" "hsplit")
     ("s-h" "hsplit")
     ("s-d" "remove")
     ("s-\\" "hsplit")
     ("s-e" "expose")
     ("s-k" "delete")
     ("s-l" "lock-screen")
     ("s-m" "toggle-mic")
     ("s-n" "fnext")
     ("M-s-n" "gnext")
     ("s-TAB" "fnext")
     ("s-ISO_Left_Tab" "fprev")
     ("s-o" "fother")
     ("s-p" "fprev")
     ("M-s-p" "gprev")
     ("s-q" "send-raw-key")
     ("s-r" "remove")
     ("s-s" "vsplit")
     ("s-t" "pull-hidden-other")
     ("s-u" "frame-undo")
     ("s-w" "windowlist")
     ("s-x" "colon")
     ("s-y" "show-clipboard-history")
     ("s-SPC" "audio-pause")
     ("s-F6" "resize-3way")
     ("s-P" "pull")
     ("XF86AudioLowerVolume" "amixer-Master-10-")
     ("S-XF86AudioLowerVolume" "amixer-Master-1-")
     ("XF86AudioRaiseVolume" "amixer-Master-10+")
     ("S-XF86AudioRaiseVolume" "amixer-Master-1+")
     ("XF86AudioMute" "amixer-Master-toggle")
     ("XF86AudioMicMute" "toggle-mic")
     ("XF86AudioPlay" "audio-pause")
     ("XF86AudioStop" "audio-stop")
     ("XF86AudioPrev" "audio-previous")
     ("XF86AudioNext" "audio-next")
     ("XF86MonBrightnessUp" "change-brightness 10")
     ("S-XF86MonBrightnessUp" "change-brightness 1")
     ("XF86MonBrightnessDown" "change-brightness -10")
     ("S-XF86MonBrightnessDown" "change-brightness -1")
     ("s-F9" "change-brightness -10")
     ("s-S-F9" "change-brightness -1")
     ("s-F10" "change-brightness 10")
     ("s-S-F10" "change-brightness 1")

     ("s-Left" "ratrelwarp -10 0")
     ("s-Right" "ratrelwarp 10 0")
     ("s-Up" "ratrelwarp 0 -10")
     ("s-Down" "ratrelwarp 0 10")
     ("s-Pause" "audio-pause")
     ("M-TAB" "cycle-windowlist")
     ("Print" "scrot-screenshot-screen")
     ("C-Print" "scrot-screenshot-screen-select")
     ("C-s-n" "pull-hidden-next")
     ("C-s-p" "pull-hidden-previous")
     ("s-RET" "switch-display")))
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
               (format nil "C-s-~d" i)
               (format nil "swap-or-pull ~d" i))))
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
     ("C-x" *ctlrx-map*)
     ("c" "term")
     ("C-c" "term")
     ("l" "lock-screen")
     ("n" "fnext")
     ("p" "fprev")
     ("z" "suspend")
     ("M-m" "show-message-window-messages")
     ("M" "ytv-toggle-mute")
     ("P" "pull")))
  (bind-keys
   *root-map*
   (loop
      for i from 0 to 9
      collect (list
               (format nil "C-~d" i)
               (format nil "swap-or-pull ~d" i)))))

(defun start-stumpwm ()
  (run-swank)
  (init-vars)
  (set-normal-gravity :top)
  (set-transient-gravity :center)
  (set-fg-color "white")
  (set-bg-color "black")
  (set-border-color "red")
  (set-win-bg-color "white")
  (set-focus-color "green")
  (set-msg-border-width 1)
  ;; (init-fonts)
  (toggle-current-mode-line)
  ;; set window-class property of mode-lines - used by compton
  (dolist (m *mode-lines*)
    (xlib:set-wm-class (mode-line-window m)
                       "StumpwmModeline"
                       "stumpwm-modeline"))
  (init-keybindings)
  (capslock-as-control)
  (f24-as-hyper)
  ;; (capslock-as-hyper)
  (set-root-window-background-color 0)
  (init-mouse-pointer)
  (setup-touchpad)
  (setup-rollermouse)
  (%grename "A" (current-group))
  (start-apps)
  #+nil
  (let ((start-appsp (getenv "STUMPWM_START_APPS")))
    (when (and start-appsp (equal "t" start-appsp))
      (start-apps))))

(let ((*sh-echo-console* t))
  (start-stumpwm))
