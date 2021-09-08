(in-package :stumpwm)

(load-module "clipboard-history")
(load-module "cpu")
(load-module "mem")
(load-module "wifi")

;;;;;;;;;;;;;;;; notify customizations ;;;;;;;;;;;;;;;;

(ql:quickload "notify")
(load-module "notify")
(notify:notify-server-toggle)

(defvar *rk--show-notifications-p* t)

(defun rk--show-notification (app icon summary body)
  (when *rk--show-notifications-p*
    (let ((stumpwm::*message-window-gravity* :bottom-right))
      (notify::show-notification app
                                 icon
                                 (format nil
                                         "[~A] ~A"
                                         (time-format *time-modeline-string*)
                                         summary)
                                 body))))

(setq notify::*notify-server-title-color* "^3"
      notify::*notify-server-body-color* "^B^7"
      notify::*notification-received-hook* '(stumpwm::rk--show-notification))

(defcommand rk-mute-notifications () ()
  "Do not display notifications popups"
  (setq *rk--show-notifications-p* nil))

(defcommand rk-show-notifications () ()
  "Display notifications popups"
  (setq *rk--show-notifications-p* t))

(in-package :stumpwm)

;;;;;;;;;;;;;;;; mu4e-mail-biff ;;;;;;;;;;;;;;;;

(defvar *mu-cmd* "./.emacs.d/el-get/mu4e/mu/mu")

(defvar *fmt-mail-biff-unread* nil)

(defun fmt-mail-biff-poll-unread ()
  (let* ((out (ignore-errors
               (sh< *mu-cmd*
                    "find"
                    "flag:unread"
                    "-f" "m	f	s"
                    "-u")))
         (msgs (unless (string-equal "" out)
                 (->> (split-string out)
                      (remove-if (lambda (msg)
                                   (cl-ppcre:scan "^error: " msg)))
                      (mapcar (lambda (msg)
                                (split-string msg "	")))))))
    (setq *fmt-mail-biff-unread*
          (remove-if (lambda (msg)
                       (cl-ppcre:scan "All Mail" (first msg)))
                     msgs))
    *fmt-mail-biff-unread*))

;; (fmt-mail-biff-poll-unread)

(defun fmt-mail-biff (mb)
  (declare (ignore mb))
  (let* ((msgs (fmt-mail-biff-poll-unread))
         (cnt (length msgs)))
    (format nil
            (if (positive-integer-p cnt)
                "^B^3*~A:~D^*^b"
                "~A:~D")
            "M"
            (length msgs))))

;; (fmt-mail-biff 0)

(defcached fmt-mail-biff-cached () (ml)
  (fmt-mail-biff ml))

(stumpwm:add-screen-mode-line-formatter #\U 'fmt-mail-biff-cached)

(defcommand rk-show-biff-unread () ()
  "Show list of unread messages"
  (if (zerop (length *fmt-mail-biff-unread*))
      (message "No unread messages")
      (select-from-menu (current-screen)
                        (mapcar (lambda (msg)
                                  (destructuring-bind (mbox sender subj)
                                      msg
                                    (format nil "~10a ~20,a ~a"
                                            (scrunch mbox :maxlen 10)
                                            (scrunch sender :maxlen 25)
                                            (scrunch subj :maxlen 35))))
                                *fmt-mail-biff-unread*)
                        nil))
  nil)

;;;;;;;;;;;;;;;; battery-portable ;;;;;;;;;;;;;;;;

(in-package :stumpwm)

(load-module "battery-portable")
(in-package :battery-portable)

(defun fmt-power-source (ml)
  (declare (ignore ml))
  (if (= 1
         (sysfs-int-field
          "/sys/class/power_supply/AC/"
          "online"))
      "AC"
      "BAT"))

(stumpwm::defcached fmt-power-source-cached () (ml)
  (fmt-power-source ml))

(defun fmt-bat-alt (ml)
  (if (equal "BAT" (fmt-power-source ml))
      (fmt-bat ml)
      (flet ((battery-levels (battery)
               (let ((path (path-of battery)))
                 (if (string= (sysfs-field path "present")
                              "0")
                     0
                     (let* ((curr (or (sysfs-int-field path "energy_now")
                                      ;; energy_* seems not to be there on
                                      ;; some boxes. Strange...
                                      (sysfs-int-field path "charge_now")
                                      (return-from fmt-bat-alt :unknown)))
                            (full (or (sysfs-int-field path "energy_full")
                                      (sysfs-int-field path "charge_full")
                                      (return-from fmt-bat-alt :unknown)))
                            (percent (* 100 (/ curr full))))
                       (round percent))))))
        (let ((pcts (mapcar #'battery-levels
                            (all-batteries
                             (or (preferred-battery-method)
                                 (return-from fmt-bat-alt
                                   "(not implemented)"))))))
          (format nil
                  "~D%"
                  (round
                   (/ (reduce '+ pcts :initial-value 0)
                      (length pcts))))))))

;; (fmt-power-source nil)
;; (fmt-bat-alt nil)

(stumpwm::defcached fmt-bat-alt-cached () (ml)
  (fmt-bat-alt ml))

(stumpwm::add-screen-mode-line-formatter #\B 'fmt-bat-alt-cached)
(stumpwm::add-screen-mode-line-formatter #\W 'fmt-power-source-cached)
