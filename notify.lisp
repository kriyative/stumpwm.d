;;;;;;;;;;;;;;;; notify customizations ;;;;;;;;;;;;;;;;

;; (ql:update-all-dists)
;; (ql: "puri")
(in-package :stumpwm)

(ql:quickload "xml-emitter")
(ql:quickload "dbus")
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
