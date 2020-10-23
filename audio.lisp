(in-package :stumpwm)

(load-module "amixer")
(in-package :amixer)

(defmacro defvolcontrol (name channel valence)
  `(defcommand ,name (&optional device) ((:string))
     (let ((stumpwm::*record-last-msg-override* t)
           (stumpwm::*timeout-wait* 1))
       (volcontrol device ,channel ,valence))))

(defvolcontrol amixer-Master-10- "Master" "10%-")
(defvolcontrol amixer-Master-10+ "Master" "10%+")
(defvolcontrol amixer-Mic-toggle "Capture" "toggle")

(setq *default-device* "pulse")

(defstruct audio-info
  name
  state
  level)

(defun get-audio-info (control)
  (or (ignore-errors
       (let ((output (stumpwm::sh< "amixer"
                                   "-D" *default-device*
                                   "sget" control)))
         (multiple-value-bind (str matches)
             (cl-ppcre:scan-to-strings ".*\\[([0-9]+)%\\].*\\[(on|off)\\]\\n"
                                       output)
           (declare (ignore str))
           (make-audio-info :name control
                            :state (aref matches 1)
                            :level (or (ignore-errors
                                        (read-from-string (aref matches 0)))
                                       0)))))
      "Err"))

(defvar *fmt-audio-state* nil)
(defun update-fmt-audio-state ()
  (let ((*default-device* "pulse")
        (master (get-audio-info "Master"))
        (mic (get-audio-info "Capture")))
    (setq *fmt-audio-state*
          (format nil
                  "~A~A"
                  (if (equal "off" (audio-info-state master))
                      "^B^1*SPK:-^*^b"
                      (format nil "SPK:~A%" (audio-info-level master)))
                  (if (equal "off" (audio-info-state mic))
                      " ^B^1*MIC:-^*^b"
                      "")))))

;; (update-fmt-audio-state)

(defun fmt-audio-state-cached (ml)
  (declare (ignore ml))
  (unless *fmt-audio-state*
    (update-fmt-audio-state))
  *fmt-audio-state*)

;; (fmt-audio-state-cached nil)

(stumpwm::add-screen-mode-line-formatter #\a 'fmt-audio-state-cached)

(in-package :stumpwm)

(defvar audio-profile-choices nil)
(setq audio-profile-choices
      '(("internal" "internal")
        ;; ("bt-a2dp" "bt-a2dp")
        ;; ("bt-headset" "bt-headset")
        ("hyperx" "hyperx")
        ("display-port" "dp")))

(defcommand set-audio-profile () ()
  "Prompt with the list of audio profiles"
  (let ((profile (second
                  (select-from-menu (current-screen)
                                    audio-profile-choices
                                    nil))))
    (when profile
      (sh "set-audio-profile" profile))))

;; (define-key *root-map* (kbd "C-a") "set-audio-profile")

(defcommand open-audio-mixer () ()
  "Open the OS native audio mixer tool"
  (run-or-raise "exec pavucontrol" '(:class "Pavucontrol")))


(defcommand toggle-mic () ()
  "Toggle on/off the active microphone"
  (amixer::amixer-mic-toggle)
  (amixer::update-fmt-audio-state)
  (update-mode-lines (current-screen)))

(defvar *audio-map* (make-sparse-keymap))
(bind-keys
 *audio-map*
 '(("n" "audio-next")
   ("p" "audio-previous")
   ("d" "set-audio-profile")
   ("C-d" "set-audio-profile")
   ("SPC" "audio-pause")
   ("m" "toggle-mic")
   ("v" "open-audio-mixer")
   ("C-v" "open-audio-mixer")))
