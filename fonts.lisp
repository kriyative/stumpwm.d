(in-package :stumpwm)

(ql:quickload "clx-truetype")
(load-module "ttf-fonts")

(in-package :stumpwm)

(defun init-fonts ()
  (xft:cache-fonts)
  ;; (clx-truetype:get-font-families)
  ;; (clx-truetype:get-font-subfamilies "DejaVu Sans Mono")
  ;; (clx-truetype:get-font-subfamilies "Noto Sans")
  (let ((fonts '((:family "DejaVu Sans Mono" :subfamily "Book" :size 11)
                 #+nil (:family "Noto Sans Devanagari"
                        :subfamily "Regular" :size 12))))
    (set-font (mapcar (lambda (font)
                        (apply 'make-instance 'xft:font font))
                      fonts))))
