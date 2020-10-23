(in-package :stumpwm)

;; (ql:quickload "archives/clx-truetype")
(asdf:load-system "clx-truetype")
(load-module "ttf-fonts")

(in-package :stumpwm)

(defun init-fonts ()
  (xft:cache-fonts)
  ;; (clx-truetype:get-font-families)
  ;; (clx-truetype:get-font-subfamilies "DejaVu Sans Mono")
  ;; (clx-truetype:get-font-subfamilies "Noto Mono")
  (let ((fonts '((:family "DejaVu Sans Mono" :subfamily "Book" :size 13)
                 (:family "Noto Mono" :subfamily "Regular" :size 13))))
    (set-font (mapcar (lambda (font)
                        (apply 'make-instance 'xft:font font))
                      fonts))))