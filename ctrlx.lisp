(in-package :stumpwm)

(defvar *ctlrx-map* (make-sparse-keymap))

(bind-keys
 *ctlrx-map*
 '(("0" "remove")
   ("1" "only")
   ("2" "vsplit")
   ("3" "hsplit")
   ("6" "resize-3way")
   ("7" "resize-33%-width")
   ("8" "resize-75%-width")
   ("+" "balance-frames")
   ("y" "show-clipboard-history")
   ("C-y" "show-clipboard-history")
   ("p" "fprev")
   ("n" "fnext")))
