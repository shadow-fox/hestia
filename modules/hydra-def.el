;;; hydra-def.el -- Summary

;;; Commentary:

;;; Code:

(use-package hydra
  :straight t)

;; hydra-frame-window is designed from ace-window (C-x f) and
;; matches aw-dispatch-alist with a few extra
(defhydra hydra-frame-window (:color red :hint nil)
  "
^Delete^                       ^Frame resize^             ^Window^                Window Size^^^^^^   ^Text^                         (__)
_0_: delete-frame              _g_: resize-frame-right    _t_: toggle               ^ ^ _k_ ^ ^        _K_                           (oo)
_1_: delete-other-frames       _H_: resize-frame-left     _e_: ace-swap-win         _h_ ^+^ _l_        ^+^                     /------\\/
_2_: make-frame                _F_: fullscreen            ^ ^                       ^ ^ _j_ ^ ^        _J_                    / |    ||
_d_: kill-and-delete-frame     _n_: new-frame-right       _w_: ace-delete-window    _b_alance^^^^      ^ ^                   *  /\\---/\\  ~~  C-x f ;
"
  ("0" delete-frame :exit t)
  ("1" delete-other-frames :exit t)
  ("2" make-frame  :exit t)
  ("b" balance-windows)
  ("d" kill-and-delete-frame :exit t)
  ("e" ace-swap-window)
  ("F" toggle-frame-fullscreen)   ;; is <f11>
  ("g" resize-frame-right :exit t)
  ("H" resize-frame-left :exit t)  ;; aw-dispatch-alist uses h, I rebind here so hjkl can be used for size
  ("n" new-frame-right :exit t)
  ;; ("r" reverse-windows)
  ("t" toggle-window-spilt)
  ("w" ace-delete-window :exit t)
  ("x" delete-frame :exit t)
  ("K" text-scale-decrease)
  ("J" text-scale-increase)
  ("h" shrink-window-horizontally)
  ("k" shrink-window)
  ("j" enlarge-window)
  ("l" enlarge-window-horizontally))


(provide 'hydra-def)
;;; hydra-def.el ends here
