;;; setup-theme -- Summary
;;; Commentary:
;;; Setup theme modifications.
;;; Code:
(require 'use-package)

(when (display-graphic-p)
  (use-package ocodo-svg-modelines
    :config (ocodo-svg-modelines-init))

  (use-package ocodo-mesh-retro-aqua-smt
    :config (smt/set-theme 'ocodo-mesh-retro-aqua-smt)))

(defun ocodo-mesh-retro-aqua-smt-background (theme)
  "Override THEME."
  (let ((ocodo-twisted-stops '(("0%" "#FFFFFF" "0.1")
                               ("30%" "#FFFFFF" "0.1")
                               ("70%" "#000000" "0.1")
                               ("100%" "#000000" "0.1"))))
    (ocodo-smt-edge-image theme ocodo-mesh-retro-aqua-graphic)))

(defun ocodo-mesh-retro-aqua-smt-overlay (theme)
  "Override THEME."
  (let ((ocodo-overlay-stops '(("0%" "#000000" "0.0")
                               ("0%" "#000000" "0.0")
                               ("80%" "#FFFFFF" "0.1")
                               ("100%" "#000000" "0.3"))))
    (ocodo-smt-overlay theme)))

(defun ocodo-mesh-retro-aqua-buffer-name-style (widget)
  "Override WIDGET."
  (list :font-weight "normal"
        :font-size "12pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#FFFFFF" "#666666")))

(defun ocodo-mesh-retro-aqua-major-mode-style (widget)
  "Override WIDGET."
  (list :font-weight "normal"
        :font-size "12pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#AAAAAA" "#666666")))

(defun ocodo-mesh-retro-aqua-info-style (widget)
  "Override WIDGET."
  (list :font-weight "normal"
        :font-size "10pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#999999" "#555555")))

(defun ocodo-mesh-retro-aqua-position-info-style (widget)
  "Override WIDGET."
  (list :font-weight "normal"
        :font-size "12pt"
        :fill (if (smt/window-active-p) "#DDDDDD" "#999999")))

(defun ocodo-mesh-retro-aqua-dirty-style (widget)
  "Override WIDGET."
  (list :font-weight "normal"
        :font-size "11pt"
        :font-family "sans-serif"
        :fill (if (and (or buffer-file-name buffer-offer-save) (buffer-modified-p))
                  ;; Dirty
                  (if (smt/window-active-p) "#FF6060" "#763030")
                ;; Untouched
                (if (smt/window-active-p) "#1F4F25" "#143519"))))

(defun ocodo-mesh-retro-aqua-minor-mode-style (widget)
  "Override WIDGET."
  (list :font-weight "normal"
        :font-size "10pt"
        :fill (if (smt/window-active-p) "#FFFFFF" "#666666")))

(defun ocodo-mesh-retro-aqua-version-control-style (widget)
  "Override WIDGET."
  (list :font-weight "normal"
        :font-size "10pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#60ACB1" "#365E63")))

(use-package moe-theme
  :ensure t
  :config (progn
            (moe-dark)
            (moe-theme-set-color 'orange)))

;; (use-package powerline
;;   :ensure t
;;   :init (powerline-default-theme))

(provide 'setup-theme)
;;; setup-theme.el ends here
