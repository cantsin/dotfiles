;;; setup-edit -- Summary
;;; Commentary:
;;; Setup packages for quicker/easier editing.
;;; Code:
(require 'use-package)

(use-package wgrep
  :defer t
  :config (setq wgrep-enable-key "r"
                wgrep-auto-save-buffer t))

(defun easy-kill-config ()
  "Configure easy-kill."
  (define-key easy-kill-base-map (kbd "C-d") 'easy-kill-delete-region)
  (define-key easy-kill-base-map (kbd "DEL") 'easy-kill-delete-region)
  (add-to-list 'easy-kill-alist '(?^ backward-line-edge ""))
  (add-to-list 'easy-kill-alist '(?$ forward-line-edge ""))
  (add-to-list 'easy-kill-alist '(?b buffer ""))
  (add-to-list 'easy-kill-alist '(?< buffer-before-point ""))
  (add-to-list 'easy-kill-alist '(?> buffer-after-point ""))
  (add-to-list 'easy-kill-alist '(?f string-to-char-forward ""))
  (add-to-list 'easy-kill-alist '(?F string-up-to-char-forward ""))
  (add-to-list 'easy-kill-alist '(?t string-to-char-backward ""))
  (add-to-list 'easy-kill-alist '(?T string-up-to-char-backward "")))

;; replace kill-ring-save.
(use-package easy-kill
  :ensure t
  :defer t
  :config (easy-kill-config))

(use-package abbrev
  :defer t
  :diminish "")

(use-package yasnippet
  :defer t
  :config (progn
            (define-key yas-minor-mode-map (kbd "M-/") 'yas-expand)))

(provide 'setup-edit)
;;; setup-edit.el ends here
