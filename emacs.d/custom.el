(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doc-view-continuous t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t)
 '(ledger-reports
   (quote
    (("equity" "ledger equity")
     ("bal" "ledger -f %(ledger-file) bal")
     ("reg" "ledger -f %(ledger-file) reg")
     ("payee" "ledger -f %(ledger-file) reg @%(payee)")
     ("account" "ledger -f %(ledger-file) reg %(account)"))))
 '(mu4e-view-show-images t)
 '(org-default-priority 53)
 '(org-highest-priority 49)
 '(org-log-done (quote time))
 '(org-lowest-priority 57)
 '(safe-local-variable-values (quote ((flycheck-gcc-language-standard . c++11))))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Liberation Mono" :foundry "unknown" :slant normal :weight normal :height 180 :width normal))))
 '(flyspell-duplicate ((t (:inherit warning :foreground "pale turquoise" :weight bold))))
 '(markdown-blockquote-face ((t (:background "gray19" :foreground "#ff8700" :slant italic))))
 '(markdown-inline-code-face ((t (:background "gray19" :foreground "#5fafd7"))))
 '(stripe-hl-line ((t (:background "dark violet" :foreground "unspecified-bg")))))
