;;; setup-projectile -- Summary
;;; Commentary:
;;; Setup projectile.
;;; Code:
(require 'use-package)

(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :diminish " Proj")

(setq hydra-is-helpful t)
(setq hydra-lv t)
(setq lv-use-separator t)
(setq projectile-switch-project-action 'projectile-find-file-dwim)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(defhydra hydra-projectile (:idle 0.0
                            :color teal
                            :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
  ("a"   projectile-ag)
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  ("s-f" projectile-find-file)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("g"   ggtags-update-tags)
  ("s-g" ggtags-update-tags)
  ("i"   projectile-ibuffer)
  ("K"   projectile-kill-buffers)
  ("s-k" projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("o"   projectile-multi-occur)
  ("s-p" projectile-switch-project "switch project")
  ("p"   projectile-switch-project)
  ("s"   projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("`"   hydra-projectile-other-window/body "other window")
  ("q"   nil "cancel" :color blue))

(add-to-list 'projectile-other-file-alist '("js" "hbs"))
(add-to-list 'projectile-other-file-alist '("hbs" "js"))

(defun projectile-select-files (project-files &optional arg)
  "Select a list of files based on filename at point.

With a prefix ARG invalidates the cache first."
  (projectile-maybe-invalidate-cache arg)
  (let* ((file (if (region-active-p)
                   (buffer-substring (region-beginning) (region-end))
                 (or (thing-at-point 'filename) "")))
         ;; don't bother with relative paths in js2-mode
         (file (if (and (eq major-mode 'js2-mode) (string-match "\\.\\./" file))
                   (substring file 3) file))
         (file (if (and (eq major-mode 'js2-mode) (string-match "\\./" file))
                   (substring file 2) file))
         (file (if (string-match "\\.?\\./" file)
                   (file-relative-name (file-truename file) (projectile-project-root))
                 file))
         ;; if nothing is found in js2-mode, then append a 'lib/' after the first slash
         (file (if (and (eq major-mode 'js2-mode)
                        (string-match "/" file)
                        (eq nil (-filter (lambda (project-file)
                                           (string-match file project-file))
                                         project-files)))
                   (let* ((x (split-string file "/"))
                          (fst (car x))
                          (lst (cadr x)))
                     (mapconcat 'identity `(,fst "lib" ,lst) "/"))
                 file))
         (files (if file
                    (-filter (lambda (project-file)
                               (string-match file project-file))
                             project-files)
                  nil)))
    files))

(provide 'setup-projectile)
;;; setup-projectile.el ends here
