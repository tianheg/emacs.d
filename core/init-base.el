;;; init-base.el --- Base configuration -*- lexical-binding: t -*-

;;; Code:

;; --- General Settings ---
(fset 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)
(setq use-dialog-box nil)
(setq inhibit-startup-echo-area-message t
      inhibit-startup-message t
      indicate-empty-lines t)

;; --- File Handling ---
(setq auto-save-default t
      make-backup-files t
      create-lockfiles nil
      confirm-kill-processes nil
      load-prefer-newer t)

(save-place-mode t)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; --- Editing ---
(setq-default indent-tabs-mode nil
              tab-width 2)
(electric-indent-mode 1)
(electric-pair-mode 1)
(column-number-mode 1)

;; --- Completion ---
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; --- Performance ---
(setq inhibit-compacting-font-caches t)

;; --- Advice ---
(defun my/align-regexp-no-tabs (orig-fun &rest args)
  "Ensure `align-regexp' doesn't use tabs."
  (let ((indent-tabs-mode nil))
    (apply orig-fun args)))
(advice-add 'align-regexp :around #'my/align-regexp-no-tabs)

;; --- Packages ---
(use-package whitespace
  :ensure t
  :demand t
  :init
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package exec-path-from-shell
  :ensure t
  :when (eq system-type 'darwin)
  :hook (after-init . exec-path-from-shell-initialize))

(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'init-base)
;;; init-base.el ends here
