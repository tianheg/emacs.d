;; -*- coding: utf-8; lexical-binding: t; -*-

;; Enable line numbers
(global-display-line-numbers-mode)
; (add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Enable syntax highlighting
(global-font-lock-mode t)

;; Enable automatic indentation
;(electric-indent-mode t)
(setq-default standard-indent 2)
(setq-default indent-tabs-mode nil)
(add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))

(defun show-current-time ()
  "Show current time."
  (interactive)
  (message (current-time-string)))

;; Automatically pair parentheses
(electric-pair-mode t)

;; Enable backup files
(setq make-backup-files t)

;; Enable auto-save files
(setq auto-save-default t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; only works at Emacs GUI
(let ((mono-spaced-font "Monospace")
      (proportionately-spaced-font "Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 100)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

;; Disable startup message
(setq inhibit-startup-message t)

;; http://yummymelon.com/devnull/surprise-and-emacs-defaults.html

(setq sentence-end-double-space nil)
(setq dired-auto-revert-buffer t)

;; Add welcome msg
(setq initial-scratch-message "archie, Emacs loves you!")

;; package
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
; (package-initialize) not require

(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
       (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-repository-branch "develop")
(straight-use-package 'use-package)

(use-package delsel
  :ensure nil ; no need to install it as it is built-in
  :hook (after-init . delete-selection-mode))

;; theme
(use-package zenburn-theme
  :ensure t
  :config
  ;; Load the theme after `use-package` installs it
  (load-theme 'zenburn t))

;; archlinux need install otf-monaspace-nerd,nerd-fonts-fontconfig pkg
;; type M-x and then call the command nerd-icons-install-fonts
(use-package nerd-icons
  :ensure t)
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; Vertico for completion
(use-package vertico
  :ensure t
  :init (vertico-mode t)
  :config
  (with-eval-after-load 'vertico
    (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
    (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-word)
    (define-key vertico-map (kbd "M-d") #'vertico-directory-delete-char))
  :hook (after-init . vertico-mode))
(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil))
(use-package savehist
  :ensure nil ; it is built-in
  :hook (after-init . savehist-mode))

;; code completion
(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;; file manager
(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))
(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))
(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;; LSP Support with Eglot
;(use-package eglot
;  :hook (prog-mode . eglot-ensure)
;  :init
;  (defalias 'start-lsp-server #'eglot))

;; Inline static analysis with Flymake
;(add-hook 'prog-mode-hook #'flymake-mode)

;; Miscellaneous options
;;; This sets the default major mode for new buffers.
;;; The lambda function provided will be used to guess the major mode based on the file name.
;;; If buffer-file-name is nil (which it will be for new, unsaved buffers), it sets buffer-file-name to the buffer name and then calls set-auto-mode, which sets the major mode based on the file name.
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
;;; Remember cursor position in files when you close them,
;;; and restores the cursor position when you reopen the files.
(save-place-mode t)
;;; Saves the minibuffer history across Emacs sessions.
(savehist-mode t)
;;; maintains a menu of recently opened files.
(recentf-mode t)
;;; yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

(use-package yasnippet
  :ensure t
  :commands (yas-reload-all)
  :hook ((org-mode . yas-minor-mode)
         (markdown-mode . yas-minor-mode)
         (prog-mode . yas-minor-mode))
  :config
  (yas-reload-all))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;; https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

(require 'init-org)

(provide 'init)
