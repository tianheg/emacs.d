;; -*- coding: utf-8; lexical-binding: t; -*-

;; Set the default font
(set-face-attribute 'default nil :font "Monospace")

;; Enable line numbers
(global-display-line-numbers-mode)
; (add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Enable syntax highlighting
(global-font-lock-mode t)

;; Enable automatic indentation
                                        ;(electric-indent-mode t)
;(setq-default standard-indent 2)
;(setq-default indent-tabs-mode nil)
(add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))

;; Automatically pair parentheses
(electric-pair-mode t)

;; Enable backup files
(setq make-backup-files t)

;; Enable auto-save files
(setq auto-save-default t)

;; Disable the menu bar
(menu-bar-mode -1)

;; Disable the tool bar
(tool-bar-mode -1)

;; Disable the scroll bars
(scroll-bar-mode -1)

;; Disable startup message
(setq inhibit-startup-message t)

;; http://yummymelon.com/devnull/surprise-and-emacs-defaults.html
(delete-selection-mode t)
(setq sentence-end-double-space nil)
(setq dired-auto-revert-buffer t)

;; Add welcome msg
(setq initial-scratch-message "archie, Emacs loves you!\n")

;; package
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(defvar bootstrap-version)
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

;; Optional
(setq use-package-always-ensure t)


;; magit
;(use-package magit
;  :ensure t
;  :commands (magit-status magit-get-current-branch))

;; theme
(use-package zenburn-theme
  :ensure t
  :config
  ;; Load the theme after `use-package` installs it
  (load-theme 'zenburn t))

;; Vertico for completion
(use-package vertico
  :init
  (vertico-mode t)
  :config
  (with-eval-after-load 'vertico
    (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
    (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-word)
    (define-key vertico-map (kbd "M-d") #'vertico-directory-delete-char)))

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

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))
