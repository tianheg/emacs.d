;;; init.el --- Initialization entry point -*- lexical-binding: t -*-

;;; Code:

;; --- Package Setup ---
(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.liujiacai.net/gnu/")
        ("nongnu" . "https://elpa.liujiacai.net/nongnu/")
        ("melpa"  . "https://elpa.liujiacai.net/melpa/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t
        use-package-compute-statistics t))

(eval-when-compile
  (require 'use-package))

;; --- Load Path ---
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))

;; --- Modules ---
(require 'init-base)
(require 'init-ui)
(require 'init-enhance)
(require 'init-document)
(require 'init-tools)
(require 'init-keymap)
(require 'init-media)

;; --- Custom File ---
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;;; init.el ends here
