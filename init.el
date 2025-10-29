(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.liujiacai.net/gnu/")
        ("nongnu" . "https://elpa.liujiacai.net/nongnu/")
        ("melpa"  . "https://elpa.liujiacai.net/melpa/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-compute-statistics t))

(eval-when-compile
  (require 'use-package))

(add-to-list 'load-path "~/.emacs.d/core/")

(require 'init-base)
(require 'init-document)
(require 'init-enhance)
(require 'init-keymap)
(require 'init-modern)
(require 'init-ui)

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))
