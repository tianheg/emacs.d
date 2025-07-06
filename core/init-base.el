(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs  'y-or-n-p
      auto-save-default    t
      ;; mouse-yank-at-point  t
      make-backup-files    t
      create-lockfiles     nil)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(defadvice align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align-regexp)

(save-place-mode t)
(setq-default tab-width 4)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(setq use-dialog-box nil)

(column-number-mode 1)

(electric-pair-mode 1)
(electric-indent-mode 1)

(setq inhibit-startup-echo-area-message t
      inhibit-startup-message t
      indicate-empty-lines t)

(setq use-short-answers t)

(blink-cursor-mode t)
(setq-default cursor-type 'bar)
(setq visible-cursor nil)

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(setq confirm-kill-processes nil)

(setq display-time-default-load-average nil
      display-time-format "%H:%M") ;;  [%m-%d %H:%M]
(display-time-mode t)
(display-battery-mode 1)

;; SmoothScrolling https://www.emacswiki.org/emacs/SmoothScrolling
(setq frame-resize-pixelwise t)
(pixel-scroll-precision-mode 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(setq inhibit-compacting-font-caches t)

(setq load-prefer-newer t)

(provide 'init-base)
