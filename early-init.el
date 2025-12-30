;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Code:

;; --- Performance ---
(setq gc-cons-threshold (* 1024 1024 100) ;; 100MB
      gc-cons-percentage 0.6
      read-process-output-max (* 4 1024 1024))

;; --- UI ---
(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (horizontal-scroll-bars)
        (vertical-scroll-bars)
        (fullscreen . maximized)
        (undecorated . t)))

(setq frame-inhibit-implied-resize t)

;; --- Environment ---
(setenv "LSP_USE_PLISTS" "true")

(provide 'early-init)
;;; early-init.el ends here
