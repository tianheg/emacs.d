;;; init-document.el --- Document and Note-taking configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for Markdown, Org-mode, YAML, and other document formats.

;;; Code:

;; --- Variables ---
(defvar my/org-daily-file "~/Documents/daily/2026.org"
  "Path to the main daily org file.")

(defvar my/org-notes-directory "~/Documents/daily/"
  "Path to the daily directory.")

;; --- Markdown ---
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package htmlize
  :ensure t
  :defer t)

;; --- Org Mode ---
(use-package org
  :load-path (lambda () (expand-file-name "elpa/org-mode/lisp" user-emacs-directory))
  :mode ("\\.org\\'" . org-mode)
  :bind (("M-p" . org-previous-visible-heading)
         ("M-n" . org-next-visible-heading)
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :init
  (setq org-adapt-indentation nil
        org-log-done t
        org-src-tab-acts-natively nil
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-startup-folded t
        org-capture-templates nil
        org-agenda-files (list my/org-daily-file)
        org-protocol-default-template-key "w")
  :config
  (require 'org-protocol)
  (require 'org-inlinetask)
  (require 'org-tempo)
  (require 'org-indent)
  (require 'org-agenda)

  ;; GTD & TODO Settings
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d@/!)")))

  (setq org-priority-faces
        '((?A :foreground "#ff6c6b" :weight bold)
          (?B :foreground "#98be65" :weight bold)
          (?C :foreground "#c678dd" :weight bold)))

  (setq org-todo-keyword-faces
        '(("DONE" . "dark")))

  (setq org-tag-alist
        '(("important" . ?i)
          ("urgent"    . ?u)))

  (setq org-tag-faces
        '(("important" . (:foreground "red" :weight bold))
          ("urgent"    . (:foreground "orange" :weight bold))))

  ;; Agenda Settings
  (setq org-agenda-include-deadlines t
        org-agenda-include-diary nil
        org-agenda-compact-blocks t
        org-agenda-start-with-log-mode t
        org-agenda-start-on-weekday 1
        org-agenda-span 28
        org-deadline-warning-days 7
        org-agenda-skip-deadline-prewarning-if-scheduled t
        org-agenda-skip-scheduled-delay-if-deadline t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t)

  (setq org-agenda-custom-commands
        '(("g" "global overview"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "* HIGH-PRIORITY:")))
            (tags "PRIORITY=\"B\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "* MEDIUM-PRIORITY:")))
            (tags "PRIORITY=\"C\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "* LOW-PRIORITY:")))))
          ;; ("x" "艾森豪威尔矩阵（Eisenhower Matrix）"
          ;;  ((tags-todo "+important+urgent"
          ;;              ((org-agenda-overriding-header "象限I：重要且紧急")))
          ;;   (tags-todo "+important-urgent"
          ;;              ((org-agenda-overriding-header "象限 II: 重要但不紧急")))
          ;;   (tags-todo "-important+urgent"
          ;;              ((org-agenda-overriding-header "象限 III: 紧急但不重要")))
          ;;   (tags-todo "-important-urgent"
          ;;              ((org-agenda-overriding-header "象限 IV: 不紧急也不重要")))))
                       ))

  ;; Agenda Appearance
  (setq org-agenda-breadcrumbs-separator " -> "
        org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈┈ now"
        org-agenda-time-grid '((weekly today require-timed)
                               (800 1000 1200 1400 1600 1800 2000)
                               "---" "┈┈┈┈┈┈┈┈┈┈┈┈┈")
        org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                   (todo . " %i %-12:c")
                                   (tags . " %i %-12:c")
                                   (search . " %i %-12:c")))

  (setq org-agenda-format-date
        (lambda (date)
          (concat "\n" (make-string (max 0 (1- (window-width))) ?─)
                  "\n"
                  (org-agenda-format-date-aligned date))))

  (setq org-cycle-separator-lines 2)

  ;; Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (css . t)
     (js . t)
     (org . t)
     (python . t)
     (shell . t)))

  ;; Faces
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
   '(org-level-2 ((t (:inherit outline-1 :height 1.2))))
   '(org-level-3 ((t (:inherit outline-1 :height 1.0))))
   '(org-level-4 ((t (:inherit outline-1 :height 1.0))))))

;; --- Org Extensions ---
(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-mode))

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode))

(use-package org-grapher
  :load-path (lambda () (expand-file-name "lisp/org-grapher" user-emacs-directory))
  :config
  (setq org-grapher-notes-directory my/org-notes-directory))

(use-package org-contrib
  :ensure t
  :after org)

;; --- Custom Functions ---
(defun my/eisenhower-matrix-dashboard ()
  "创建一个 2x2 的窗口布局展示艾森豪威尔矩阵"
  (interactive)
  (require 'org-agenda)
  (let ((org-agenda-window-setup 'current-window)
        (org-agenda-sticky t))
    (delete-other-windows)
    ;; Q1: Top Left
    (org-tags-view t "+important+urgent")
    (rename-buffer "*Q1: Do Now*" t)

    ;; Q2: Top Right
    (let ((win2 (split-window-right)))
      (with-selected-window win2
        (org-tags-view t "+important-urgent")
        (rename-buffer "*Q2: Schedule*" t)))

    ;; Q3: Bottom Left
    (let ((win3 (split-window-below)))
      (with-selected-window win3
        (org-tags-view t "-important+urgent")
        (rename-buffer "*Q3: Delegate*" t)))

    ;; Q4: Bottom Right
    (let ((win2 (window-in-direction 'right)))
      (with-selected-window win2
        (let ((win4 (split-window-below)))
          (with-selected-window win4
            (org-tags-view t "-important-urgent")
            (rename-buffer "*Q4: Delete*" t)))))))

;; --- Other Formats ---
(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package beancount
  :ensure t
  :mode ("\\.beancount\\'" . beancount-mode))

(provide 'init-document)
;;; init-document.el ends here
