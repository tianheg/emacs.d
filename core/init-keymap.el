;;; init-keymap.el --- Keybindings and custom functions -*- lexical-binding: t -*-

;;; Code:

;; --- Custom Functions ---
(defvar current-date-time-format "%Y-%m-%d %H:%M:%S"
  "Format of date to insert with `insert-current-date-time'.")

(defun insert-current-date-time ()
  "Insert the current date and time into current buffer."
  (interactive)
  (insert (format-time-string current-date-time-format)))

(defun toggle-frame-alpha ()
  "Toggle frame transparency."
  (interactive)
  (let* ((pair (or (frame-parameter nil 'alpha) '(100 100)))
         (alpha (apply '+ pair)))
    (set-frame-parameter nil
                         'alpha
                         (if (or (null alpha) (eq alpha 200) (eq alpha 2.0))
                             '(85 60) '(100 100)))))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; --- Global Keybindings ---
(use-package emacs
  :ensure nil
  :bind (("C-2" . set-mark-command)
         ("C-x k" . kill-current-buffer)
         ("M-*" . match-paren)
         ("S-<backspace>" . kill-whole-line)))

;; --- Hydra ---
(use-package hydra
  :ensure t
  :bind ("<f9>" . hydra-default/body)
  :config
  (defhydra hydra-default (:hint nil :idle 1)
    "
^Edit^                ^Buffer^              ^Window^              ^Other^
^^^^^^^^---------------------------------------------------------------------------
_r_: replace string   _f_: find file        _1_: delete other     _ti_: toggle column
_w_: save buffer      _b_: switch buffer    _2_: split below      _m_: imenu
_o_: outline          _'_: file name        _3_: split right      _M_: consult outline
_s_: search buffer    _n_: line numbers     _x_: fullscreen       _c_: eshell
_e_: ripgrep          _u_: revert buffer    _X_: toggle alpha     _q_: quit
_i_: insert date      _j_: goto line
_l_: align text
"
    ("r" replace-string)
    ("w" save-buffer)
    ("o" consult-outline)
    ("s" consult-line)
    ("e" consult-ripgrep)
    ("i" insert-current-date-time)
    ("l" align-regexp)

    ("f" find-file)
    ("b" switch-to-buffer)
    ("'" show-file-name)
    ("n" display-line-numbers-mode)
    ("u" revert-buffer)
    ("j" consult-goto-line)

    ("1" delete-other-windows)
    ("2" split-window-below)
    ("3" split-window-horizontally)
    ("x" toggle-frame-fullscreen)
    ("X" toggle-frame-alpha)

    ("ti" display-fill-column-indicator-mode)
    ("m" consult-imenu)
    ("M" consult-outline)
    ("c" eshell)
    ("q" nil :exit t)))

(provide 'init-keymap)
;;; init-keymap.el ends here
