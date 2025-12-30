;;; init-media.el --- Media configuration -*- lexical-binding: t -*-

;;; Code:

(use-package listen
  :load-path "lisp/listen"
  :commands (listen)
  :init
  ;; Music library location
  (setq listen-directory "~/Music/")

  ;; Use MPV as backend
  (setq listen-backend #'make-listen-player-mpv)
  (setq listen-mpv-volume 50)

  ;; Mode line display format
  (setq listen-lighter-format "🎵:%s %a: %t (%r)%E ")
  (setq listen-lighter-title-max-length 40)

  ;; Repeat mode settings
  (setq listen-queue-repeat-mode 'queue)  ; Options: nil, 'queue, 'shuffle

  :config
  ;; Load all modules needed by the transient menu
  (require 'listen-queue)
  (require 'listen-library)
  (require 'listen-mpd)

  ;; Enable mode-line lighter on demand when playback starts.
  (defun listen--ensure-mode-enabled (&rest _)
    (unless listen-mode
      (listen-mode 1)))
  (dolist (fn '(listen-play listen-queue-play listen-library-play listen-dired-play))
    (advice-add fn :before #'listen--ensure-mode-enabled))

  ;; Customize faces
  (custom-set-faces
   '(listen-artist ((t (:foreground "#7CB8BB" :weight bold))))
   '(listen-title ((t (:foreground "#F0DFAF" :weight semi-bold))))
   '(listen-album ((t (:foreground "#93E0E3" :slant italic))))
   '(listen-genre ((t (:foreground "#DFAF8F"))))
   '(listen-rating ((t (:foreground "#CC9393")))))

  (with-eval-after-load 'listen
    ;; Filter to common audio extensions
    (defvar listen-audio-regexp
      "\\.\\(mp3\\|flac\\|ogg\\|opus\\|m4a\\|aac\\|wav\\|alac\\|aiff\\|wma\\)\\'"
      "Regexp for audio files used by Listen helpers.")

    (defun listen-library-audio (dir)
      "Open Listen library view for DIR, filtering by `listen-audio-regexp'."
      (interactive "D音乐目录: ")
      (let ((dir (expand-file-name dir)))
        (listen-library (lambda ()
                          (listen-queue-tracks-for
                           (directory-files-recursively dir listen-audio-regexp)))
                        :name dir)))))

(provide 'init-media)
;;; init-media.el ends here
