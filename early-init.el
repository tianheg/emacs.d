;; (setq package-enable-at-startup nil)
;; don't GC during startup to save time, emacs-init-time from 0.015984 to 0.008002
(unless (bound-and-true-p my-computer-has-smaller-memory-p)
  (setq gc-cons-percentage 0.6)
  (setq gc-cons-threshold most-positive-fixnum))

(provide 'early-init)

