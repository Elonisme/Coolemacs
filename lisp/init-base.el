;;; init-base.el -- init-base settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Remove GUI elements
(tool-bar-mode -1)             ; Hide the outdated icons
(scroll-bar-mode -1)           ; Hide the always-visible scrollbar
(setq inhibit-splash-screen t) ; Remove the "Welcome to GNU Emacs" splash screen
(setq use-file-dialog nil)      ; Ask for textual confirmation instead of GUI
(setq ring-bell-function 'ignore) ;;close ring


;; Remove initial scratch message and "For information about GNU Emacs and the
;; GNU system, type C-h C-a"
(use-package emacs
  :init
   (setq initial-scratch-message
        (concat ";; Happy hacking, "
                (capitalize user-login-name) " - Emacs ♥ you!\n\n"))
  (defun display-startup-echo-area-message ()
    (message "")))

;; Always :defer t
(setq use-package-always-defer t)

;; Allow y/n instead of having to type yes/no
(use-package emacs
  :init
  (defalias 'yes-or-no-p 'y-or-n-p))

;; UTF-8 everywhere
(use-package emacs
  :init
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))

;; Use spaces by default, and set tab width to 2
(use-package emacs
  :init
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2))


;; open save-place
(save-place-mode 1)

;; cancel backup
(setq make-backup-files nil)

;; open visual-line-mode
(global-visual-line-mode t)

;; 显示行号
(global-display-line-numbers-mode t)

;; auto delete save files
(setq delete-auto-save-files t)

;; set font size
;; (set-face-attribute 'default nil :height 150)


(provide 'init-base)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-base.el ends here
