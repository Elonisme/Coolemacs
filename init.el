;;; init.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; Initialize the package management system early
(package-initialize)

;; setting mirror site
(setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))


;; loading lisp directory
(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory dir)))

;; loading other packages
;; not showing meassgae in`*message*' buffer
(with-temp-message ""
  (require 'init-base)
  (require 'init-basic)
  (require 'init-write)
  (require 'init-prog)
  (require 'init-themes)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(vterm flycheck-clj-kondo company-cider cider restart-emacs dap-mode denote yasnippet-snippets which-key visual-fill-column valign tommyh-theme spinner quickrun python-mode pytest ox-spectacle org-superstar org-modern org-fragtog org-download org-auto-tangle org-appear nordic-night-theme neotree multiple-cursors mixed-pitch markdown-preview-mode lv flycheck-posframe eglot deft counsel conda company-web company-tabnine company-statistics company-quickhelp company-prescient company-box color-theme-sanityinc-tomorrow blacken amx all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
