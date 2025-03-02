;;; init.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; Initialize the package management system early
(setq package-archives '(("gnu"    . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/packages/")
                         ("melpa"  . "https://melpa.org/packages/")))
(package-initialize)


;; loading lisp directory
(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory dir)))

;; loading other packages
;; not showing meassgae in`*message*' buffer
(with-temp-message ""
  (require 'init-foundation)
  (require 'init-themes)
  (require 'init-tool)
  (require 'init-write)
  (require 'init-prog)
  (require 'init-plan)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e" "9b21c848d09ba7df8af217438797336ac99cbbbc87a08dc879e9291673a6a631" "5c7720c63b729140ed88cf35413f36c728ab7c70f8cd8422d9ee1cedeb618de5" "4e2e42e9306813763e2e62f115da71b485458a36e8b4c24e17a2168c45c9cf9d" "6bdc4e5f585bb4a500ea38f563ecf126570b9ab3be0598bdf607034bb07a8875" "6819104c5f7d70485b32c10323aa396806d282fcee5b707e462bf3d156f44c39" default))
 '(org-safe-remote-resources
   '("\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-bigblow\\.setup\\'" "\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'" "\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-NAME\\.setup\\'"))
 '(package-selected-packages
   '(ivy-emms mpvi emms minions powerline doom-themes org-historian org-side-tree ivy-historian restart-emacs ivy-yasnippet ivy-posframe posframe centered-cursor-mode marginalia orderless vertico nerd-icons-corfu corfu nerd-icons-ivy-rich nerd-icons-completion nerd-icons-ibuffer org-pretty-tags org-super-agenda org-timeblock org-tag-beautify super-save snippet-collection auto-yasnippet command-log-mode org-wild-notifier org-notify org-alert org-modern cnfonts org org-denote org-journal company-yasnippet yasnippet-snippets yasnippet company quickrun org-download org-auto-tangle org-fragtog org-appear org-superstar markdown-preview-mode markdown-mode smart-mode-line ace-window amx counsel which-key neotree all-the-icons multiple-cursors dashboard color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
