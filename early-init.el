;;; early-init.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; early-init.el is used to perform early initialization and optimization
;;; tasks before the full Emacs initialization. It is typically used to
;;; improve startup performance by configuring settings that don't rely
;;; on full package loading.
;;;

;;; Code:

;; Don't flicker GUI elements on startup
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)


;; Defer package loading until after full initialization
(setq package-enable-at-startup nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
