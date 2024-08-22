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
(set-face-attribute 'default nil :height 120)

;; 启用 electric-pair-mode
(electric-pair-mode 1)

;; open pixel-scroll
(pixel-scroll-mode t)
(pixel-scroll-precision-mode t)

;; 使用代理
(setq my-proxy "127.0.0.1:7890")

;; Configure network proxy
(defun show-proxy ()
  "Show http/https proxy."
  (interactive)
  (if url-proxy-services
      (message "Current proxy is \"%s\"" my-proxy)
    (message "No proxy")))

(defun set-proxy ()
  "Set http/https proxy."
  (interactive)
  (setq url-proxy-services `(("http" . ,my-proxy)
                             ("https" . ,my-proxy)))
  (show-proxy))

(defun unset-proxy ()
  "Unset http/https proxy."
  (interactive)
  (setq url-proxy-services nil)
  (show-proxy))

(defun toggle-proxy ()
  "Toggle http/https proxy."
  (interactive)
  (if url-proxy-services
      (unset-proxy)
    (set-proxy)))

(global-set-key (kbd "C-c p") 'toggle-proxy)



;; 启用原生编译的默认设置
(setq native-comp-deferred-compilation t)  ;; 延迟编译，以加快启动速度

;; 如果原生编译遇到问题，可以使用以下配置来忽略错误
(setq native-comp-async-report-warnings-errors nil)  ;; 禁用编译错误和警告报告

;; 为了避免在 Emacs 启动时显示警告，可以禁用原生编译的消息
(setq native-comp-async-query-on-exit nil)  ;; 关闭退出时的编译警告

;; 设置编译后的文件存放位置
(setq native-comp-eln-load-path (list (expand-file-name "eln-cache/" user-emacs-directory)))

;; 仅在需要时编译第三方包
(setq package-native-compile t)  ;; 编译已安装的包

;; 添加一些性能优化选项
(setq comp-deferred-compilation t)  ;; 延迟编译，使 Emacs 更快启动
(setq comp-async-report-warnings-errors nil)  ;; 禁止报告编译时的警告和错误

;; 显示 Emacs 启动时的性能信息
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs startup time：%s seconds" (emacs-init-time))
            ))

;; 将垃圾回收阈值设置为 100MB，默认是 800KB
(setq gc-cons-threshold (* 100 1024 1024))  ;; 增大垃圾回收的阈值，减少回收频率

;; 将每次分配内存后进行垃圾回收的阈值设置为 0.1
(setq gc-cons-percentage 0.1)  ;; 增加内存分配比例，减少垃圾回收的频率

;; 在 Emacs 退出时恢复默认的垃圾回收设置
(add-hook 'kill-emacs-hook
          (lambda ()
            (setq gc-cons-threshold (* 20 1024 1024))  ;; 启动后将阈值恢复为较小的值
            (setq gc-cons-percentage 0.1)))

;; 在 Emacs 最小化或切换应用时，手动触发垃圾回收
(add-hook 'focus-out-hook #'garbage-collect)

;; 在 Emacs 空闲时延迟执行垃圾回收
(run-with-idle-timer 5 t #'garbage-collect)  ;; 每隔 5 秒空闲时间执行垃圾回收

;; 显示每次垃圾回收的信息（可选）
(setq garbage-collection-messages t)  ;; 设置为 t 可以查看垃圾回收的详情


(provide 'init-base)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-base.el ends here
