;;; init-themes.el -- theme settings -*-  lexical-binding: t -*-
;;; Commentary:

;;; Code:


(use-package nordic-night-theme
  :ensure t
)

(use-package tommyh-theme
  :ensure t
  )

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  )


;; auto-theme-switch.el

(defvar auto-theme-switch-light-theme 'sanityinc-tomorrow-day  "The light theme to be used during the day.")

(defvar auto-theme-switch-dark-theme 'sanityinc-tomorrow-night
  "The dark theme to be used during the night.")

(defun auto-theme-switch-set-theme ()
  "Set the theme based on the time of day."
  (interactive)
  (let ((current-hour (string-to-number (format-time-string "%H"))))
    (if (and (>= current-hour 6) (< current-hour 18))
        (load-theme auto-theme-switch-light-theme t)
      (load-theme auto-theme-switch-dark-theme t))))

;; Set the initial theme
(auto-theme-switch-set-theme)

(global-set-key (kbd "C-c l") 'auto-theme-switch-set-theme)

;; Hook into the timer to check and switch theme every hour
(run-with-timer 0 3600 'auto-theme-switch-set-theme)


(provide 'init-themes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-themes.el ends here
