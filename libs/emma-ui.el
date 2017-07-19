;; TODO: create settings to toggle these.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No tabs.
(setq indent-tabs-mode nil)
(setq tab-width 2)

;; Indentation.
(setq-default c-basic-offset 2)

;; Column number mode.
(setq column-number-mode t)

(setq inhibit-startup-message t)

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(provide 'emma-ui)
