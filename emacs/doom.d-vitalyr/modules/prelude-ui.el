;;; -*- lexical-binding: t; -*-

(defcustom prelude-ui-accent-color
  "brown2"
  "Accent color of UI elements. This affects the mode line."
  :group 'prelude
  :type '(color))

(defcustom prelude-ui-frame-alpha       ; TODO
  0.92
  "Default frame alpha."
  :group 'prelude)

;; Pixelwise resize
;; (setq frame-resize-pixelwise t)

;; Don't show useless UI elements
;; (add-hook 'after-init-hook
;;           #'(lambda ()
;;               (menu-bar-mode 1)
;;               (tool-bar-mode 0)))

;; My vendor settings!
;; 1. Choose a better background color (thanks to Xah Lee)
;; 2. Better default size
;;
;; I strongly advise you write equivalent settings in your ~/.Xresources!
(setq default-frame-alist nil)
(setq initial-frame-alist nil)
(if (display-graphic-p)
    (progn
      (setq default-frame-alist
            '((vertical-scroll-bars . nil))))
  (progn
    (setq default-frame-alist '((tool-bar-lines . 0)))))
(add-to-list 'initial-frame-alist '(height . 50))
(add-to-list 'initial-frame-alist '(width . 140))

;; Mac-specific settings
(when *is-a-mac*
  (use-package ns-auto-titlebar
    :init
    (ns-auto-titlebar-mode)))

;; Default theme
(use-package srcery-theme
  :disabled
  :config
  (load-theme 'srcery t)
  (set-face-attribute 'font-lock-comment-face nil :foreground "orange")
  (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "orange"))

;; Mode line
(use-package diminish
  :config
  (diminish 'eldoc-mode))

(use-package doom-modeline
  :disabled
  :hook ((after-init . doom-modeline-mode))
  :init
  (setq doom-modeline-minor-modes nil)
  :config
  (set-face-attribute 'doom-modeline-bar nil :background prelude-ui-accent-color))

;; I'm the winner ;-)
;; (use-package winner
;;   :defer t
;;   :bind (:map winner-mode-map
;;               ("C-c ," . winner-undo)
;;               ("C-c ." . winner-redo))
;;   :init
;;   (defvar winner-dont-bind-my-keys t)
;;   (add-hook 'after-init-hook #'winner-mode))

(use-package winum
  ;; M-0 is reserved for Treemacs
  ;; Treemacs will be moved here from apps soon
  :hook ((after-init . winum-mode))
  :bind (("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6)
         ("M-7" . winum-select-window-7)
         ("M-8" . winum-select-window-8)
         ("M-9" . winum-select-window-9)))

;; Rainbow
(use-package rainbow-mode
  :commands (rainbow-mode))

;; Enable ligatures
(when (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode))

;; Auto set margins
;; (use-package perfect-margin
;;   :disabled
;;   :quelpa (perfect-margin :fetcher github :repo "mpwang/perfect-margin")
;;   :custom
;;   (perfect-margin-visible-width 128)
;;   :config
;;   (setq perfect-margin-ignore-regexps '("^minibuf"))
;;   (perfect-margin-mode t)
;;   (dolist (margin '("<left-margin> " "<right-margin> "))
;;     (global-set-key (kbd (concat margin "<mouse-1>")) 'ignore)
;;     (global-set-key (kbd (concat margin "<mouse-3>")) 'ignore)
;;     (dolist (multiple '("" "double-" "triple-"))
;;       (global-set-key (kbd (concat margin "<" multiple "wheel-up>")) 'mwheel-scroll)
;;       (global-set-key (kbd (concat margin "<" multiple "wheel-down>")) 'mwheel-scroll))))

;; Dashboard
;; (use-package dashboard
;;   :disabled
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-items '((projects . 5)
;;                           (bookmarks . 5)
;;                           (recents . 5)))
;;   (setq dashboard-center-content t)
;;   (setq dashboard-set-heading-icons t)
;;   (setq dashboard-set-file-icons t))

;; Cycle themes
(defcustom prelude-themes
  (list 'doom-monokai-pro)
  "Themes that you may want to use."
  :group 'prelude)

(defun cycle-theme ()
  "Cycle around `prelude-themes'."
  (interactive)
  (require 'dash)
  (if (> (length prelude-themes) 1)
      (let ((next (car prelude-themes)))
        (dolist (current custom-enabled-themes)
          (disable-theme current))
        (if (listp next)
            (dolist (theme next)
              (load-theme theme t))
          (load-theme next t))
        (setq prelude-themes (append (cdr prelude-themes)
                                     (list (car prelude-themes)))))))

;; valign
(use-package valign
  :commands (valign-mode))

;; ligatures
(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://" "/\\" "\\/"))
  (global-ligature-mode t))

;; don't pop windows everywhere!
(use-package shackle
  :hook (after-init . shackle-mode)
  :custom
  (shackle-default-size 0.5)
  (shackle-default-alignment 'below)
  (shackle-rules
   '((magit-log-mode       :select t :inhibit-window-quit t :same t)
     ("*quickrun*"         :select t :inhibit-window-quit t :same t)
     (profiler-report-mode :select t)
     (apropos-mode         :select t :align t :size 0.3)
     (help-mode            :select t :align t :size 0.4)
     (comint-mode          :select t :align t :size 0.4)
     (grep-mode            :select t :align t)
     (rg-mode              :select t :align t)
     ("*Flycheck errors*"         :select t   :align t :size 10)
     ("*Backtrace*"               :select t   :align t :size 14)
     ("*ydcv*"                    :select nil :align t :size 0.4)
     ("*Shell Command Output*"    :select nil :align t :size 0.4)
     ("*Async Shell Command*"     :select nil :align t :size 0.4)
     ("*Org-Babel Error Output*"  :select nil :align t :size 0.3)
     ("*package update results*"  :select nil :align t :size 10)
     ("*Process List*"            :select t   :align t :size 0.3)
     ("*Help*"                    :select t   :align t :size 0.3)
     ("*Occur*"                   :select t   :align right)
     ("\\*ivy-occur .*\\*"        :select t   :align right :regexp t)
     ("\\*eldoc\\( for \\)?.*\\*" :select nil :align t :size 15 :regexp t))))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:family "Iosevka Slab" :foundry "nil" :slant normal :weight normal :height 141 :width normal))))
;;  '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
;;  '(org-block ((t (:extend t :background "#f7e0c3" :foreground "#5b5143" :family "Latin Modern Mono"))))
;;  '(org-block-begin-line ((t (:extend t :background "#f7e0c3" :foreground "gray" :weight semi-bold :height 151 :family "Monaco"))))
;;  '(org-code ((t (:foreground "#957f5f" :family "Latin Modern Mono"))))
;;  '(org-document-title ((t (:foreground "midnight blue" :weight bold :height 2.0))))
;;  '(org-hide ((t (:foreground "#E5E9F0" :height 0.1))))
;;  '(org-level-1 ((t (:inherit outline-1 :foreground "#076678" :weight extra-bold :height 1.5 :family "Canela Deck"))))
;;  '(org-level-2 ((t (:inherit outline-2 :foreground "#b57614" :height 1.2 :family "Canela Deck"))))
;;  '(org-list-dt ((t (:foreground "#7382a0"))))
;;  '(org-verbatim ((t (:foreground "#81895d" :family "Latin Modern Mono"))))
;;  '(variable-pitch ((t (:family "Georgia")))))


(provide 'prelude-ui)
