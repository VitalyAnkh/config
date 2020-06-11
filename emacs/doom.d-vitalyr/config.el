;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "VitalyR"
      user-mail-address "vitalyankh@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "FantasqueSansMono Nerd Font Mono" :size 40 :weight 'semi-light)
                 doom-variable-pitch-font (font-spec :family "Fira Code" :size 40))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/projects/learn/Notebook/org")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq +latex-viewers '(pdf-tools))
(setq-default TeX-engine 'xetex
              TeX-PDF-mode t)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)

(add-hook 'doom-first-file-hook #'auto-image-file-mode)
(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-show-graph)
  :init
  (setq org-roam-directory org-directory)
  ;;(setq org-roam-graph-viewer "/usr/bin/open")
  (map! :leader
        :prefix "n"
        :desc "Org-Roam-Insert" "i" #'org-roam-insert
        :desc "Org-Roam-Find" "/" #'org-roam-find-file
        :desc "Org-roam-Buffer" "r" #'org-roam
        :desc "Org-Roam-Show-Graph" "g" #'org-roam-show-graph
        )
  :config
  (org-roam-mode t)
  (require 'org-roam-protocol) ;; require org-roam-protocol here
  )
(setq org-roam-server-host "127.0.0.1"
                                        org-roam-server-port 8080
                                        org-roam-server-authenticate nil
                                        org-roam-server-label-truncate t
                                        org-roam-server-label-truncate-lenght 60
                                        org-roam-server-label-wrap-length 20)

(use-package pyim
  :ensure nil
  :config
  (use-package pyim-basedict
              :ensure nil
              :config (pyim-basedict-enable))
  (setq default-input-method "pyim")
  (setq pyim-default-scheme 'xiaohe-shuangpin)

  ;;(setq-default pyim-english-input-switch-functions
   ;;             '(pyim-probe-dynamic-english
    ;;              pyim-probe-isearch-mode
     ;;             pyim-probe-program-mode
      ;;            pyim-probe-org-structure-template))
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  (pyim-isearch-mode 1)
  ;;(setq pyim-page-tooltip 'popup)

  ;;(setq pyim-page-length 5)
  (add-hook 'emacs-startup-hook #'(lambda () (pyim-restart-1 t)))
  )
(setq pyim-dicts
      '((:name "dict1" :file "/home/vitalyr/sdk/config/emacs/pyim-bigdict.pyim.gz")
        ))
        