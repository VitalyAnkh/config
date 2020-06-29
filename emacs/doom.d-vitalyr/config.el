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
(setq doom-font (font-spec :family "JetBrains Mono" :size 40 :weight 'light)
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

(require 'posframe)

(setq +latex-viewers '(pdf-tools))
(setq pdf-view-use-scaling t)
(setq pdf-view-resize-factor 1.5)
(setq-default TeX-engine 'xetex
              TeX-PDF-mode t)
(use-package pdf-tools
  :config
  (setq-default pdf-view-display-size 'fit-width)

  )
(setq TeX-source-correlate-start-server t)
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)
;;(setq-default preview-scale-function 2)
;; preview-scale-function and preview-scale has no effect in the size of the
;; preview image

(setq-default preview-default-document-pt 5)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)

(add-hook 'doom-first-file-hook #'auto-image-file-mode)
(auto-image-file-mode 1)

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-show-graph)
  :init
  (setq org-roam-directory org-directory)
  ;;(setq org-roam-graph-viewer "inkscape")
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
;; auto start org roam server
(add-hook 'org-mode #'(lambda () (org-roam-server-mode 1)))
;; (use-package pyim                       ;
;;   :ensure nil
;;   :config
;;   (use-package pyim-basedict
;;               :ensure nil
;;               :config (pyim-basedict-enable))
;;   ;;(setq default-input-method "pyim")
;;   (setq pyim-default-scheme 'xiaohe-shuangpin)
;; ;

  ;;(setq-default pyim-english-input-switch-functions
   ;;             '(pyim-probe-dynamic-english
    ;;              pyim-probe-isearch-mode
     ;;             pyim-probe-program-mode
      ;;            pyim-probe-org-structure-template))
  ;; (setq-default pyim-punctuation-half-width-functions
  ;;               '(pyim-probe-punctuation-line-beginning
  ;;                 pyim-probe-punctuation-after-punctuation))
  ;; (pyim-isearch-mode 1)
  ;; ;;(setq pyim-page-tooltip 'popup)

  ;;(setq pyim-page-length 5)
;;   (add-hook 'emacs-startup-hook #'(lambda () (pyim-restart-1 t)))
;;   )
;; (setq pyim-dicts                        ;
;;       '((:name "dict1" :file "/home/vitalyr/sdk/config/emacs/pyim-bigdict.pyim.gz")
;;         ))
        

(use-package rime
;;   :straight (rime :type git             ;
;;                   :host github          ;
;;                   :repo "DogLooksGood/emacs-rime" ;
;;                   :files ("*.el" "Makefile" "lib.c"))
  :custom
  (default-input-method "rime"))

(setq rime-user-data-dir "~/sdk/config/input_method/rime")
;; (setq rime-show-candidate "posframe")
(setq rime-disable-predicates
      '(rime-predicate-evil-mode-p
        rime-predicate-after-alphabet-char-p ;; 当光标处于紧挨着字母的位置时，自动由中文切换为英文
        rime-predicate-prog-in-code-p
        ))
;;(setq rime--popup t)
(setq rime-show-preedit t)

(use-package! valign
  :init
  (require 'valign)
  :hook
  ('org-mode . #'valign-mode))

(require 'kana)
(setq-hook! 'LaTeX-mode-hook +spellcheck-immediately nil)
(require 'org)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 4.0))
(setq org-preview-latex-default-process 'dvisvgm)
(use-package org-latex-instant-preview
  :defer t
  :hook (org-mode . org-latex-instant-preview-mode)
  :init
  (setq org-latex-instant-preview-tex2svg-bin
        ;; location of tex2svg executable
        "tex2svg"))
(org-roam-server-mode)
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
