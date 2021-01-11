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
(setq doom-font (font-spec :family "mononoki" :size 22 :weight 'light)
      doom-variable-pitch-font (font-spec :family "等距更纱黑体 SC Light" :size 24)
      doom-unicode-font (font-spec :family "等距更纱黑体 SC" :size 22)
      doom-big-font (font-spec :family "等距更纱黑体 SC" :size 24))
(set-fontset-font t 'unicode "Symbola" nil 'prepend)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

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
(after! rustic
  (setq rustic-lsp-server 'rust-analyzer)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  (setq lsp-rust-analyzer-proc-macro-enable t)
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-display-parameter-hints t)
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (setq lsp-rust-all-features t)
  ;; (setq lsp-rust-full-docs t)
  (setq lsp-enable-semantic-highlighting t))
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


(setq +latex-viewers '(pdf-tools))
(setq pdf-view-use-scaling t
      pdf-view-use-imagemagick nil
      pdf-view-resize-factor 10)
(setq-default TeX-engine 'xetex
              TeX-PDF-mode t)
(use-package pdf-tools
  :config
  (setq-default pdf-view-display-size 'fit-width)
  )

(setq TeX-source-correlate-start-server t)
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)
(setq-default preview-scale-function 2)
;; preview-scale-function and preview-scale has no effect in the size of the
;; preview image

(setq-default preview-default-document-pt 20)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)


(add-hook 'doom-first-file-hook #'auto-image-file-mode)
;;(auto-image-file-mode 1)

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
;; (add-hook 'org-mode #'(lambda () (org-roam-server-mode 1)))


(setq default-input-method "rime")
(setq rime-user-data-dir "~/sdk/config/input_method/rime")
(setq rime-show-candidate 'posframe)
(setq rime-disable-predicates
      '(rime-predicate-evil-mode-p
        rime-predicate-after-alphabet-char-p ;; 当光标处于紧挨着字母的位置时，自动由中文切换为英文
        rime-predicate-prog-in-code-p
        ))
(setq rime-posframe-properties
      (list :font "sarasa ui sc"
            :internal-border-width 10))

(setq rime--popup 1)
(setq rime-show-preedit 1)
(setq rime-posframe-fixed-position t)

(use-package! valign
  :init
  (require 'valign)
  :hook
  ('org-mode . #'valign-mode))

(require 'kana)

(setq-hook! 'LaTeX-mode-hook +spellcheck-immediately nil)

(require 'org)

(use-package org-latex-impatient
  :defer t
  :hook (org-mode . org-latex-impatient-mode)
  :init
  (setq org-latex-impatient-tex2svg-bin "tex2svg")
  (setq org-latex-impatient-scale 1)
  (setq org-latex-impatient-delay 0.01)
  )

(setq org-format-latex-options (plist-put org-format-latex-options :scale 2))
(setq org-preview-latex-default-process 'dvisvgm)

(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

(require 'deft)
(setq deft-directory org-directory)

;; (use-package sis
;;   ;; :hook
;;   ;; enable the /follow context/ and /inline region/ mode for specific buffers
;;   ;; (((text-mode prog-mode) . sis-follow-context-mode)
;;   ;;  ((text-mode prog-mode) . sis-inline-mode))

;;   :config
;;   ;; (sis-ism-lazyman-config
;;   ;;  ;; "com.apple.keylayout.ABC"
;;   ;;  "com.apple.keylayout.US"
;;   ;;  ;; "im.rime.inputmethod.Squirrel.Rime"
;;   ;;  "com.sogou.inputmethod.sogou.pinyin")

;;   (sis-ism-lazyman-config nil nil 'fcitx5)
;;   ;; enable the /cursor color/ mode
;;   (sis-global-cursor-color-mode t)
;;   ;; enable the /respect/ mode
;;   (sis-global-respect-mode t)
;;   ;; enable the /follow context/ mode for all buffers
;;   (sis-global-follow-context-mode t)
;;   ;; enable the /inline english/ mode for all buffers
;;   (sis-global-inline-mode t)
;;   )

;; (require 'nox)
;; (dolist (hook (list
;;                'html-mode-hook
;;                'css-mode-hook
;;                'js-mode-hook
;;                'json-mode-hook
;;                ;;'rust-mode-hook
;;                'python-mode-hook
;;                'ruby-mode-hook
;;                'java-mode-hook
;;                'sh-mode-hook
;;                'tex-mode-hook
;;                'php-mode-hook
;;                'c-mode-common-hook
;;                'c-mode-hook
;;                'csharp-mode-hook
;;                'c++-mode-hook
;;                'haskell-mode-hook


;;                ))
;;  (add-hook hook '(lambda () (nox-ensure))))


;; garbage collection for org-roam
(setq org-roam-db-gc-threshold most-positive-fixnum)

(setq haskell-process-type 'cabal-new-repl)

(add-hook 'prog-mode-hook #'wucuo-start)
(add-hook 'text-mode-hook #'wucuo-start)

;; to speed up company
(setq company-idle-delay 0)

(use-package maple-run
  :ensure nil
  :commands (maple-run))
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-opera-light t)

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
;;(transwin-toggle-transparent-frame)

(use-package company-tabnine
  :ensure t)
(require 'company-tabnine)
(add-to-list 'company-backends #'company-tabnine)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)
;; use aspell as ispell backend
(setq-default ispell-program-name "aspell")
;; use American English as ispell default dictionary
(ispell-change-dictionary "american" t)

(setq
 ghc-ghc-options '("-fno-warn-missing-signatures")
 haskell-compile-cabal-build-command "cd %s && stack build"
 haskell-process-type 'stack-ghci
 haskell-interactive-popup-errors nil
 haskell-process-args-stack-ghci '("--ghc-options=-ferror-spans" "--with-ghc=ghci")
 haskell-process-path-ghci "stack"
 )

;; When time comes, I will use emacs-eaf again
;; (require 'eaf)
;; (use-package eaf
;;   :load-path "~/usr/share/emacs/site-lisp/eaf" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
;;   :custom
;;   (eaf-find-alternate-file-in-dired t)
;;   :config
;;   (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key take_photo "p" eaf-camera-keybinding))

(add-to-list 'load-path "/home/vitalyr/.opam/default/share/emacs/site-lisp")
     (require 'ocp-indent)
(setq word-wrap-by-category t)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; add this after install emacs-libvterm-git from AUR
;; (require 'vterm)

(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)
