;;; config.el -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: © 2020-2023 tecosaur <contact@tecosaur.net>
;; SPDX-License-Identifier: MIT

;; Generated at 2023-09-14T11:33:23+0800 from the literate configuration.

(add-to-list 'load-path "~/.config/doom/subconf/")

;;:------------------------
;;; Confpkg timings
;;:------------------------

;; This block defines `confpkg-timings-report', `confpkg-with-record',
;; `confpkg-finish-record', `confpkg-start-record',
;; `confpkg-create-record', `confpkg-record-num',
;; `confpkg-record-branch', and `confpkg-load-time-tree'.

(defvar confpkg-load-time-tree (list (list 'root)))
(defvar confpkg-record-branch (list 'root))
(defvar confpkg-record-num 0)
(defun confpkg-create-record (name elapsed &optional parent enclosing)
  (let ((parent (assoc (or parent (car confpkg-record-branch))
                       confpkg-load-time-tree))
        (record (cons name (list (list 'self
                                       :name (format "%s" name)
                                       :num (cl-incf confpkg-record-num)
                                       :elapsed elapsed
                                       :enclosing enclosing)))))
    (push record confpkg-load-time-tree)
    (push record (cdr parent))
    record))

(defun confpkg-start-record (name &optional parent)
  (let ((record (confpkg-create-record name 0.0e+NaN parent t)))
    (plist-put (cdadr record) :start (float-time))
    (push name confpkg-record-branch)
    record))

(defun confpkg-finish-record (name)
  (let ((self-record (cdar (last (cdr (assoc name confpkg-load-time-tree))))))
    (plist-put self-record :elapsed
               (- (float-time) (plist-get self-record :start) 0.0))
    (unless (equal (car confpkg-record-branch) name)
      (message "Warning: Confpkg timing record expected to finish %S, instead found %S. %S"
               name (car confpkg-record-branch) confpkg-record-branch))
    (setq confpkg-record-branch (cdr confpkg-record-branch))))
(defmacro confpkg-with-record (name &rest body)
  "Create a time record around BODY.
The record must have a NAME."
  (declare (indent 1))
  (let ((name-val (make-symbol "name-val"))
        (record-spec (make-symbol "record-spec")))
    `(let* ((,name-val ,name)
            (,record-spec (if (consp ,name-val) ,name-val (list ,name-val))))
       (apply #'confpkg-start-record ,record-spec)
       (unwind-protect
           (progn ,@body)
         (confpkg-finish-record (car ,record-spec))))))
(defadvice! +require--log-timing-a (orig-fn feature &optional filename noerror)
  :around #'require
  (if (or (featurep feature)
          (eq feature 'cus-start) ; HACK Why!?!
          (assoc (format "require: %s" feature) confpkg-load-time-tree))
      (funcall orig-fn feature filename noerror)
    (confpkg-with-record (list (format "require: %s" feature)
                               (and (eq (car confpkg-record-branch) 'root)
                                    'requires))
      (funcall orig-fn feature filename noerror))))
(defun confpkg-timings-report (&optional sort-p node)
  "Display a report on load-time information.
Supply SORT-P (or the universal argument) to sort the results.
NODE defaults to the root node."
  (interactive
   (list (and current-prefix-arg t)))
  (let ((buf (get-buffer-create "*Confpkg Load Time Report*"))
        (depth 0)
        num-pad name-pad max-time max-total-time max-depth)
    (cl-labels
        ((sort-records-by-time
          (record)
          (let ((self (assoc 'self record)))
            (append (list self)
                    (sort (nreverse (remove self (cdr record)))
                          (lambda (a b)
                            (> (or (plist-get (alist-get 'self a) :total) 0.0)
                               (or (plist-get (alist-get 'self b) :total) 0.0)))))))
         (print-record
          (record)
          (cond
           ((eq (car record) 'self)
            (insert
             (propertize
              (string-pad (number-to-string (plist-get (cdr record) :num)) num-pad)
              'face 'font-lock-keyword-face)
             " "
             (propertize
              (apply #'concat
                     (make-list (1- depth) "• "))
              'face 'font-lock-comment-face)
             (string-pad (format "%s" (plist-get (cdr record) :name)) name-pad)
             (make-string (* (- max-depth depth) 2) ?\s)
             (propertize
              (format "%.4fs" (plist-get (cdr record) :elapsed))
              'face
              (list :foreground
                    (doom-blend 'orange 'green
                                (/ (plist-get (cdr record) :elapsed) max-time))))
             (if (= (plist-get (cdr record) :elapsed)
                    (plist-get (cdr record) :total))
                 ""
               (concat "   (Σ="
                       (propertize
                        (format "%.3fs" (plist-get (cdr record) :total))
                        'face
                        (list :foreground
                              (doom-blend 'orange 'green
                                          (/ (plist-get (cdr record) :total) max-total-time))))
                       ")"))
             "\n"))
           (t
            (cl-incf depth)
            (mapc
             #'print-record
             (if sort-p
                 (sort-records-by-time record)
               (reverse (cdr record))))
            (cl-decf depth))))
         (flatten-records
          (records)
          (if (eq (car records) 'self)
              (list records)
            (mapcan
             #'flatten-records
             (reverse (cdr records)))))
         (tree-depth
          (records &optional depth)
          (if (eq (car records) 'self)
              (or depth 0)
            (1+ (cl-reduce #'max (cdr records) :key #'tree-depth))))
         (mapreduceprop
          (list map reduce prop)
          (cl-reduce
           reduce list
           :key
           (lambda (p) (funcall map (plist-get (cdr p) prop)))))
         (elaborate-timings
          (record)
          (if (eq (car record) 'self)
              (plist-get (cdr record) :elapsed)
            (let ((total (cl-reduce #'+ (cdr record)
                                    :key #'elaborate-timings))
                  (self (cdr (assoc 'self record))))
              (if (plist-get self :enclosing)
                  (prog1
                      (plist-get self :elapsed)
                    (plist-put self :total (plist-get self :elapsed))
                    (plist-put self :elapsed
                               (- (* 2 (plist-get self :elapsed)) total)))
                (plist-put self :total total)
                total))))
         (elaborated-timings
          (record)
          (let ((record (copy-tree record)))
            (elaborate-timings record)
            record)))
      (let* ((tree
              (elaborated-timings
               (append '(root)
                       (copy-tree
                        (alist-get (or node 'root)
                                   confpkg-load-time-tree
                                   nil nil #'equal))
                       '((self :num 0 :elapsed 0)))))
             (flat-records
              (cl-remove-if
               (lambda (rec) (= (plist-get (cdr rec) :num) 0))
               (flatten-records tree))))
        (setq max-time (mapreduceprop flat-records #'identity #'max :elapsed)
              max-total-time (mapreduceprop flat-records #'identity #'max :total)
              name-pad (mapreduceprop flat-records #'length #'max :name)
              num-pad (mapreduceprop flat-records
                                     (lambda (n) (length (number-to-string n)))
                                     #'max :num)
              max-depth (tree-depth tree))
        (with-current-buffer buf
          (erase-buffer)
          (setq-local outline-regexp "[0-9]+ *\\(?:• \\)*")
          (outline-minor-mode 1)
          (use-local-map (make-sparse-keymap))
          (local-set-key "TAB" #'outline-toggle-children)
          (local-set-key "\t" #'outline-toggle-children)
          (local-set-key (kbd "<backtab>") #'outline-show-subtree)
          (local-set-key (kbd "C-<iso-lefttab>")
                         (eval `(cmd! (if current-prefix-arg
                                          (outline-show-all)
                                        (outline-hide-sublevels (+ ,num-pad 2))))))
          (insert
           (propertize
            (concat (string-pad "#" num-pad) " "
                    (string-pad "Confpkg"
                                (+ name-pad (* 2 max-depth) -3))
                    (format " Load Time (Σ=%.3fs)\n"
                            (plist-get (cdr (assoc 'self tree)) :total)))
            'face '(:inherit (tab-bar-tab bold) :extend t :underline t)))
          (dolist (record (if sort-p
                              (sort-records-by-time tree)
                            (reverse (cdr tree))))
            (unless (eq (car record) 'self)
              (print-record record)))
          (set-buffer-modified-p nil)
          (goto-char (point-min)))
        (pop-to-buffer buf)))))

(provide 'config-confpkg-timings)

(confpkg-create-record 'doom-pre-config (float-time (time-subtract (current-time) before-init-time)))
(confpkg-start-record 'config)
(confpkg-create-record 'config-defered 0.0 'config)
(confpkg-create-record 'set-hooks 0.0 'config-defered)
(confpkg-create-record 'load-hooks 0.0 'config-defered)
(confpkg-create-record 'requires 0.0 'root)

;;:------------------------
;;; Personal Information
;;:------------------------

(confpkg-with-record '("load: personal-information" config)
(setq user-full-name "VitalyR"
      user-mail-address "vitalyankh@gmail.com")
(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry nil) ; default is 7200 (2h)

(provide 'config-personal-information))

;;:------------------------
;;; !Pkg Workaround
;;:------------------------

;; This block defines `force-debug'.

(confpkg-with-record '("load: pkg-workaround" config)
;;(package! consult-recoll)
;; (package! emacs-nerd-icons
;;   :recipe (:type git
;;            :host github
;;            :repo "rainstormstudio/emacs-nerd-icons"
;;            :files ("*.el" "data")))
(setq tab-line-separator (propertize "▶" 'face  '(inherit 'tab-line-tab-inactive  :foreground  "SeaGreen3")))

;; TODO
(setq org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl")))

;; I like line numbers
(use-package! global-display-line-numbers-mode
  :hook (prog-mode . global-display-line-numbers-mode)
  :hook (fundamental-mode . global-display-line-numbers-mode)
  )

(defadvice copilot--complete-post-command (around intercept activate)
  (condition-case err
      ad-do-it
    ;; Let the debugger run
    ((debug error) (signal (car err) (cdr err)))))

(setq straight-repository-branch "develop")

(setq package-native-compile t
      ;; byte+native-compile t
      )
;; TODO to make eglot show function's signature within one line
(setq eldoc-echo-area-use-multiline-p nil)

;; debug vertico--exhibit error
(defun force-debug (func &rest args)
  (condition-case e
      (apply func args)
    ((debug error) (signal (car e) (cdr e)))))

(advice-add #'vertico--exhibit :around #'force-debug)
;; make org latex preview images' baseline the same as the text
;; (defun my-org-latex-preview-advice (beg end &rest _args)
;;   (let* ((ov (car (overlays-in beg end)))
;;          (img (cdr (overlay-get ov 'display)))
;;          (new-img (plist-put img :ascent 90)))
;;     (overlay-put ov 'display (cons 'image new-img))))
;; (advice-add 'org--make-preview-overlay
;;             :after #'my-org-latex-preview-advice)
(use-package org-xlatex
  :after (org)
  :hook (org-mode . org-xlatex-mode))
;; (use-package emacs-nerd-icons
;;   :custom
;;   (emacs-nerd-icons-font-family "FiraCode Nerd Font") ;; The Nerd Font you want to use in GUI
;;   )

(provide 'config--pkg-workaround))

;;:------------------------
;;; Better defaults
;;:------------------------

(confpkg-with-record '("load: better-defaults" config)
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 ;; x-stretch-cursor t
 )                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      ;; TODO: find an equivalent for Emacs?
      ;; evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      ;; scroll-preserve-screen-position 'always     ; Don't have `point' jump around
      scroll-margin 2                             ; It's nice to maintain a little margin
      warning-minimum-level  :error               ; Who cares about warnings?
      word-wrap-by-category t                     ; Different languages live together happily
      mouse-drag-and-drop-region-cross-program t
      mouse-1-click-follows-link nil
      server-raise-frame nil
      display-time-default-load-average nil)      ; I don't think I've ever found this useful

;; (display-time-mode 1)                             ; Enable time in the mode-line

;; (unless (string-match-p "^Power N/A" (battery))   ; On laptops...
;;   (display-battery-mode 1))                       ; it's nice to know how much power you have

(global-visual-line-mode 1)                       ; Wrap lines at window edge, not at 80th character: my screen is wide enough!

(scroll-bar-mode)                                 ; I like scroll bars

;; Unset C-z which is bound to =suspend-frame= by default
(keymap-global-unset "C-z")

(global-subword-mode 1)                           ; Iterate through CamelCase words
;; (push  '(alpha-background . 95) default-frame-alist)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq-default custom-file (expand-file-name ".custom.el" doom-user-dir))
(when (file-exists-p custom-file)
  (load custom-file))
;; (map! :map evil-window-map
;;       "SPC" #'rotate-layout
;;       ;; Navigation
;;       "<left>"     #'evil-window-left
;;       "<down>"     #'evil-window-down
;;       "<up>"       #'evil-window-up
;;       "<right>"    #'evil-window-right
;;       ;; Swapping windows
;;       "C-<left>"       #'+evil/window-move-left
;;       "C-<down>"       #'+evil/window-move-down
;;       "C-<up>"         #'+evil/window-move-up
;;       "C-<right>"      #'+evil/window-move-right)
;; (setq-default major-mode 'org-mode)

(provide 'config-better-defaults))

;;:------------------------
;;; Doom
;;:------------------------

(confpkg-with-record '("load: doom" config)
(setq doom-font (font-spec :family "JetBrains Mono" :size 14)
      doom-big-font (font-spec :family "JetBrains Mono" :size 28)
      ;;doom-variable-pitch-font (font-spec :family "American Typewriter" :size 21)
      doom-variable-pitch-font (font-spec :family "CMU Typewriter Text" :size 17)
      doom-serif-font (font-spec :family "CMU Typewriter Text" :size 17))

(add-hook 'after-init-hook (lambda ()
                             ;; Emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿
                             (set-fontset-font "fontset-default" 'symbol "Apple Color Emoji" nil 'prepend)
                             (set-fontset-font "fontset-default" 'symbol "Symbola" nil 'append)
                             (set-fontset-font "fontset-default" 'symbol "Noto Color Emoji" nil 'prepend)
                             (set-fontset-font "fontset-default" 'symbol "Liberation Mono" nil 'prepend)
                             (set-fontset-font "fontset-default" 'symbol "Noto Sans Symbols2" nil 'prepend)
                             (set-fontset-font "fontset-default" 'symbol "Segoe UI Emoji" nil 'append)
                             (set-fontset-font "fontset-default" 'symbol "Free Serif" nil 'prepend)
                             (set-fontset-font "fontset-default" 'symbol "twemoji" nil 'prepend)
                             ;;(set-face-attribute 'default nil :font "Droid Sans Mono")
                             ;; East Asia: 你好, 早晨, こんにちは, 안녕하세요
                             (set-fontset-font "fontset-default" 'han "LXGW WenKai" nil 'prepend)
                             (set-fontset-font "fontset-default" 'kana "LXGW WenKai" nil 'prepend)
                             (set-fontset-font "fontset-default" 'hangul "LXGW WenKai" nil 'prepend)
                             (set-fontset-font "fontset-default" 'cjk-misc "Noto Serif CJK SC Regular" nil 'prepend)
                             ;; Cyrillic: Привет, Здравствуйте, Здраво, Здравейте
                             (set-fontset-font "fontset-default" 'cyrillic "Noto Serif" nil 'prepend)
                             ))
;; (defvar required-fonts '("JetBrainsMono.*" "Overpass" "JuliaMono" "IBM Plex Mono" "Merriweather" "Alegreya"))

;; (defvar available-fonts
;;   (delete-dups (or (font-family-list)
;;                    (split-string (shell-command-to-string "fc-list : family")
;;                                  "[,\n]"))))

;; (defvar missing-fonts
;;   (delq nil (mapcar
;;              (lambda (font)
;;                (unless (delq nil (mapcar (lambda (f)
;;                                            (string-match-p (format "^%s$" font) f))
;;                                          available-fonts))
;;                  font))
;;              required-fonts)))

;; (if missing-fonts
;;     (pp-to-string
;;      `(unless noninteractive
;;         (add-hook! 'doom-init-ui-hook
;;           (run-at-time nil nil
;;                        (lambda ()
;;                          (message "%s missing the following fonts: %s"
;;                                   (propertize "Warning!" 'face '(bold warning))
;;                                   (mapconcat (lambda (font)
;;                                                (propertize font 'face 'font-lock-variable-name-face))
;;                                              ',missing-fonts
;;                                              ", "))
;;                          (sleep-for 0.5))))))
;;   ";; No missing fonts detected")
nil
(setq
 doom-themes-enable-bold nil ; if nil, bold is universally disabled
 doom-themes-enable-italic t ; if nil, italics is universally disabled
 doom-themes-padded-modeline t
 doom-themes-treemacs-enable-variable-pitch nil)

(setq doom-theme 'doom-one-light)
(delq! t custom-theme-load-path)
(remove-hook 'window-setup-hook #'doom-init-theme-h)
(add-hook 'after-init-hook #'doom-init-theme-h 'append)
;; (setq display-line-numbers-type 'relative)
;; (evil-define-command +evil-buffer-org-new (count file)
;;   "Creates a new ORG buffer replacing the current window, optionally
;;    editing a certain FILE"
;;   :repeat nil
;;   (interactive "P<f>")
;;   (if file
;;       (evil-edit file)
;;     (let ((buffer (generate-new-buffer "*new org*")))
;;       (set-window-buffer nil buffer)
;;       (with-current-buffer buffer
;;         (org-mode)
;;         (setq-local doom-real-buffer-p t)))))

;; (map! :leader
;;       (:prefix "b"
;;        :desc "New empty Org buffer" "o" #'+evil-buffer-org-new))

(provide 'config-doom))

;;:------------------------
;;; Dashboard
;;:------------------------

;; This block defines `+doom-dashboard-tweak',
;; `+doom-dashboard-benchmark-line',
;; `+doom-dashboard-setup-modified-keymap', and
;; `doom-dashboard-draw-ascii-emacs-banner-fn'.

(confpkg-with-record '("load: dashboard" config)
(defun doom-dashboard-draw-ascii-emacs-banner-fn ()
  (let* ((banner
          '(",---.,-.-.,---.,---.,---."
            "|---'| | |,---||    `---."
            "`---'` ' '`---^`---'`---'"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))
(unless (display-graphic-p) ; for some reason this messes up the graphical splash screen atm
  (setq +doom-dashboard-ascii-banner-fn #'doom-dashboard-draw-ascii-emacs-banner-fn))
(defun +doom-dashboard-setup-modified-keymap ()
  (setq +doom-dashboard-mode-map (make-sparse-keymap))
  (map! :map +doom-dashboard-mode-map
        :desc "Find file" :ng "f" #'find-file
        :desc "Recent files" :ng "r" #'consult-recent-file
        :desc "Config dir" :ng "C" #'doom/open-private-config
        :desc "Open config.org" :ng "c" (cmd! (find-file (expand-file-name "config.org" doom-user-dir)))
        :desc "Open org-mode root" :ng "O" (cmd! (find-file (expand-file-name "lisp/org/" doom-user-dir)))
        :desc "Open dotfile" :ng "." (cmd! (doom-project-find-file "~/.config/"))
        :desc "Notes (roam)" :ng "n" #'org-roam-node-find
        :desc "Switch buffer" :ng "b" #'+vertico/switch-workspace-buffer
        :desc "Switch buffers (all)" :ng "B" #'consult-buffer
        :desc "IBuffer" :ng "i" #'ibuffer
        :desc "Previous buffer" :ng "p" #'previous-buffer
        :desc "Set theme" :ng "t" #'consult-theme
        :desc "Quit" :ng "Q" #'save-buffers-kill-terminal
        :desc "Show keybindings" :ng "h" (cmd! (which-key-show-keymap '+doom-dashboard-mode-map))))

(add-transient-hook! #'+doom-dashboard-mode (+doom-dashboard-setup-modified-keymap))
(add-transient-hook! #'+doom-dashboard-mode :append (+doom-dashboard-setup-modified-keymap))
(add-hook! 'doom-init-ui-hook :append (+doom-dashboard-setup-modified-keymap))
(map! :leader :desc "Dashboard" "d" #'+doom-dashboard/open)
(defun +doom-dashboard-benchmark-line ()
  "Insert the load time line."
  (when doom-init-time
    (insert
     "\n\n"
     (propertize
      (+doom-dashboard--center
       +doom-dashboard--width
       (doom-display-benchmark-h 'return))
      'face 'doom-dashboard-loaded))))
(remove-hook 'doom-after-init-hook #'doom-display-benchmark-h)
(setq +doom-dashboard-functions
      (list #'doom-dashboard-widget-banner
            #'+doom-dashboard-benchmark-line
            #'splash-phrase-dashboard-insert))
(defun +doom-dashboard-tweak (&optional _)
  (with-current-buffer (get-buffer +doom-dashboard-name)
    (setq-local line-spacing 0.2
                mode-line-format nil
                ;; evil-normal-state-cursor (list nil)
)))
(add-hook '+doom-dashboard-mode-hook #'+doom-dashboard-tweak)
(add-hook 'doom-after-init-hook #'+doom-dashboard-tweak 1)
(setq +doom-dashboard-name "► Doom"
      doom-fallback-buffer-name +doom-dashboard-name)
(add-hook 'window-size-change-functions #'fancy-splash-apply-appropriate-image)
(add-hook 'doom-load-theme-hook #'fancy-splash-apply-appropriate-image)

(provide 'config-dashboard))

;;:------------------------
;;; fancy-splash
;;:------------------------

;; This block defines `fancy-splash-apply-appropriate-image',
;; `fancy-splash-get-appropriate-size',
;; `fancy-splash-switch-template', `fancy-splash-clear-cache',
;; `fancy-splash-ensure-theme-images-exist',
;; `fancy-splash-generate-all-images', `fancy-splash-generate-image',
;; `fancy-splash-filename', `fancy-splash-sizes',
;; `fancy-splash-cache-dir', `fancy-splash-check-buffer',
;; `fancy-splash-template-colours', `fancy-splash-image-template', and
;; `fancy-splash-image-directory'.

(confpkg-with-record '("load: fancy-splash" config)
(defvar fancy-splash-image-directory
  (expand-file-name "misc/splash-images/" doom-user-dir)
  "Directory in which to look for splash image templates.")

(defvar fancy-splash-image-template
  (expand-file-name "emacs-e-template.svg" fancy-splash-image-directory)
  "Default template svg used for the splash image.
Colours are substituted as per `fancy-splash-template-colours'.")
(defvar fancy-splash-template-colours
  '(("#111112" :face default   :attr :foreground)
    ("#8b8c8d" :face shadow)
    ("#eeeeef" :face default   :attr :background)
    ("#e66100" :face highlight :attr :background)
    ("#1c71d8" :face font-lock-keyword-face)
    ("#f5c211" :face font-lock-type-face)
    ("#813d9c" :face font-lock-constant-face)
    ("#865e3c" :face font-lock-function-name-face)
    ("#2ec27e" :face font-lock-string-face)
    ("#c01c28" :face error)
    ("#000001" :face ansi-color-black)
    ("#ff0000" :face ansi-color-red)
    ("#ff00ff" :face ansi-color-magenta)
    ("#00ff00" :face ansi-color-green)
    ("#ffff00" :face ansi-color-yellow)
    ("#0000ff" :face ansi-color-blue)
    ("#00ffff" :face ansi-color-cyan)
    ("#fffffe" :face ansi-color-white))
  "Alist of colour-replacement plists.
Each plist is of the form (\"$placeholder\" :doom-color 'key :face 'face).
If the current theme is a doom theme :doom-color will be used,
otherwise the colour will be face foreground.")
(defun fancy-splash-check-buffer ()
  "Check the current SVG buffer for bad colours."
  (interactive)
  (when (eq major-mode 'image-mode)
    (xml-mode))
  (when (and (featurep 'rainbow-mode)
             (not (bound-and-true-p rainbow-mode)))
    (rainbow-mode 1))
  (let* ((colours (mapcar #'car fancy-splash-template-colours))
         (colourise-hex
          (lambda (hex)
            (propertize
             hex
             'face `((:foreground
                      ,(if (< 0.5
                              (cl-destructuring-bind (r g b) (x-color-values hex)
                                ;; Values taken from `rainbow-color-luminance'
                                (/ (+ (* .2126 r) (* .7152 g) (* .0722 b))
                                   (* 256 255 1.0))))
                           "white" "black")
                      (:background ,hex))))))
         (cn 96)
         (colour-menu-entries
          (mapcar
           (lambda (colour)
             (cl-incf cn)
             (cons cn
                   (cons
                    (substring-no-properties colour)
                    (format " (%s) %s %s"
                            (propertize (char-to-string cn)
                                        'face 'font-lock-keyword-face)
                            (funcall colourise-hex colour)
                            (propertize
                             (symbol-name
                              (plist-get
                               (cdr (assoc colour fancy-splash-template-colours))
                               :face))
                             'face 'shadow)))))
           colours))
         (colour-menu-template
          (format
           "Colour %%s is unexpected! Should this be one of the following?\n
%s
 %s to ignore
 %s to quit"
           (mapconcat
            #'cddr
            colour-menu-entries
            "\n")
           (propertize "SPC" 'face 'font-lock-keyword-face)
           (propertize "ESC" 'face 'font-lock-keyword-face)))
         (colour-menu-choice-keys
          (append (mapcar #'car colour-menu-entries)
                  (list ?\s)))
         (buf (get-buffer-create "*fancy-splash-lint-colours-popup*"))
         (good-colour-p
          (lambda (colour)
            (or (assoc colour fancy-splash-template-colours)
                ;; Check if greyscale
                (or (and (= (length colour) 4)
                         (= (aref colour 1)   ; r
                            (aref colour 2)   ; g
                            (aref colour 3))) ; b
                    (and (= (length colour) 7)
                         (string= (substring colour 1 3)       ; rr =
                                  (substring colour 3 5))      ; gg
                         (string= (substring colour 3 5)       ; gg =
                                  (substring colour 5 7))))))) ; bb
         (prompt-to-replace
          (lambda (target)
            (with-current-buffer buf
              (erase-buffer)
              (insert (format colour-menu-template
                              (funcall colourise-hex target)))
              (setq-local cursor-type nil)
              (set-buffer-modified-p nil)
              (goto-char (point-min)))
            (save-window-excursion
              (pop-to-buffer buf)
              (fit-window-to-buffer (get-buffer-window buf))
              (car (alist-get
                    (read-char-choice
                     (format "Select replacement, %s-%s or SPC: "
                             (char-to-string (caar colour-menu-entries))
                             (char-to-string (caar (last colour-menu-entries))))
                     colour-menu-choice-keys)
                    colour-menu-entries))))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "#[0-9A-Fa-f]\\{6\\}\\|#[0-9A-Fa-f]\\{3\\}" nil t)
        (recenter)
        (let* ((colour (match-string 0))
               (replacement (and (not (funcall good-colour-p colour))
                                 (funcall prompt-to-replace colour))))
          (when replacement
            (replace-match replacement t t))))
      (message "Done"))))
(defvar fancy-splash-cache-dir (expand-file-name "theme-splashes/" doom-cache-dir))
(defvar fancy-splash-sizes
  `((:height 300 :min-height 50 :padding (0 . 2))
    (:height 250 :min-height 42 :padding (2 . 4))
    (:height 200 :min-height 35 :padding (3 . 3))
    (:height 150 :min-height 28 :padding (3 . 3))
    (:height 100 :min-height 20 :padding (2 . 2))
    (:height 75  :min-height 15 :padding (2 . 1))
    (:height 50  :min-height 10 :padding (1 . 0))
    (:height 1   :min-height 0  :padding (0 . 0)))
  "List of plists specifying image sizing states.
Each plist should have the following properties:
- :height, the height of the image
- :min-height, the minimum `frame-height' for image
- :padding, a `+doom-dashboard-banner-padding' (top . bottom) padding
  specification to apply
Optionally, each plist may set the following two properties:
- :template, a non-default template file
- :file, a file to use instead of template")
(defun fancy-splash-filename (theme template height)
  "Get the file name for the splash image with THEME and of HEIGHT."
  (expand-file-name (format "%s-%s-%d.svg" theme (file-name-base template) height) fancy-splash-cache-dir))
(defun fancy-splash-generate-image (template height)
  "Create a themed image from TEMPLATE of HEIGHT.
The theming is performed using `fancy-splash-template-colours'
and the current theme."
  (with-temp-buffer
    (insert-file-contents template)
    (goto-char (point-min))
    (if (re-search-forward "$height" nil t)
        (replace-match (number-to-string height) t t)
      (if (re-search-forward "height=\"100\\(?:\\.0[0-9]*\\)?\"" nil t)
          (progn
            (replace-match (format "height=\"%s\"" height) t t)
            (goto-char (point-min))
            (when (re-search-forward "\\([ \t\n]\\)width=\"[\\.0-9]+\"[ \t\n]*" nil t)
              (replace-match "\\1")))
        (warn "Warning! fancy splash template: neither $height nor height=100 not found in %s" template)))
    (dolist (substitution fancy-splash-template-colours)
      (goto-char (point-min))
      (let* ((replacement-colour
              (face-attribute (plist-get (cdr substitution) :face)
                              (or (plist-get (cdr substitution) :attr) :foreground)
                              nil 'default))
             (replacement-hex
              (if (string-prefix-p "#" replacement-colour)
                  replacement-colour
                (apply 'format "#%02x%02x%02x"
                       (mapcar (lambda (c) (ash c -8))
                               (color-values replacement-colour))))))
        (while (search-forward (car substitution) nil t)
          (replace-match replacement-hex nil nil))))
    (unless (file-exists-p fancy-splash-cache-dir)
      (make-directory fancy-splash-cache-dir t))
    (let ((inhibit-message t))
      (write-region nil nil (fancy-splash-filename (car custom-enabled-themes) template height)))))
(defun fancy-splash-generate-all-images ()
  "Perform `fancy-splash-generate-image' in bulk."
  (dolist (size fancy-splash-sizes)
    (unless (plist-get size :file)
      (fancy-splash-generate-image
       (or (plist-get size :template)
           fancy-splash-image-template)
       (plist-get size :height)))))
(defun fancy-splash-ensure-theme-images-exist (&optional height)
  "Ensure that the relevant images exist.
Use the image of HEIGHT to check, defaulting to the height of the first
specification in `fancy-splash-sizes'. If that file does not exist for
the current theme, `fancy-splash-generate-all-images' is called. "
  (unless (file-exists-p
           (fancy-splash-filename
            (car custom-enabled-themes)
            fancy-splash-image-template
            (or height (plist-get (car fancy-splash-sizes) :height))))
    (fancy-splash-generate-all-images)))
(defun fancy-splash-clear-cache (&optional delete-files)
  "Clear all cached fancy splash images.
Optionally delete all cache files and regenerate the currently relevant set."
  (interactive (list t))
  (dolist (size fancy-splash-sizes)
    (unless (plist-get size :file)
      (let ((image-file
             (fancy-splash-filename
              (car custom-enabled-themes)
              (or (plist-get size :template)
                  fancy-splash-image-template)
              (plist-get size :height))))
        (image-flush (create-image image-file) t))))
  (message "Fancy splash image cache cleared!")
  (when delete-files
    (delete-directory fancy-splash-cache-dir t)
    (fancy-splash-generate-all-images)
    (message "Fancy splash images cache deleted!")))
(defun fancy-splash-switch-template ()
  "Switch the template used for the fancy splash image."
  (interactive)
  (let ((new (completing-read
              "Splash template: "
              (mapcar
               (lambda (template)
                 (replace-regexp-in-string "-template\\.svg$" "" template))
               (directory-files fancy-splash-image-directory nil "-template\\.svg\\'"))
              nil t)))
    (setq fancy-splash-image-template
          (expand-file-name (concat new "-template.svg") fancy-splash-image-directory))
    (fancy-splash-clear-cache)
    (message "") ; Clear message from `fancy-splash-clear-cache'.
    (setq fancy-splash--last-size nil)
    (fancy-splash-apply-appropriate-image)))
(defun fancy-splash-get-appropriate-size ()
  "Find the firt `fancy-splash-sizes' with min-height of at least frame height."
  (let ((height (frame-height)))
    (cl-some (lambda (size) (when (>= height (plist-get size :min-height)) size))
             fancy-splash-sizes)))
(setq fancy-splash--last-size nil)
(setq fancy-splash--last-theme nil)

(defun fancy-splash-apply-appropriate-image (&rest _)
  "Ensure the appropriate splash image is applied to the dashboard.
This function's signature is \"&rest _\" to allow it to be used
in hooks that call functions with arguments."
  (let ((appropriate-size (fancy-splash-get-appropriate-size)))
    (unless (and (equal appropriate-size fancy-splash--last-size)
                 (equal (car custom-enabled-themes) fancy-splash--last-theme))
      (unless (plist-get appropriate-size :file)
        (fancy-splash-ensure-theme-images-exist (plist-get appropriate-size :height)))
      (setq fancy-splash-image
            (or (plist-get appropriate-size :file)
                (fancy-splash-filename (car custom-enabled-themes)
                                       fancy-splash-image-template
                                       (plist-get appropriate-size :height)))
            +doom-dashboard-banner-padding (plist-get appropriate-size :padding)
            fancy-splash--last-size appropriate-size
            fancy-splash--last-theme (car custom-enabled-themes))
      (+doom-dashboard-reload))))

(provide 'fancy-splash))

;;:------------------------
;;; splash-phrases
;;:------------------------

;; This block defines `splash-phrase-dashboard-insert',
;; `splash-phrase-dashboard-formatted', `splash-phrase',
;; `splash-phrase-get-from-file', `splash-phrase--cached-lines',
;; `splash-phrase-select-set', `splash-phrase-set-random-set',
;; `splash-phrase-set', `splash-phrase-sources', and
;; `splash-phrase-source-folder'.

(confpkg-with-record '("load: splash-phrases" config)
(defvar splash-phrase-source-folder
  (expand-file-name "misc/splash-phrases" doom-user-dir)
  "A folder of text files with a fun phrase on each line.")
(defvar splash-phrase-sources
  (let* ((files (directory-files splash-phrase-source-folder nil "\\.txt\\'"))
         (sets (delete-dups (mapcar
                             (lambda (file)
                               (replace-regexp-in-string "\\(?:-[0-9]+-\\w+\\)?\\.txt" "" file))
                             files))))
    (mapcar (lambda (sset)
              (cons sset
                    (delq nil (mapcar
                               (lambda (file)
                                 (when (string-match-p (regexp-quote sset) file)
                                   file))
                               files))))
            sets))
  "A list of cons giving the phrase set name, and a list of files which contain phrase components.")
(defvar splash-phrase-set
  (nth (random (length splash-phrase-sources)) (mapcar #'car splash-phrase-sources))
  "The default phrase set. See `splash-phrase-sources'.")
(defun splash-phrase-set-random-set ()
  "Set a new random splash phrase set."
  (interactive)
  (setq splash-phrase-set
        (nth (random (1- (length splash-phrase-sources)))
             (cl-set-difference (mapcar #'car splash-phrase-sources) (list splash-phrase-set))))
  (+doom-dashboard-reload t))

(defun splash-phrase-select-set ()
  "Select a specific splash phrase set."
  (interactive)
  (setq splash-phrase-set (completing-read "Phrase set: " (mapcar #'car splash-phrase-sources)))
  (+doom-dashboard-reload t))
(defvar splash-phrase--cached-lines nil)
(defun splash-phrase-get-from-file (file)
  "Fetch a random line from FILE."
  (let ((lines (or (cdr (assoc file splash-phrase--cached-lines))
                   (cdar (push (cons file
                                     (with-temp-buffer
                                       (insert-file-contents (expand-file-name file splash-phrase-source-folder))
                                       (split-string (string-trim (buffer-string)) "\n")))
                               splash-phrase--cached-lines)))))
    (nth (random (length lines)) lines)))
(defun splash-phrase (&optional set)
  "Construct a splash phrase from SET. See `splash-phrase-sources'."
  (mapconcat
   #'splash-phrase-get-from-file
   (cdr (assoc (or set splash-phrase-set) splash-phrase-sources))
   " "))
(defun splash-phrase-dashboard-formatted ()
  "Get a splash phrase, flow it over multiple lines as needed, and fontify it."
  (mapconcat
   (lambda (line)
     (+doom-dashboard--center
      +doom-dashboard--width
      (with-temp-buffer
        (insert-text-button
         line
         'action
         (lambda (_) (+doom-dashboard-reload t))
         'face 'doom-dashboard-menu-title
         'mouse-face 'doom-dashboard-menu-title
         'help-echo "Random phrase"
         'follow-link t)
        (buffer-string))))
   (split-string
    (with-temp-buffer
      (insert (splash-phrase))
      (setq fill-column (min 70 (/ (* 2 (window-width)) 3)))
      (fill-region (point-min) (point-max))
      (buffer-string))
    "\n")
   "\n"))
(defun splash-phrase-dashboard-insert ()
  "Insert the splash phrase surrounded by newlines."
  (insert "\n" (splash-phrase-dashboard-formatted) "\n"))

(provide 'splash-phrases))

;;:------------------------
;;; Mouse
;;:------------------------

(confpkg-with-record '("load: mouse" config)
(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)
(setq mouse-yank-at-point nil)
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-large-scroll-height 60
      pixel-scroll-precision-interpolation-factor 30.0)
(setq
 mouse-drag-and-drop-region-cross-program t
 mouse-drag-and-drop-region t)

(provide 'config-mouse))

;;:------------------------
;;; Frame title
;;:------------------------

(confpkg-with-record '("load: frame-title" config)
(setq frame-title-format
      '(""
        (:eval
         (if (string-match-p (regexp-quote (or (bound-and-true-p org-roam-directory) "\u0000"))
                             (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?\s buffer-file-name))
           "%b"))
        (:eval
         (when-let ((project-name (and (featurep 'projectile) (projectile-project-name))))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

(provide 'config-frame-title))

;;:------------------------
;;; Emacs daemon setup
;;:------------------------

;; This block defines `greedily-do-daemon-setup'.

(confpkg-with-record '("load: emacs-daemon-setup" config)
(defun greedily-do-daemon-setup ()
  (require 'org)
  (when (require 'mu4e nil t)
    (setq mu4e-confirm-quit t)
    (setq +mu4e-lock-greedy t)
    (setq +mu4e-lock-relaxed t)
    (when (+mu4e-lock-available t)
      (mu4e--start)))
  (when (require 'elfeed nil t)
    (run-at-time nil (* 8 60 60) #'elfeed-update)))

(when (daemonp)
  (add-hook 'emacs-startup-hook #'greedily-do-daemon-setup)
  (add-hook! 'server-after-make-frame-hook
    (unless (string-match-p "\\*draft\\|\\*stdin\\|emacs-everywhere" (buffer-name))
      (switch-to-buffer +doom-dashboard-name))))

(provide 'config-emacs-daemon-setup))

;;:------------------------
;;; Setup script prompt
;;:------------------------

(confpkg-with-record '("load: setup-script-prompt" config)
;; (if (file-exists-p "setup.sh")
;;     (if (string-empty-p (string-trim (with-temp-buffer (insert-file-contents "setup.sh") (buffer-string)) "#!/usr/bin/env bash"))
;;         (message ";; Setup script is empty")
;;       (message ";; Detected content in the setup script")
;;       (pp-to-string
;;        `(unless noninteractive
;;           (defun +config-run-setup ()
;;             (when (yes-or-no-p (format "%s The setup script has content. Check and run the script?"
;;                                        (propertize "Warning!" 'face '(bold warning))))
;;               (find-file (expand-file-name "setup.sh" doom-user-dir))
;;               (when (yes-or-no-p "Would you like to run this script?")
;;                 (async-shell-command "./setup.sh"))))
;;           (add-hook! 'doom-init-ui-hook
;;             (run-at-time nil nil #'+config-run-setup)))))
;;   (message ";; setup.sh did not exist during tangle. Tangle again.")
;;   (pp-to-string
;;    `(unless noninteractive
;;       (add-hook! 'doom-init-ui-hook #'+literate-tangle-async-h))))
nil

(provide 'config-setup-script-prompt))

;;:------------------------
;;; !Pkg avy
;;:------------------------

(confpkg-with-record '("load: pkg-avy" config)
;; Avy: Colemak layout not detected (ErgoDox not mentioned in dmesg).

(provide 'config--pkg-avy))

;;:------------------------
;;; !Pkg Rotate
;;:------------------------

(confpkg-with-record '("load: pkg-rotate" config)
(use-package rotate
  :defer t
  :init
  (keymap-set window-prefix-map "SPC" 'rotate-layout)
  (keymap-set window-prefix-map "r" 'rotate-window)
  (keymap-set window-prefix-map "m v" 'rotate:main-vertical)
  (keymap-set window-prefix-map "m h" 'rotate:main-horizontal)
  (keymap-set window-prefix-map "e v" 'rotate:even-vertical)
  (keymap-set window-prefix-map "e h" 'rotate:even-horizontal)
  )

(provide 'config--pkg-rotate))

;;:------------------------
;;; !Pkg emacs-everywhere
;;:------------------------

(confpkg-with-record '("load: pkg-emacs-everywhere" config)
;; (package! emacs-everywhere :recipe (:host github :repo "tecosaur/emacs-everywhere"))
;; (unpin! emacs-everywhere)
;; (use-package! emacs-everywhere
;;   :if (daemonp)
;;   :config
;;   (require 'spell-fu)
;;   (setq emacs-everywhere-major-mode-function #'org-mode
;;         emacs-everywhere-frame-name-format "Edit ∷ %s — %s")
;;   (defadvice! emacs-everywhere-raise-frame ()
;;     :after #'emacs-everywhere-set-frame-name
;;     (setq emacs-everywhere-frame-name (format emacs-everywhere-frame-name-format
;;                                 (emacs-everywhere-app-class emacs-everywhere-current-app)
;;                                 (truncate-string-to-width
;;                                  (emacs-everywhere-app-title emacs-everywhere-current-app)
;;                                  45 nil nil "…")))
;;     ;; need to wait till frame refresh happen before really set
;;     (run-with-timer 0.1 nil #'emacs-everywhere-raise-frame-1))
;;   (defun emacs-everywhere-raise-frame-1 ()
;;     (call-process "wmctrl" nil nil nil "-a" emacs-everywhere-frame-name)))

(provide 'config--pkg-emacs-everywhere))

;;:------------------------
;;; !Pkg which-key
;;:------------------------

(confpkg-with-record '("load: pkg-which-key" config)
(setq which-key-idle-delay 0.5) ;; I need the help, I really do
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

(provide 'config--pkg-which-key))

;;:------------------------
;;; Multi-mode abbrev
;;:------------------------

;; This block defines `+abbrev-file-name'.

(confpkg-with-record '("load: multi-mode-abbrev" config)
(add-hook 'doom-first-buffer-hook
          (defun +abbrev-file-name ()
            (setq-default abbrev-mode t)
            (setq abbrev-file-name (expand-file-name "abbrev.el" doom-user-dir))))

(provide 'config-multi-mode-abbrev))

;;:------------------------
;;; Ace-window
;;:------------------------

(confpkg-with-record '("load: ace-window" config)
(use-package ace-window
  :config
  (keymap-global-set "M-o" 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(provide 'config-ace-window))

;;:------------------------
;;; !Pkg embark
;;:------------------------

(confpkg-with-record '("load: pkg-embark" config)
(use-package embark
  :config
  (keymap-global-set "C-," 'embark-act)
  ;; TODO: this doesn't work
  ;; file one wayland to open a file with external programs
  ;; (map minibuffer-local-map
  ;;       :n "o" #'embark-open-externally
  ;;       )
  )

(provide 'config--pkg-embark))

;;:------------------------
;;; !Pkg VLF
;;:------------------------

;; This block defines `+vlf-isearch-wrap', `+vlf-last-chunk-or-end',
;; `+vlf-next-chunk-or-start', and `+vlf-update-linum'.

(confpkg-with-record '("load: pkg-vlf" config)
(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write
  vlf-search vlf-occur vlf-follow vlf-ediff vlf
  :commands vlf vlf-mode
  :init
  (defadvice! +files--ask-about-large-file-vlf (size op-type filename offer-raw)
  "Like `files--ask-user-about-large-file', but with support for `vlf'."
  :override #'files--ask-user-about-large-file
  (if (eq vlf-application 'dont-ask)
      (progn (vlf filename) (error ""))
    (let ((prompt (format "File %s is large (%s), really %s?"
                          (file-name-nondirectory filename)
                          (funcall byte-count-to-string-function size) op-type)))
      (if (not offer-raw)
          (if (y-or-n-p prompt) nil 'abort)
        (let ((choice
               (car
                (read-multiple-choice
                 prompt '((?y "yes")
                          (?n "no")
                          (?l "literally")
                          (?v "vlf"))
                 (files--ask-user-about-large-file-help-text
                  op-type (funcall byte-count-to-string-function size))))))
          (cond ((eq choice ?y) nil)
                ((eq choice ?l) 'raw)
                ((eq choice ?v)
                 (vlf filename)
                 (error ""))
                (t 'abort)))))))
  :config
  (advice-remove 'abort-if-file-too-large #'ad-Advice-abort-if-file-too-large)
  (defvar-local +vlf-cumulative-linenum '((0 . 0))
  "An alist keeping track of the cumulative line number.")

(defun +vlf-update-linum ()
  "Update the line number offset."
  (let ((linenum-offset (alist-get vlf-start-pos +vlf-cumulative-linenum)))
    (setq display-line-numbers-offset (or linenum-offset 0))
    (when (and linenum-offset (not (assq vlf-end-pos +vlf-cumulative-linenum)))
      (push (cons vlf-end-pos (+ linenum-offset
                                 (count-lines (point-min) (point-max))))
            +vlf-cumulative-linenum))))

(add-hook 'vlf-after-chunk-update-hook #'+vlf-update-linum)

;; Since this only works with absolute line numbers, let's make sure we use them.
(add-hook! 'vlf-mode-hook (setq-local display-line-numbers t))
  (defun +vlf-next-chunk-or-start ()
  (if (= vlf-file-size vlf-end-pos)
      (vlf-jump-to-chunk 1)
    (vlf-next-batch 1))
  (goto-char (point-min)))

(defun +vlf-last-chunk-or-end ()
  (if (= 0 vlf-start-pos)
      (vlf-end-of-file)
    (vlf-prev-batch 1))
  (goto-char (point-max)))

(defun +vlf-isearch-wrap ()
  (if isearch-forward
      (+vlf-next-chunk-or-start)
    (+vlf-last-chunk-or-end)))

(add-hook! 'vlf-mode-hook (setq-local isearch-wrap-function #'+vlf-isearch-wrap)))

(provide 'config--pkg-vlf))

;;:------------------------
;;; !Pkg Straight
;;:------------------------

(confpkg-with-record '("load: pkg-straight" config)
(use-package straight
  :defer t
  :config
  (add-to-list 'straight-built-in-pseudo-packages 'org)
  )

(provide 'config--pkg-straight))

;;:------------------------
;;; !Pkg Eros
;;:------------------------

(confpkg-with-record '("load: pkg-eros" config)
(setq eros-eval-result-prefix "⟹ ") ; default =>

(provide 'config--pkg-eros))

;;:------------------------
;;; !Pkg Meow
;;:------------------------

;; This block defines `meow-setup' and `meow/setup-leader'.

(confpkg-with-record '("load: pkg-meow" config)
(defun meow/setup-leader ()
  (map! :leader
        "?" #'meow-cheatsheet
        "/" #'meow-keypad-describe-key
        "1" #'meow-digit-argument
        "2" #'meow-digit-argument
        "3" #'meow-digit-argument
        "4" #'meow-digit-argument
        "5" #'meow-digit-argument
        "6" #'meow-digit-argument
        "7" #'meow-digit-argument
        "8" #'meow-digit-argument
        "9" #'meow-digit-argument
        "0" #'meow-digit-argument))

(defun meow/setup-doom-keybindings()
  (map! :map meow-normal-state-keymap
        doom-leader-key doom-leader-map)
  (map! :map meow-motion-state-keymap
        doom-leader-key doom-leader-map)
  (map! :map meow-beacon-state-keymap
        doom-leader-key nil)
  (meow/setup-leader)
  )
(defun set-useful-keybindings()
  (keymap-global-set "M-j" 'kmacro-start-macro-or-insert-counter)
  (keymap-global-set "M-k" 'kmacro-end-or-call-macro)
  ;;for doom emacs buffer management
  (map! :leader
        ;; make doom-leader-buffer-map alive
        (:prefix-map ("b" . "buffer")
         :desc "Toggle narrowing"            "-"   #'doom/toggle-narrow-buffer
         :desc "Previous buffer"             "["   #'previous-buffer
         :desc "Next buffer"                 "]"   #'next-buffer
         (:when (modulep! :ui workspaces)
           :desc "Switch workspace buffer"    "b" #'persp-switch-to-buffer
           :desc "Switch buffer"              "B" #'switch-to-buffer)
         (:unless (modulep! :ui workspaces)
           :desc "Switch buffer"               "b"   #'switch-to-buffer)
         :desc "Clone buffer"                "c"   #'clone-indirect-buffer
         :desc "Clone buffer other window"   "C"   #'clone-indirect-buffer-other-window
         :desc "Kill buffer"                 "d"   #'kill-current-buffer
         :desc "ibuffer"                     "i"   #'ibuffer
         :desc "Kill buffer"                 "k"   #'kill-current-buffer
         :desc "Kill all buffers"            "K"   #'doom/kill-all-buffers
         :desc "Switch to last buffer"       "l"   #'evil-switch-to-windows-last-buffer
         :desc "Set bookmark"                "m"   #'bookmark-set
         :desc "Delete bookmark"             "M"   #'bookmark-delete
         :desc "Next buffer"                 "n"   #'next-buffer
         :desc "New empty buffer"            "N"   #'+default/new-buffer
         :desc "Kill other buffers"          "O"   #'doom/kill-other-buffers
         :desc "Previous buffer"             "p"   #'previous-buffer
         :desc "Revert buffer"               "r"   #'revert-buffer
         :desc "Save buffer"                 "s"   #'basic-save-buffer
         ;;:desc "Save all buffers"            "S"   #'evil-write-all
         :desc "Save buffer as root"         "u"   #'doom/sudo-save-buffer
         :desc "Pop up scratch buffer"       "x"   #'doom/open-scratch-buffer
         :desc "Switch to scratch buffer"    "X"   #'doom/switch-to-scratch-buffer
         :desc "Bury buffer"                 "z"   #'bury-buffer
         :desc "Kill buried buffers"         "Z"   #'doom/kill-buried-buffers)
        )
  )

(defun meow-setup ()
  (set-useful-keybindings)
  (meow/setup-doom-keybindings)
  ;; for doom emacs
  ;;(add-to-list 'meow-keymap-alist (cons 'leader doom-leader-map))
  ;;(meow-normal-define-key (cons "SPC" doom-leader-map))
  ;;(meow-motion-overwrite-define-key (cons "SPC" doom-leader-map))
  (map!
   (:when (modulep! :ui workspaces)
     :n "C-t"   #'+workspace/new
     :n "C-S-t" #'+workspace/display
     :g "M-1"   #'+workspace/switch-to-0
     :g "M-2"   #'+workspace/switch-to-1
     :g "M-3"   #'+workspace/switch-to-2
     :g "M-4"   #'+workspace/switch-to-3
     :g "M-5"   #'+workspace/switch-to-4
     :g "M-6"   #'+workspace/switch-to-5
     :g "M-7"   #'+workspace/switch-to-6
     :g "M-8"   #'+workspace/switch-to-7
     :g "M-9"   #'+workspace/switch-to-8
     :g "M-0"   #'+workspace/switch-to-final
     ))
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   )

  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("/" . meow-visit)
   '("a" . meow-append)
   '("A" . meow-join)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-symbol)
   '("E" . meow-mark-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("P" . pop-to-mark-command)
   '("q" . meow-open-below)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-grab)
   '("w" . meow-next-word)
   '("x" . meow-line)
   '("X" . meow-line-expand)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("\\" . quoted-insert)
   ;;TODO: for C-[ translate to ESC, this doesn't work
   ;;'("ESC" . meow-insert-exit)
   ;;'("<escape>" . meow-insert-exit)
   )

  (setq meow-expand-exclude-mode-list nil)
  (setq meow-expand-hint-remove-delay 1024)
  ;; TODO: replace define-key with keymap-set
  (define-key input-decode-map (kbd "C-[") [control-bracketleft])
  (define-key meow-insert-state-keymap [control-bracketleft] 'meow-insert-exit)
  ;; (meow-define-keys 'insert
  ;;   '("ESC" . meow-insert-exit))

  ;; (keymap-set input-decode-map "C-[" 'meow-insert-exit)
  ;; (keymap-set meow-insert-state-keymap "C-[" 'meow-insert-exit)

  (setq meow-use-clipboard t
        meow-visit-sanitize-completion nil
        meow-expand-exclude-mode-list nil
        meow-expand-hint-remove-delay 1024
        )
  (add-to-list 'meow-mode-state-list '(hexl-mode . normal))
  )
(use-package meow
  :config
  (require 'meow)
  (meow-setup)
  (meow-global-mode 1)
  )

(provide 'config--pkg-meow))

;;:------------------------
;;; !Pkg Copilot
;;:------------------------

(confpkg-with-record '("load: pkg-copilot" config)
;; (customize-set-variable 'copilot-enable-predicates '((lambda () (eq (meow--current-state) 'insert))))
;; (defun my-tab ()
;;   (interactive)
;;   (or (copilot-accept-completion)
;;       (company-indent-or-complete-common nil)))
;; complete by copilot first, then company-mode

;; disabled for now
;; (use-package! copilot
;;   :after company
;;   :hook (prog-mode . copilot-mode)
;;   :hook (text-mode . copilot-mode)
;;   :bind (("C-TAB" . 'copilot-accept-completion-by-word)
;;          ("C-<tab>" . 'copilot-accept-completion-by-word)
;;          :map company-active-map
;;          ("<tab>" . 'my-tab)
;;          ("TAB" . 'my-tab)
;;          :map company-mode-map
;;          ("<tab>" . 'my-tab)
;;          ("TAB" . 'my-tab)))


(provide 'config--pkg-copilot))

;;:------------------------
;;; !Pkg Consult
;;:------------------------

(confpkg-with-record '("load: pkg-consult" config)
(after! consult
  (set-face-attribute 'consult-file nil :inherit 'consult-buffer)
  (setf (plist-get (alist-get 'perl consult-async-split-styles-alist) :initial) ";"))

(provide 'config--pkg-consult))

;;:------------------------
;;; !Pkg Magit
;;:------------------------

;; This block defines `+org-commit-message-template',
;; `+magit-fill-in-commit-template', `+magit-default-forge-remote',
;; and `+magit-project-commit-templates-alist'.

(confpkg-with-record '("load: pkg-magit" config)
(defvar +magit-project-commit-templates-alist nil
  "Alist of toplevel dirs and template hf strings/functions.")
(after! magit
  (defvar +magit-default-forge-remote "gitea@git.tecosaur.net:tec/%s.git"
  "Format string that fills out to a remote from the repo name.
Set to nil to disable this functionality.")
(defadvice! +magit-remote-add--streamline-forge-a (args)
  :filter-args #'magit-remote-add
  (interactive
   (or (and +magit-default-forge-remote
            (not (magit-list-remotes))
            (eq (read-char-choice
                 (format "Setup %s remote? [y/n]: "
                         (replace-regexp-in-string
                          "\\`\\(?:[^@]+@\\|https://\\)\\([^:/]+\\)[:/].*\\'" "\\1"
                          +magit-default-forge-remote))
                 '(?y ?n))
                ?y)
            (let* ((default-name
                     (subst-char-in-string ?\s ?-
                                           (file-name-nondirectory
                                            (directory-file-name (doom-project-root)))))
                   (name (read-string "Name: " default-name)))
              (list "origin" (format +magit-default-forge-remote name)
                    (transient-args 'magit-remote))))
       (let ((origin (magit-get "remote.origin.url"))
             (remote (magit-read-string-ns "Remote name"))
             (gh-user (magit-get "github.user")))
         (when (and (equal remote gh-user)
                    (string-match "\\`https://github\\.com/\\([^/]+\\)/\\([^/]+\\)\\.git\\'"
                                  origin)
                    (not (string= (match-string 1 origin) gh-user)))
           (setq origin (replace-regexp-in-string
                         "\\`https://github\\.com/" "git@github.com:"
                         origin)))
         (list remote
               (magit-read-url
                "Remote url"
                (and origin
                     (string-match "\\([^:/]+\\)/[^/]+\\(\\.git\\)?\\'" origin)
                     (replace-match remote t t origin 1)))
               (transient-args 'magit-remote)))))
  args)
(defun +magit-fill-in-commit-template ()
  "Insert template from `+magit-fill-in-commit-template' if applicable."
  (when-let ((template (and (save-excursion (goto-char (point-min)) (string-match-p "\\`\\s-*$" (thing-at-point 'line)))
                            (cdr (assoc (file-name-base (directory-file-name (magit-toplevel)))
                                        +magit-project-commit-templates-alist)))))
    (goto-char (point-min))
    (insert (if (stringp template) template (funcall template)))
    (goto-char (point-min))
    (end-of-line)))
(add-hook 'git-commit-setup-hook #'+magit-fill-in-commit-template 90)
(defun +org-commit-message-template ()
  "Create a skeleton for an Org commit message based on the staged diff."
  (let (change-data last-file file-changes temp-point)
    (with-temp-buffer
      (apply #'call-process magit-git-executable
             nil t nil
             (append
              magit-git-global-arguments
              (list "diff" "--cached")))
      (goto-char (point-min))
      (while (re-search-forward "^@@\\|^\\+\\+\\+ b/" nil t)
        (if (looking-back "^\\+\\+\\+ b/" (line-beginning-position))
            (progn
              (push (list last-file file-changes) change-data)
              (setq last-file (buffer-substring-no-properties (point) (line-end-position))
                    file-changes nil))
          (setq temp-point (line-beginning-position))
          (re-search-forward "^\\+\\|^-" nil t)
          (end-of-line)
          (cond
           ((string-match-p "\\.el$" last-file)
            (when (re-search-backward "^\\(?:[+-]? *\\|@@[ +-\\d,]+@@ \\)(\\(?:cl-\\)?\\(?:defun\\|defvar\\|defmacro\\|defcustom\\)" temp-point t)
              (re-search-forward "\\(?:cl-\\)?\\(?:defun\\|defvar\\|defmacro\\|defcustom\\) " nil t)
              (add-to-list 'file-changes (buffer-substring-no-properties (point) (forward-symbol 1)))))
           ((string-match-p "\\.org$" last-file)
            (when (re-search-backward "^[+-]\\*+ \\|^@@[ +-\\d,]+@@ \\*+ " temp-point t)
              (re-search-forward "@@ \\*+ " nil t)
              (add-to-list 'file-changes (buffer-substring-no-properties (point) (line-end-position)))))))))
    (push (list last-file file-changes) change-data)
    (setq change-data (delete '(nil nil) change-data))
    (concat
     (if (= 1 (length change-data))
         (replace-regexp-in-string "^.*/\\|.[a-z]+$" "" (caar change-data))
       "?")
     ": \n\n"
     (mapconcat
      (lambda (file-changes)
        (if (cadr file-changes)
            (format "* %s (%s): "
                    (car file-changes)
                    (mapconcat #'identity (cadr file-changes) ", "))
          (format "* %s: " (car file-changes))))
      change-data
      "\n\n"))))

(add-to-list '+magit-project-commit-templates-alist (cons "org" #'+org-commit-message-template)))

(provide 'config--pkg-magit))

;;:------------------------
;;; !Pkg Smerge
;;:------------------------

;; This block defines `smerge-repeatedly'.

(confpkg-with-record '("load: pkg-smerge" config)
(defun smerge-repeatedly ()
  "Perform smerge actions again and again"
  (interactive)
  (smerge-mode 1)
  (smerge-transient))
(after! transient
  (transient-define-prefix smerge-transient ()
    [["Move"
      ("n" "next" (lambda () (interactive) (ignore-errors (smerge-next)) (smerge-repeatedly)))
      ("p" "previous" (lambda () (interactive) (ignore-errors (smerge-prev)) (smerge-repeatedly)))]
     ["Keep"
      ("b" "base" (lambda () (interactive) (ignore-errors (smerge-keep-base)) (smerge-repeatedly)))
      ("u" "upper" (lambda () (interactive) (ignore-errors (smerge-keep-upper)) (smerge-repeatedly)))
      ("l" "lower" (lambda () (interactive) (ignore-errors (smerge-keep-lower)) (smerge-repeatedly)))
      ("a" "all" (lambda () (interactive) (ignore-errors (smerge-keep-all)) (smerge-repeatedly)))
      ("RET" "current" (lambda () (interactive) (ignore-errors (smerge-keep-current)) (smerge-repeatedly)))]
     ["Diff"
      ("<" "upper/base" (lambda () (interactive) (ignore-errors (smerge-diff-base-upper)) (smerge-repeatedly)))
      ("=" "upper/lower" (lambda () (interactive) (ignore-errors (smerge-diff-upper-lower)) (smerge-repeatedly)))
      (">" "base/lower" (lambda () (interactive) (ignore-errors (smerge-diff-base-lower)) (smerge-repeatedly)))
      ("R" "refine" (lambda () (interactive) (ignore-errors (smerge-refine)) (smerge-repeatedly)))
      ("E" "ediff" (lambda () (interactive) (ignore-errors (smerge-ediff)) (smerge-repeatedly)))]
     ["Other"
      ("c" "combine" (lambda () (interactive) (ignore-errors (smerge-combine-with-next)) (smerge-repeatedly)))
      ("r" "resolve" (lambda () (interactive) (ignore-errors (smerge-resolve)) (smerge-repeatedly)))
      ("k" "kill current" (lambda () (interactive) (ignore-errors (smerge-kill-current)) (smerge-repeatedly)))
      ("q" "quit" (lambda () (interactive) (smerge-auto-leave)))]]))

(provide 'config--pkg-smerge))

;;:------------------------
;;; !Pkg Company
;;:------------------------

(confpkg-with-record '("load: pkg-company" config)
(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  ;; (add-hook 'evil-normal-state-entry-hook #'company-abort)
) ;; make aborting less annoying.
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)
(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    ;; company-ispell
    company-files
    company-yasnippet))
(set-company-backend! 'ess-r-mode '(company-R-args company-R-objects company-dabbrev-code :separate))

(provide 'config--pkg-company))

;;:------------------------
;;; !Pkg Projectile
;;:------------------------

;; This block defines `projectile-ignored-project-function'.

(confpkg-with-record '("load: pkg-projectile" config)
(setq projectile-ignored-projects
      (list "~/" "/tmp" (expand-file-name "straight/repos" doom-local-dir)))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))

(provide 'config--pkg-projectile))

;;:------------------------
;;; !Pkg Ispell
;;:------------------------

(confpkg-with-record '("load: pkg-ispell" config)
(setq ispell-dictionary "en-custom")
(setq ispell-personal-dictionary
      (expand-file-name "misc/ispell_personal" doom-user-dir))

(provide 'config--pkg-ispell))

;;:------------------------
;;; TRAMP
;;:------------------------

(confpkg-with-record '("load: tramp" config)
(after! tramp
  (setenv "SHELL" "/bin/bash")
  (setq tramp-shell-prompt-pattern "\\(?:^\\|\n\\|\x0d\\)[^]#$%>\n]*#?[]#$%>] *\\(\e\\[[0-9;]*[a-zA-Z] *\\)*")) ;; default + 
(after! tramp
  (appendq! tramp-remote-path
            '("~/.guix-profile/bin" "~/.guix-profile/sbin"
              "/run/current-system/profile/bin"
              "/run/current-system/profile/sbin")))

(provide 'config-tramp))

;;:------------------------
;;; !Pkg AAS
;;:------------------------

(confpkg-with-record '("load: pkg-aas" config)
(use-package! aas
  :commands aas-mode)

(provide 'config--pkg-aas))

;;:------------------------
;;; !Pkg Screenshot
;;:------------------------

(confpkg-with-record '("load: pkg-screenshot" config)
(use-package! screenshot
  :defer t
  :config (setq screenshot-upload-fn "upload %s 2>/dev/null"))

(provide 'config--pkg-screenshot))

;;:------------------------
;;; !Pkg etrace
;;:------------------------

(confpkg-with-record '("load: pkg-etrace" config)
(use-package! etrace
  :after elp)

(provide 'config--pkg-etrace))

;;:------------------------
;;; !Pkg YASnippet
;;:------------------------

(confpkg-with-record '("load: pkg-yasnippet" config)
(setq yas-triggers-in-field t)

(provide 'config--pkg-yasnippet))

;;:------------------------
;;; !Pkg String Inflection
;;:------------------------

(confpkg-with-record '("load: pkg-string-inflection" config)
(use-package! string-inflection
  :commands (string-inflection-all-cycle
             string-inflection-toggle
             string-inflection-camelcase
             string-inflection-lower-camelcase
             string-inflection-kebab-case
             string-inflection-underscore
             string-inflection-capital-underscore
             string-inflection-upcase)
  :init
  (map! :leader :prefix ("c~" . "naming convention")
        :desc "cycle" "~" #'string-inflection-all-cycle
        :desc "toggle" "t" #'string-inflection-toggle
        :desc "CamelCase" "c" #'string-inflection-camelcase
        :desc "downCase" "d" #'string-inflection-lower-camelcase
        :desc "kebab-case" "k" #'string-inflection-kebab-case
        :desc "under_score" "_" #'string-inflection-underscore
        :desc "Upper_Score" "u" #'string-inflection-capital-underscore
        :desc "UP_CASE" "U" #'string-inflection-upcase)
  ;; (after! evil
  ;;   (evil-define-operator evil-operator-string-inflection (beg end _type)
  ;;     "Define a new evil operator that cycles symbol casing."
  ;;     :move-point nil
  ;;     (interactive "<R>")
  ;;     (string-inflection-all-cycle)
  ;;     (setq evil-repeat-info '([?g ?~])))
  ;;   (define-key evil-normal-state-map (kbd "g~") 'evil-operator-string-inflection))
)

(provide 'config--pkg-string-inflection))

;;:------------------------
;;; !Pkg SmartParens
;;:------------------------

(confpkg-with-record '("load: pkg-smartparens" config)
(sp-local-pair
 '(org-mode)
 "<<" ">>"
 :actions '(insert))

(provide 'config--pkg-smartparens))

;;:------------------------
;;; !Pkg Info colors
;;:------------------------

(confpkg-with-record '("load: pkg-info-colors" config)
(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(provide 'config--pkg-info-colors))

;;:------------------------
;;; !Pkg Theme magic
;;:------------------------

(confpkg-with-record '("load: pkg-theme-magic" config)
(use-package! theme-magic
  :commands theme-magic-from-emacs
  :config
  (defadvice! theme-magic--auto-extract-16-doom-colors ()
    :override #'theme-magic--auto-extract-16-colors
    (list
     (face-attribute 'default :background)
     (doom-color 'error)
     (doom-color 'success)
     (doom-color 'type)
     (doom-color 'keywords)
     (doom-color 'constants)
     (doom-color 'functions)
     (face-attribute 'default :foreground)
     (face-attribute 'shadow :foreground)
     (doom-blend 'base8 'error 0.1)
     (doom-blend 'base8 'success 0.1)
     (doom-blend 'base8 'type 0.1)
     (doom-blend 'base8 'keywords 0.1)
     (doom-blend 'base8 'constants 0.1)
     (doom-blend 'base8 'functions 0.1)
     (face-attribute 'default :foreground))))

(provide 'config--pkg-theme-magic))

;;:------------------------
;;; !Pkg simple-comment-markup
;;:------------------------

(confpkg-with-record '("load: pkg-simple-comment-markup" config)
;; (package! simple-comment-markup :recipe (:host github :repo "tecosaur/simple-comment-markup"))
;; (use-package! simple-comment-markup
;;   :hook (prog-mode . simple-comment-markup-mode)
;;   :config
;;   (setq simple-comment-markup-set '(org markdown-code)))

(provide 'config--pkg-simple-comment-markup))

;;:------------------------
;;; !Pkg emojify
;;:------------------------

;; This block defines `emojify--replace-text-with-emoji' and
;; `emojify-disabled-emojis'.

(confpkg-with-record '("load: pkg-emojify" config)
(setq emojify-emoji-set "twemoji-v2")
(defvar emojify-disabled-emojis
  '(;; Org
    "◼" "☑" "☸" "⚙" "⏩" "⏪" "⬆" "⬇" "❓" "↔"
    ;; Terminal powerline
    "✔"
    ;; Box drawing
    "▶" "◀"
    ;; I just want to see this as text
    "©" "™")
  "Characters that should never be affected by `emojify-mode'.")

(defadvice! emojify-delete-from-data ()
  "Ensure `emojify-disabled-emojis' don't appear in `emojify-emojis'."
  :after #'emojify-set-emoji-data
  (dolist (emoji emojify-disabled-emojis)
    (remhash emoji emojify-emojis)))
(defun emojify--replace-text-with-emoji (orig-fn emoji text buffer start end &optional target)
  "Modify `emojify--propertize-text-for-emoji' to replace ascii/github emoticons with unicode emojis, on the fly."
  (if (or (not emoticon-to-emoji) (= 1 (length text)))
      (funcall orig-fn emoji text buffer start end target)
    (delete-region start end)
    (insert (ht-get emoji "unicode"))))

(define-minor-mode emoticon-to-emoji
  "Write ascii/gh emojis, and have them converted to unicode live."
  :global nil
  :init-value nil
  (if emoticon-to-emoji
      (progn
        (setq-local emojify-emoji-styles '(ascii github unicode))
        (advice-add 'emojify--propertize-text-for-emoji :around #'emojify--replace-text-with-emoji)
        (unless emojify-mode
          (emojify-turn-on-emojify-mode)))
    (setq-local emojify-emoji-styles (default-value 'emojify-emoji-styles))
    (advice-remove 'emojify--propertize-text-for-emoji #'emojify--replace-text-with-emoji)))
;; (add-hook! '(mu4e-compose-mode org-msg-edit-mode circe-channel-mode) (emoticon-to-emoji 1))

(provide 'config--pkg-emojify))

;;:------------------------
;;; !Pkg Doom modeline
;;:------------------------

;; This block defines `doom-modeline-update-pdf-pages',
;; `+doom-modeline-micro-clock', `+doom-modeline--clock-text',
;; `+doom-modeline-clock-text-format',
;; `+doom-modeline-micro-clock--cache',
;; `+doom-modeline-micro-clock-inverse-size',
;; `+doom-modeline-micro-clock-minute-resolution', `micro-clock-svg',
;; `micro-clock-minute-hand-ratio', `micro-clock-hour-hand-ratio', and
;; `doom-modeline-conditional-buffer-encoding'.

(confpkg-with-record '("hook: pkg-doom-modeline" set-hooks)
  (with-eval-after-load 'doom-modeline
(confpkg-with-record '("load: pkg-doom-modeline" load-hooks)
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))
;; (setq doom-modeline-height 25)
(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)
(defvar micro-clock-hour-hand-ratio 0.45
  "Length of the hour hand as a proportion of the radius.")
(defvar micro-clock-minute-hand-ratio 0.7
  "Length of the minute hand as a proportion of the radius.")

(defun micro-clock-svg (hour minute radius color)
  "Construct an SVG clock showing the time HOUR:MINUTE.
The clock will be of the specified RADIUS and COLOR."
  (let ((hour-x (* radius (sin (* (- 6 hour (/ minute 60.0)) (/ float-pi 6)))
                   micro-clock-hour-hand-ratio))
        (hour-y (* radius (cos (* (- 6 hour (/ minute 60.0)) (/ float-pi 6)))
                   micro-clock-hour-hand-ratio))
        (minute-x (* radius (sin (* (- 30 minute) (/ float-pi 30)))
                     micro-clock-minute-hand-ratio))
        (minute-y (* radius (cos (* (- 30 minute) (/ float-pi 30)))
                     micro-clock-minute-hand-ratio))
        (svg (svg-create (* 2 radius) (* 2 radius) :stroke color)))
    (svg-circle svg radius radius (1- radius) :fill "none" :stroke-width 2)
    (svg-circle svg radius radius 1 :fill color :stroke "none")
    (svg-line svg radius radius (+ radius hour-x) (+ radius hour-y)
              :stroke-width 2)
    (svg-line svg radius radius (+ radius minute-x) (+ radius minute-y)
              :stroke-width 1.5)
    svg))
(require 'svg)

(defvar +doom-modeline-micro-clock-minute-resolution 1
  "The clock will be updated every this many minutes, truncating.")
(defvar +doom-modeline-micro-clock-inverse-size 4.8
  "The size of the clock, as an inverse proportion to the mode line height.")

(defvar +doom-modeline-micro-clock--cache nil)

(defvar +doom-modeline-clock-text-format "%c")

(defun +doom-modeline--clock-text (&optional _window _object _pos)
  (format-time-string +doom-modeline-clock-text-format))

(defun +doom-modeline-micro-clock ()
  "Return a string containing an current analogue clock."
  (cdr
   (if (equal (truncate (float-time)
                        (* +doom-modeline-micro-clock-minute-resolution 60))
              (car +doom-modeline-micro-clock--cache))
       +doom-modeline-micro-clock--cache
     (setq +doom-modeline-micro-clock--cache
           (cons (truncate (float-time)
                           (* +doom-modeline-micro-clock-minute-resolution 60))
                 (with-temp-buffer
                   (svg-insert-image
                    (micro-clock-svg
                     (string-to-number (format-time-string "%-I")) ; hour
                     (* (truncate (string-to-number (format-time-string "%-M"))
                                  +doom-modeline-micro-clock-minute-resolution)
                        +doom-modeline-micro-clock-minute-resolution) ; minute
                     (/ doom-modeline-height +doom-modeline-micro-clock-inverse-size) ; radius
                     "currentColor"))
                   (propertize
                    " "
                    'display
                    (append (get-text-property 0 'display (buffer-string))
                            '(:ascent center))
                    'face 'doom-modeline-time
                    'help-echo #'+doom-modeline--clock-text)))))))
(doom-modeline-def-segment time
  (when (and doom-modeline-time
             (bound-and-true-p display-time-mode)
             (not doom-modeline--limited-width-p))
    (concat
     doom-modeline-spc
     (when doom-modeline-time-icon
       (concat
        (+doom-modeline-micro-clock)
        (and (or doom-modeline-icon doom-modeline-unicode-fallback)
             doom-modeline-spc)))
     (propertize display-time-string
                 'face (doom-modeline-face 'doom-modeline-time)))))
(doom-modeline-def-segment buffer-name
  "Display the current buffer's name, without any other information."
  (concat
   doom-modeline-spc
   (doom-modeline--buffer-name)))

(doom-modeline-def-segment pdf-icon
  "PDF icon from all-the-icons."
  (concat
   doom-modeline-spc
   (doom-modeline-icon 'octicon "file-pdf" nil nil
                       :face (if (doom-modeline--active)
                                 'all-the-icons-red
                               'mode-line-inactive)
                       :v-adjust 0.02)))

(defun doom-modeline-update-pdf-pages ()
  "Update PDF pages."
  (setq doom-modeline--pdf-pages
        (let ((current-page-str (number-to-string (eval `(pdf-view-current-page))))
              (total-page-str (number-to-string (pdf-cache-number-of-pages))))
          (concat
           (propertize
            (concat (make-string (- (length total-page-str) (length current-page-str)) ? )
                    " P" current-page-str)
            'face 'mode-line)
           (propertize (concat "/" total-page-str) 'face 'doom-modeline-buffer-minor-mode)))))

(doom-modeline-def-segment pdf-pages
  "Display PDF pages."
  (if (doom-modeline--active) doom-modeline--pdf-pages
    (propertize doom-modeline--pdf-pages 'face 'mode-line-inactive)))

(doom-modeline-def-modeline 'pdf
  '(bar window-number pdf-pages pdf-icon buffer-name)
  '(misc-info matches major-mode process vcs))

(provide 'config--pkg-doom-modeline))))

;;:------------------------
;;; !Pkg Keycast
;;:------------------------

(confpkg-with-record '("load: pkg-keycast" config)
(use-package! keycast
  :commands keycast-mode
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" mode-line-keycast " ")))
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (remove '("" mode-line-keycast " ") global-mode-string))))
  (custom-set-faces!
    '(keycast-command :inherit doom-modeline-debug
                      :height 0.9)
    '(keycast-key :inherit custom-modified
                  :height 1.1
                  :weight bold)))

(provide 'config--pkg-keycast))

;;:------------------------
;;; !Pkg Screencast
;;:------------------------

;; This block defines `gif-screencast-write-colormap'.

(confpkg-with-record '("load: pkg-screencast" config)
(use-package! gif-screencast
  :commands gif-screencast-mode
  :config
  (map! :map gif-screencast-mode-map
        :g "<f8>" #'gif-screencast-toggle-pause
        :g "<f9>" #'gif-screencast-stop)
  (setq gif-screencast-program "maim"
        gif-screencast-args `("--quality" "3" "-i" ,(string-trim-right
                                                     (shell-command-to-string
                                                      "xdotool getactivewindow")))
        gif-screencast-optimize-args '("--batch" "--optimize=3" "--usecolormap=/tmp/doom-color-theme"))
  (defun gif-screencast-write-colormap ()
    (f-write-text
     (replace-regexp-in-string
      "\n+" "\n"
      (mapconcat (lambda (c) (if (listp (cdr c))
                                 (cadr c))) doom-themes--colors "\n"))
     'utf-8
     "/tmp/doom-color-theme" ))
  (gif-screencast-write-colormap)
  (add-hook 'doom-load-theme-hook #'gif-screencast-write-colormap))

(provide 'config--pkg-screencast))

;;:------------------------
;;; !Pkg mixed pitch
;;:------------------------

;; This block defines `mixed-pitch-serif-mode', `init-mixed-pitch-h',
;; and `mixed-pitch-modes'.

(confpkg-with-record '("load: pkg-mixed-pitch" config)
(defvar mixed-pitch-modes '(org-mode LaTeX-mode markdown-mode gfm-mode Info-mode)
  "Modes that `mixed-pitch-mode' should be enabled in, but only after UI initialisation.")
(defun init-mixed-pitch-h ()
  "Hook `mixed-pitch-mode' into each mode in `mixed-pitch-modes'.
Also immediately enables `mixed-pitch-modes' if currently in one of the modes."
  (when (memq major-mode mixed-pitch-modes)
    (mixed-pitch-mode 1))
  (dolist (hook mixed-pitch-modes)
    (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode)))
(add-hook 'doom-init-ui-hook #'init-mixed-pitch-h)
(autoload #'mixed-pitch-serif-mode "mixed-pitch"
  "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch." t)

(setq! variable-pitch-serif-font (font-spec :family "Alegreya" :size 27))

(after! mixed-pitch
  (setq mixed-pitch-set-height t)
  (set-face-attribute 'variable-pitch-serif nil :font variable-pitch-serif-font)
  (defun mixed-pitch-serif-mode (&optional arg)
    "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch."
    (interactive)
    (let ((mixed-pitch-face 'variable-pitch-serif))
      (mixed-pitch-mode (or arg 'toggle)))))
(set-char-table-range composition-function-table ?f '(["\\(?:ff?[fijlt]\\)" 0 font-shape-gstring]))
(set-char-table-range composition-function-table ?T '(["\\(?:Th\\)" 0 font-shape-gstring]))

(provide 'config--pkg-mixed-pitch))

;;:------------------------
;;; Variable pitch serif font
;;:------------------------

;; This block defines `variable-pitch-serif-font' and
;; `variable-pitch-serif'.

(confpkg-with-record '("load: variable-pitch-serif-font" config)
(defface variable-pitch-serif
    '((t (:family "serif")))
    "A variable-pitch face with serifs."
    :group 'basic-faces)
(defcustom variable-pitch-serif-font (font-spec :family "serif")
  "The font face used for `variable-pitch-serif'."
  :group 'basic-faces
  :set (lambda (symbol value)
         (set-face-attribute 'variable-pitch-serif nil :font value)
         (set-default-toplevel-value symbol value)))

(provide 'config-variable-pitch-serif-font))

;;:------------------------
;;; !Pkg Marginalia
;;:------------------------

;; This block defines `+marginalia-file-size-colorful' and
;; `+marginalia--time-colorful'.

(confpkg-with-record '("load: pkg-marginalia" config)
(after! marginalia
  (setq marginalia-censor-variables nil)

  (defadvice! +marginalia--anotate-local-file-colorful (cand)
    "Just a more colourful version of `marginalia--anotate-local-file'."
    :override #'marginalia--annotate-local-file
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((+marginalia-file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((+marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12))))

  (defun +marginalia--time-colorful (time)
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun +marginalia-file-size-colorful (size)
    (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
           (color (if (< size-index 10000000) ; 10m
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange (- size-index 1)))))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))

(provide 'config--pkg-marginalia))

;;:------------------------
;;; !Pkg Centaur Tabs
;;:------------------------

(confpkg-with-record '("load: pkg-centaur-tabs" config)
(after! centaur-tabs
  (centaur-tabs-mode -1)
  (setq centaur-tabs-height 27
        centaur-tabs-set-icons t
        centaur-tabs-set-bar 'nil
        centaur-tabs-gray-out-icons 'buffer)
  (keymap-global-set "C-<"  'centaur-tabs-backward)
  (keymap-global-set "C->" 'centaur-tabs-forward)  
  )

;; (setq x-underline-at-descent-line t)

(provide 'config--pkg-centaur-tabs))

;;:------------------------
;;; !Pkg All the Icons
;;:------------------------

(confpkg-with-record '("load: pkg-all-the-icons" config)
(after! all-the-icons
  (setcdr (assoc "m" all-the-icons-extension-icon-alist)
          (cdr (assoc "matlab" all-the-icons-extension-icon-alist))))

(provide 'config--pkg-all-the-icons))

;;:------------------------
;;; !Pkg page break lines
;;:------------------------

(confpkg-with-record '("load: pkg-page-break-lines" config)
(use-package! page-break-lines
  :commands page-break-lines-mode
  :init
  (autoload 'turn-on-page-break-lines-mode "page-break-lines")
  :config
  (setq page-break-lines-max-width fill-column)
  (map! :prefix "g"
        :desc "Prev page break" :nv "[" #'backward-page
        :desc "Next page break" :nv "]" #'forward-page))

(provide 'config--pkg-page-break-lines))

;;:------------------------
;;; Writeroom
;;:------------------------

;; This block defines `+zen-nonprose-org-h', `+zen-prose-org-h',
;; `+zen-enable-mixed-pitch-mode-h', `+zen-org-starhide', and
;; `+zen-serif-p'.

(confpkg-with-record '("load: writeroom" config)
(setq +zen-text-scale 0.8)
(defvar +zen-serif-p t
  "Whether to use a serifed font with `mixed-pitch-mode'.")
(defvar +zen-org-starhide t
  "The value `org-modern-hide-stars' is set to.")

(after! writeroom-mode
  (defvar-local +zen--original-org-indent-mode-p nil)
  (defvar-local +zen--original-mixed-pitch-mode-p nil)
  (defun +zen-enable-mixed-pitch-mode-h ()
    "Enable `mixed-pitch-mode' when in `+zen-mixed-pitch-modes'."
    (when (apply #'derived-mode-p +zen-mixed-pitch-modes)
      (if writeroom-mode
          (progn
            (setq +zen--original-mixed-pitch-mode-p mixed-pitch-mode)
            (funcall (if +zen-serif-p #'mixed-pitch-serif-mode #'mixed-pitch-mode) 1))
        (funcall #'mixed-pitch-mode (if +zen--original-mixed-pitch-mode-p 1 -1)))))
  (defun +zen-prose-org-h ()
    "Reformat the current Org buffer appearance for prose."
    (when (eq major-mode 'org-mode)
      (setq display-line-numbers nil
            visual-fill-column-width 60
            org-adapt-indentation nil)
      (when (featurep 'org-modern)
        (setq-local org-modern-star '("🙘" "🙙" "🙚" "🙛")
                    ;; org-modern-star '("🙐" "🙑" "🙒" "🙓" "🙔" "🙕" "🙖" "🙗")
                    org-modern-hide-stars +zen-org-starhide)
        (org-modern-mode -1)
        (org-modern-mode 1))
      (setq
       +zen--original-org-indent-mode-p org-indent-mode)
      (org-indent-mode -1)))
  (defun +zen-nonprose-org-h ()
    "Reverse the effect of `+zen-prose-org'."
    (when (eq major-mode 'org-mode)
      (when (bound-and-true-p org-modern-mode)
        (org-modern-mode -1)
        (org-modern-mode 1))
      (when +zen--original-org-indent-mode-p (org-indent-mode 1))))
  (pushnew! writeroom--local-variables
            'display-line-numbers
            'visual-fill-column-width
            'org-adapt-indentation
            'org-modern-mode
            'org-modern-star
            'org-modern-hide-stars)
  (add-hook 'writeroom-mode-enable-hook #'+zen-prose-org-h)
  (add-hook 'writeroom-mode-disable-hook #'+zen-nonprose-org-h))

(provide 'config-writeroom))

;;:------------------------
;;; !Pkg treemacs
;;:------------------------

;; This block defines `treemacs-ignore-filter',
;; `treemacs-file-ignore-generate-regexps',
;; `treemacs-file-ignore-regexps', `treemacs-file-ignore-globs', and
;; `treemacs-file-ignore-extensions'.

(confpkg-with-record '("load: pkg-treemacs" config)
(after! treemacs
  (defvar treemacs-file-ignore-extensions '()
    "File extension which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-globs '()
    "Globs which will are transformed to `treemacs-file-ignore-regexps' which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-regexps '()
    "RegExps to be tested to ignore files, generated from `treeemacs-file-ignore-globs'")
  (defun treemacs-file-ignore-generate-regexps ()
    "Generate `treemacs-file-ignore-regexps' from `treemacs-file-ignore-globs'"
    (setq treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp treemacs-file-ignore-globs)))
  (if (equal treemacs-file-ignore-globs '()) nil (treemacs-file-ignore-generate-regexps))
  (defun treemacs-ignore-filter (file full-path)
    "Ignore files specified by `treemacs-file-ignore-extensions', and `treemacs-file-ignore-regexps'"
    (or (member (file-name-extension file) treemacs-file-ignore-extensions)
        (let ((ignore-file nil))
          (dolist (regexp treemacs-file-ignore-regexps ignore-file)
            (setq ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-filter))
(setq treemacs-file-ignore-extensions
      '(;; LaTeX
        "aux"
        "ptc"
        "fdb_latexmk"
        "fls"
        "synctex.gz"
        "toc"
        ;; LaTeX - glossary
        "glg"
        "glo"
        "gls"
        "glsdefs"
        "ist"
        "acn"
        "acr"
        "alg"
        ;; LaTeX - pgfplots
        "mw"
        ;; LaTeX - pdfx
        "pdfa.xmpi"
        ))
(setq treemacs-file-ignore-globs
      '(;; LaTeX
        "*/_minted-*"
        ;; AucTeX
        "*/.auctex-auto"
        "*/_region_.log"
        "*/_region_.tex"))
(after! (treemacs meow)
  ;; No EVIL, then I add this
  (keymap-set treemacs-mode-map "/" 'meow-visit)
  (keymap-set treemacs-mode-map "e" 'treemacs-toggle-node)
)
(use-package treemacs
  :config
  (setq
   treemacs-file-event-delay 1000
   treemacs-follow-after-init t
   treemacs-file-follow-delay 0.1
   treemacs-indentation 1
   treemacs-recenter-after-file-follow t
   treemacs-recenter-after-tag-follow t
   treemacs-text-scale -1.1
   treemacs-follow-mode t
   treemacs-width 35
   )
  (treemacs-follow-mode)
  ;;(set-popup-rule! "^ \\*Treemacs-Scoped-Buffer-* [^*]*\\*" :size 0.25 :side 'left :quit nil)

  :bind
  (:map global-map
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  )
(after! (treemacs ace-window)
  (setq aw-ignored-buffers (delete 'treemacs-mode aw-ignored-buffers))
  )

(provide 'config--pkg-treemacs))

;;:------------------------
;;; XKCD
;;:------------------------

;; This block defines `+xkcd-db-write', `+xkcd-db-list-to-plist',
;; `+xkcd-db-read-all', `+xkcd-db-read', `+xkcd-db-query', `+xkcd-db',
;; `+xkcd-db--init', `+xkcd-db--get-connection', `+xkcd-db--get',
;; `+xkcd-db--connection', `+xkcd-stored-info', `+xkcd-check-latest',
;; `+xkcd-latest-max-age', `+xkcd-find-and-view', `+xkcd-copy',
;; `+xkcd-find-and-copy', `+xkcd-fetch-info', `+xkcd-select-format',
;; and `+xkcd-select'.

(confpkg-with-record '("load: xkcd" config)
(use-package! xkcd
  :commands (xkcd-get-json
             xkcd-download xkcd-get
             ;; now for funcs from my extension of this pkg
             +xkcd-find-and-copy +xkcd-find-and-view
             +xkcd-fetch-info +xkcd-select)
  :config
  (setq xkcd-cache-dir (expand-file-name "xkcd/" doom-cache-dir)
        xkcd-cache-latest (concat xkcd-cache-dir "latest"))
  (unless (file-exists-p xkcd-cache-dir)
    (make-directory xkcd-cache-dir))
  (after! evil-snipe
    (add-to-list 'evil-snipe-disabled-modes 'xkcd-mode))
  :general (:states 'normal
            :keymaps 'xkcd-mode-map
            "<right>" #'xkcd-next
            "n"       #'xkcd-next ; evil-ish
            "<left>"  #'xkcd-prev
            "N"       #'xkcd-prev ; evil-ish
            "r"       #'xkcd-rand
            "a"       #'xkcd-rand ; because image-rotate can interfere
            "t"       #'xkcd-alt-text
            "q"       #'xkcd-kill-buffer
            "o"       #'xkcd-open-browser
            "e"       #'xkcd-open-explanation-browser
            ;; extras
            "s"       #'+xkcd-find-and-view
            "/"       #'+xkcd-find-and-view
            "y"       #'+xkcd-copy))
(after! xkcd
  (require 'emacsql-sqlite)

  (defun +xkcd-select ()
    "Prompt the user for an xkcd using `completing-read' and `+xkcd-select-format'. Return the xkcd number or nil"
    (let* (prompt-lines
           (-dummy (maphash (lambda (key xkcd-info)
                              (push (+xkcd-select-format xkcd-info) prompt-lines))
                            +xkcd-stored-info))
           (num (completing-read (format "xkcd (%s): " xkcd-latest) prompt-lines)))
      (if (equal "" num) xkcd-latest
        (string-to-number (replace-regexp-in-string "\\([0-9]+\\).*" "\\1" num)))))

  (defun +xkcd-select-format (xkcd-info)
    "Creates each completing-read line from an xkcd info plist. Must start with the xkcd number"
    (format "%-4s  %-30s %s"
            (propertize (number-to-string (plist-get xkcd-info :num))
                        'face 'counsel-key-binding)
            (plist-get xkcd-info :title)
            (propertize (plist-get xkcd-info :alt)
                        'face '(variable-pitch font-lock-comment-face))))

  (defun +xkcd-fetch-info (&optional num)
    "Fetch the parsed json info for comic NUM. Fetches latest when omitted or 0"
    (require 'xkcd)
    (when (or (not num) (= num 0))
      (+xkcd-check-latest)
      (setq num xkcd-latest))
    (let ((res (or (gethash num +xkcd-stored-info)
                   (puthash num (+xkcd-db-read num) +xkcd-stored-info))))
      (unless res
        (+xkcd-db-write
         (let* ((url (format "https://xkcd.com/%d/info.0.json" num))
                (json-assoc
                 (if (gethash num +xkcd-stored-info)
                     (gethash num +xkcd-stored-info)
                   (json-read-from-string (xkcd-get-json url num)))))
           json-assoc))
        (setq res (+xkcd-db-read num)))
      res))

  ;; since we've done this, we may as well go one little step further
  (defun +xkcd-find-and-copy ()
    "Prompt for an xkcd using `+xkcd-select' and copy url to clipboard"
    (interactive)
    (+xkcd-copy (+xkcd-select)))

  (defun +xkcd-copy (&optional num)
    "Copy a url to xkcd NUM to the clipboard"
    (interactive "i")
    (let ((num (or num xkcd-cur)))
      (gui-select-text (format "https://xkcd.com/%d" num))
      (message "xkcd.com/%d copied to clipboard" num)))

  (defun +xkcd-find-and-view ()
    "Prompt for an xkcd using `+xkcd-select' and view it"
    (interactive)
    (xkcd-get (+xkcd-select))
    (switch-to-buffer "*xkcd*"))

  (defvar +xkcd-latest-max-age (* 60 60) ; 1 hour
    "Time after which xkcd-latest should be refreshed, in seconds")

  ;; initialise `xkcd-latest' and `+xkcd-stored-info' with latest xkcd
  (add-transient-hook! '+xkcd-select
    (require 'xkcd)
    (+xkcd-fetch-info xkcd-latest)
    (setq +xkcd-stored-info (+xkcd-db-read-all)))

  (add-transient-hook! '+xkcd-fetch-info
    (xkcd-update-latest))

  (defun +xkcd-check-latest ()
    "Use value in `xkcd-cache-latest' as long as it isn't older thabn `+xkcd-latest-max-age'"
    (unless (and (file-exists-p xkcd-cache-latest)
                 (< (- (time-to-seconds (current-time))
                       (time-to-seconds (file-attribute-modification-time (file-attributes xkcd-cache-latest))))
                    +xkcd-latest-max-age))
      (let* ((out (xkcd-get-json "http://xkcd.com/info.0.json" 0))
             (json-assoc (json-read-from-string out))
             (latest (cdr (assoc 'num json-assoc))))
        (when (/= xkcd-latest latest)
          (+xkcd-db-write json-assoc)
          (with-current-buffer (find-file xkcd-cache-latest)
            (setq xkcd-latest latest)
            (erase-buffer)
            (insert (number-to-string latest))
            (save-buffer)
            (kill-buffer (current-buffer)))))
      (shell-command (format "touch %s" xkcd-cache-latest))))

  (defvar +xkcd-stored-info (make-hash-table :test 'eql)
    "Basic info on downloaded xkcds, in the form of a hashtable")

  (defadvice! xkcd-get-json--and-cache (url &optional num)
    "Fetch the Json coming from URL.
If the file NUM.json exists, use it instead.
If NUM is 0, always download from URL.
The return value is a string."
    :override #'xkcd-get-json
    (let* ((file (format "%s%d.json" xkcd-cache-dir num))
           (cached (and (file-exists-p file) (not (eq num 0))))
           (out (with-current-buffer (if cached
                                         (find-file file)
                                       (url-retrieve-synchronously url))
                  (goto-char (point-min))
                  (unless cached (re-search-forward "^$"))
                  (prog1
                      (buffer-substring-no-properties (point) (point-max))
                    (kill-buffer (current-buffer))))))
      (unless (or cached (eq num 0))
        (xkcd-cache-json num out))
      out))

  (defadvice! +xkcd-get (num)
    "Get the xkcd number NUM."
    :override 'xkcd-get
    (interactive "nEnter comic number: ")
    (xkcd-update-latest)
    (get-buffer-create "*xkcd*")
    (switch-to-buffer "*xkcd*")
    (xkcd-mode)
    (let (buffer-read-only)
      (erase-buffer)
      (setq xkcd-cur num)
      (let* ((xkcd-data (+xkcd-fetch-info num))
             (num (plist-get xkcd-data :num))
             (img (plist-get xkcd-data :img))
             (safe-title (plist-get xkcd-data :safe-title))
             (alt (plist-get xkcd-data :alt))
             title file)
        (message "Getting comic...")
        (setq file (xkcd-download img num))
        (setq title (format "%d: %s" num safe-title))
        (insert (propertize title
                            'face 'outline-1))
        (center-line)
        (insert "\n")
        (xkcd-insert-image file num)
        (if (eq xkcd-cur 0)
            (setq xkcd-cur num))
        (setq xkcd-alt alt)
        (message "%s" title))))

  (defconst +xkcd-db--sqlite-available-p
    (with-demoted-errors "+org-xkcd initialization: %S"
      (emacsql-sqlite-ensure-binary)
      t))

  (defvar +xkcd-db--connection (make-hash-table :test #'equal)
    "Database connection to +org-xkcd database.")

  (defun +xkcd-db--get ()
    "Return the sqlite db file."
    (expand-file-name "xkcd.db" xkcd-cache-dir))

  (defun +xkcd-db--get-connection ()
    "Return the database connection, if any."
    (gethash (file-truename xkcd-cache-dir)
             +xkcd-db--connection))

  (defconst +xkcd-db--table-schema
    '((xkcds
       [(num integer :unique :primary-key)
        (year        :not-null)
        (month       :not-null)
        (link        :not-null)
        (news        :not-null)
        (safe_title  :not-null)
        (title       :not-null)
        (transcript  :not-null)
        (alt         :not-null)
        (img         :not-null)])))

  (defun +xkcd-db--init (db)
    "Initialize database DB with the correct schema and user version."
    (emacsql-with-transaction db
      (pcase-dolist (`(,table . ,schema) +xkcd-db--table-schema)
        (emacsql db [:create-table $i1 $S2] table schema))))

  (defun +xkcd-db ()
    "Entrypoint to the +org-xkcd sqlite database.
Initializes and stores the database, and the database connection.
Performs a database upgrade when required."
    (unless (and (+xkcd-db--get-connection)
                 (emacsql-live-p (+xkcd-db--get-connection)))
      (let* ((db-file (+xkcd-db--get))
             (init-db (not (file-exists-p db-file))))
        (make-directory (file-name-directory db-file) t)
        (let ((conn (emacsql-sqlite db-file)))
          (set-process-query-on-exit-flag (emacsql-process conn) nil)
          (puthash (file-truename xkcd-cache-dir)
                   conn
                   +xkcd-db--connection)
          (when init-db
            (+xkcd-db--init conn)))))
    (+xkcd-db--get-connection))

  (defun +xkcd-db-query (sql &rest args)
    "Run SQL query on +org-xkcd database with ARGS.
SQL can be either the emacsql vector representation, or a string."
    (if  (stringp sql)
        (emacsql (+xkcd-db) (apply #'format sql args))
      (apply #'emacsql (+xkcd-db) sql args)))

  (defun +xkcd-db-read (num)
    (when-let ((res
                (car (+xkcd-db-query [:select * :from xkcds
                                      :where (= num $s1)]
                                     num
                                     :limit 1))))
      (+xkcd-db-list-to-plist res)))

  (defun +xkcd-db-read-all ()
    (let ((xkcd-table (make-hash-table :test 'eql :size 4000)))
      (mapcar (lambda (xkcd-info-list)
                (puthash (car xkcd-info-list) (+xkcd-db-list-to-plist xkcd-info-list) xkcd-table))
              (+xkcd-db-query [:select * :from xkcds]))
      xkcd-table))

  (defun +xkcd-db-list-to-plist (xkcd-datalist)
    `(:num ,(nth 0 xkcd-datalist)
      :year ,(nth 1 xkcd-datalist)
      :month ,(nth 2 xkcd-datalist)
      :link ,(nth 3 xkcd-datalist)
      :news ,(nth 4 xkcd-datalist)
      :safe-title ,(nth 5 xkcd-datalist)
      :title ,(nth 6 xkcd-datalist)
      :transcript ,(nth 7 xkcd-datalist)
      :alt ,(nth 8 xkcd-datalist)
      :img ,(nth 9 xkcd-datalist)))

  (defun +xkcd-db-write (data)
    (+xkcd-db-query [:insert-into xkcds
                     :values $v1]
                    (list (vector
                           (cdr (assoc 'num        data))
                           (cdr (assoc 'year       data))
                           (cdr (assoc 'month      data))
                           (cdr (assoc 'link       data))
                           (cdr (assoc 'news       data))
                           (cdr (assoc 'safe_title data))
                           (cdr (assoc 'title      data))
                           (cdr (assoc 'transcript data))
                           (cdr (assoc 'alt        data))
                           (cdr (assoc 'img        data))
                           )))))

(provide 'config-xkcd))

;;:------------------------
;;; !Pkg Selectric
;;:------------------------

(confpkg-with-record '("load: pkg-selectric" config)
(use-package! selectic-mode
  :commands selectic-mode)

(provide 'config--pkg-selectric))

;;:------------------------
;;; !Pkg Wttrin
;;:------------------------

(confpkg-with-record '("load: pkg-wttrin" config)
;; (package! wttrin :recipe (:host :local-repo "lisp/wttrin"))
;; (use-package! wttrin
;;   :commands wttrin)

(provide 'config--pkg-wttrin))

;;:------------------------
;;; !Pkg Spray
;;:------------------------

;; This block defines `spray-mode-hide-cursor'.

(confpkg-with-record '("load: pkg-spray" config)
(use-package! spray
  :commands spray-mode
  :config
  (setq spray-wpm 600
        spray-height 800)
  (defun spray-mode-hide-cursor ()
    "Hide or unhide the cursor as is appropriate."
    (if spray-mode
        (setq-local spray--last-evil-cursor-state evil-normal-state-cursor
                    evil-normal-state-cursor '(nil))
      (setq-local evil-normal-state-cursor spray--last-evil-cursor-state)))
  (add-hook 'spray-mode-hook #'spray-mode-hide-cursor)
  (map! :map spray-mode-map
        "<return>" #'spray-start/stop
        "f" #'spray-faster
        "s" #'spray-slower
        "t" #'spray-time
        "<right>" #'spray-forward-word
        "h" #'spray-forward-word
        "<left>" #'spray-backward-word
        "l" #'spray-backward-word
        "q" #'spray-quit))

(provide 'config--pkg-spray))

;;:------------------------
;;; !Pkg Elcord
;;:------------------------

(confpkg-with-record '("load: pkg-elcord" config)
(use-package! elcord
  :commands elcord-mode
  :config
  (setq elcord-use-major-mode-as-main-icon t))

(provide 'config--pkg-elcord))

;;:------------------------
;;; !Pkg Systemd
;;:------------------------

(confpkg-with-record '("load: pkg-systemd" config)
;; (package! systemd :pin "b6ae63a236605b1c5e1069f7d3afe06ae32a7bae")
;; (use-package! systemd
;;   :defer t)

(provide 'config--pkg-systemd))

;;:------------------------
;;; !Pkg Wakatime
;;:------------------------

(confpkg-with-record '("load: pkg-wakatime" config)
(use-package wakatime-mode
  :config
  (global-wakatime-mode)
  )

(provide 'config--pkg-wakatime))

;;:------------------------
;;; !Pkg Smart-input-source
;;:------------------------

(confpkg-with-record '("load: pkg-smart-input-source" config)
(use-package! sis
  ;;:hook
  ;;enable the /follow context/ and /inline region/ mode for specific buffers
  ;;(((text-mode prog-mode) . sis-context-mode)
  ;; ((text-mode prog-mode) . sis-inline-mod
  :after meow
  ;;:defer-incrementally meow
  :config
  (sis-ism-lazyman-config "1" "2" 'fcitx5)
  (add-hook 'meow-insert-exit-hook #'sis-set-english)
  (add-to-list 'sis-context-hooks 'meow-insert-exit-hook)
  ;; (defun describe-key-sis ()
  ;;   (interactive)
  ;;   (sis-set-english)
  ;;   (sis-global-respect-mode 0)
  ;;   (describe-key (help--read-key-sequence))
  ;;   (sis-global-respect-mode t))
  ;; :bind (("C-h k" . describe-key-sis))
  )

(provide 'config--pkg-smart-input-source))

;;:------------------------
;;; Use =emacs-rime=
;;:------------------------

(confpkg-with-record '("load: use--emacs-rime-" config)
;;(package! rime)
;; (use-package rime
;;   :custom
;;   (default-input-method "rime")
;;   (rime-user-data-dir "~/.config/input_method/rime")
;;   (rime-disable-predicates
;;    '(meow-normal-mode-p
;;      meow-motion-mode-p
;;      meow-keypad-mode-p
;;      meow-beacon-mode-p
;;      ))
;;   )

(provide 'config-use--emacs-rime-))

;;:------------------------
;;; Ebooks
;;:------------------------

;; This block defines `+nov-mode-setup' and
;; `doom-modeline-segment--nov-info'.

(confpkg-with-record '("load: ebooks" config)
(use-package! calibredb
  :commands calibredb
  :config
  (setq calibredb-root-dir "~/nutstore_files/Books"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (map! :map calibredb-show-mode-map
        :ne "?" #'calibredb-entry-dispatch
        :ne "o" #'calibredb-find-file
        :ne "O" #'calibredb-find-file-other-frame
        :ne "V" #'calibredb-open-file-with-default-tool
        :ne "s" #'calibredb-set-metadata-dispatch
        :ne "e" #'calibredb-export-dispatch
        :ne "q" #'calibredb-entry-quit
        :ne "." #'calibredb-open-dired
        :ne [tab] #'calibredb-toggle-view-at-point
        :ne "M-t" #'calibredb-set-metadata--tags
        :ne "M-a" #'calibredb-set-metadata--author_sort
        :ne "M-A" #'calibredb-set-metadata--authors
        :ne "M-T" #'calibredb-set-metadata--title
        :ne "M-c" #'calibredb-set-metadata--comments)
  (map! :map calibredb-search-mode-map
        :ne [mouse-3] #'calibredb-search-mouse
        :ne "RET" #'calibredb-find-file
        :ne "?" #'calibredb-dispatch
        :ne "a" #'calibredb-add
        :ne "A" #'calibredb-add-dir
        :ne "c" #'calibredb-clone
        :ne "d" #'calibredb-remove
        :ne "D" #'calibredb-remove-marked-items
        :ne "j" #'calibredb-next-entry
        :ne "k" #'calibredb-previous-entry
        :ne "l" #'calibredb-virtual-library-list
        :ne "L" #'calibredb-library-list
        :ne "n" #'calibredb-virtual-library-next
        :ne "N" #'calibredb-library-next
        :ne "p" #'calibredb-virtual-library-previous
        :ne "P" #'calibredb-library-previous
        :ne "s" #'calibredb-set-metadata-dispatch
        :ne "S" #'calibredb-switch-library
        :ne "o" #'calibredb-find-file
        :ne "O" #'calibredb-find-file-other-frame
        :ne "v" #'calibredb-view
        :ne "V" #'calibredb-open-file-with-default-tool
        :ne "." #'calibredb-open-dired
        :ne "b" #'calibredb-catalog-bib-dispatch
        :ne "e" #'calibredb-export-dispatch
        :ne "r" #'calibredb-search-refresh-and-clear-filter
        :ne "R" #'calibredb-search-clear-filter
        :ne "q" #'calibredb-search-quit
        :ne "m" #'calibredb-mark-and-forward
        :ne "f" #'calibredb-toggle-favorite-at-point
        :ne "x" #'calibredb-toggle-archive-at-point
        :ne "h" #'calibredb-toggle-highlight-at-point
        :ne "u" #'calibredb-unmark-and-forward
        :ne "i" #'calibredb-edit-annotation
        :ne "DEL" #'calibredb-unmark-and-backward
        :ne [backtab] #'calibredb-toggle-view
        :ne [tab] #'calibredb-toggle-view-at-point
        :ne "M-n" #'calibredb-show-next-entry
        :ne "M-p" #'calibredb-show-previous-entry
        :ne "/" #'calibredb-search-live-filter
        :ne "M-t" #'calibredb-set-metadata--tags
        :ne "M-a" #'calibredb-set-metadata--author_sort
        :ne "M-A" #'calibredb-set-metadata--authors
        :ne "M-T" #'calibredb-set-metadata--title
        :ne "M-c" #'calibredb-set-metadata--comments))
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (map! :map nov-mode-map
        :n "RET" #'nov-scroll-up)

  (defun doom-modeline-segment--nov-info ()
    (concat
     " "
     (propertize
      (cdr (assoc 'creator nov-metadata))
      'face 'doom-modeline-project-parent-dir)
     " "
     (cdr (assoc 'title nov-metadata))
     " "
     (propertize
      (format "%d/%d"
              (1+ nov-documents-index)
              (length nov-documents))
      'face 'doom-modeline-info)))

  (advice-add 'nov-render-title :override #'ignore)

  (defun +nov-mode-setup ()
    "Tweak nov-mode to our liking."
    (face-remap-add-relative 'variable-pitch
                             :family "Merriweather"
                             :height 1.4
                             :width 'semi-expanded)
    (face-remap-add-relative 'default :height 1.3)
    (setq-local line-spacing 0.2
                next-screen-context-lines 4
                shr-use-colors nil)
    (require 'visual-fill-column nil t)
    (setq-local visual-fill-column-center-text t
                visual-fill-column-width 81
                nov-text-width 80)
    (visual-fill-column-mode 1)
    (hl-line-mode -1)
    ;; Re-render with new display settings
    (nov-render-document)
    ;; Look up words with the dictionary.
    (add-to-list '+lookup-definition-functions #'+lookup/dictionary-definition)
    ;; Customise the mode-line to make it more minimal and relevant.
    (setq-local
     mode-line-format
     `((:eval
        (doom-modeline-segment--workspace-name))
       (:eval
        (doom-modeline-segment--window-number))
       (:eval
        (doom-modeline-segment--nov-info))
       ,(propertize
         " %P "
         'face 'doom-modeline-buffer-minor-mode)
       ,(propertize
         " "
         'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)
         'display `((space
                     :align-to
                     (- (+ right right-fringe right-margin)
                        ,(* (let ((width (doom-modeline--font-width)))
                              (or (and (= width 1) 1)
                                  (/ width (frame-char-width) 1.0)))
                            (string-width
                             (format-mode-line (cons "" '(:eval (doom-modeline-segment--major-mode))))))))))
       (:eval (doom-modeline-segment--major-mode)))))

  (add-hook 'nov-mode-hook #'+nov-mode-setup))

(provide 'config-ebooks))

;;:------------------------
;;; Calculator
;;:------------------------

;; This block defines `calc-embedded-calculator-window' and
;; `calc-embedded-trail-window'.

(confpkg-with-record '("load: calculator" config)
(use-package! calctex
  :commands calctex-mode
  :init
  (add-hook 'calc-mode-hook #'calctex-mode)
  :config
  (setq calctex-additional-latex-packages "
\\usepackage[usenames]{xcolor}
\\usepackage{soul}
\\usepackage{adjustbox}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{siunitx}
\\usepackage{cancel}
\\usepackage{mathtools}
\\usepackage{mathalpha}
\\usepackage{xparse}
\\usepackage{arevmath}"
        calctex-additional-latex-macros
        (concat calctex-additional-latex-macros
                "\n\\let\\evalto\\Rightarrow"))
  (defadvice! no-messaging-a (orig-fn &rest args)
    :around #'calctex-default-dispatching-render-process
    (let ((inhibit-message t) message-log-max)
      (apply orig-fn args)))
  ;; Fix hardcoded dvichop path (whyyyyyyy)
  (let ((vendor-folder (concat (file-truename doom-local-dir)
                               "straight/"
                               (format "build-%s" emacs-version)
                               "/calctex/vendor/")))
    (setq calctex-dvichop-sty (concat vendor-folder "texd/dvichop")
          calctex-dvichop-bin (concat vendor-folder "texd/dvichop")))
  (unless (file-exists-p calctex-dvichop-bin)
    (message "CalcTeX: Building dvichop binary")
    (let ((default-directory (file-name-directory calctex-dvichop-bin)))
      (call-process "make" nil nil nil))))
(setq calc-angle-mode 'rad  ; radians are rad
      calc-symbolic-mode t) ; keeps expressions like \sqrt{2} irrational for as long as possible
(map! :map calc-mode-map
      :after calc
      :localleader
      :desc "Embedded calc (toggle)" "e" #'calc-embedded)
(map! :map org-mode-map
      :after org
      :localleader
      :desc "Embedded calc (toggle)" "E" #'calc-embedded)
(map! :map latex-mode-map
      :after latex
      :localleader
      :desc "Embedded calc (toggle)" "e" #'calc-embedded)
(defvar calc-embedded-trail-window nil)
(defvar calc-embedded-calculator-window nil)

(defadvice! calc-embedded-with-side-pannel (&rest _)
  :after #'calc-do-embedded
  (when calc-embedded-trail-window
    (ignore-errors
      (delete-window calc-embedded-trail-window))
    (setq calc-embedded-trail-window nil))
  (when calc-embedded-calculator-window
    (ignore-errors
      (delete-window calc-embedded-calculator-window))
    (setq calc-embedded-calculator-window nil))
  (when (and calc-embedded-info
             (> (* (window-width) (window-height)) 1200))
    (let ((main-window (selected-window))
          (vertical-p (> (window-width) 80)))
      (select-window
       (setq calc-embedded-trail-window
             (if vertical-p
                 (split-window-horizontally (- (max 30 (/ (window-width) 3))))
               (split-window-vertically (- (max 8 (/ (window-height) 4)))))))
      (switch-to-buffer "*Calc Trail*")
      (select-window
       (setq calc-embedded-calculator-window
             (if vertical-p
                 (split-window-vertically -6)
               (split-window-horizontally (- (/ (window-width) 2))))))
      (switch-to-buffer "*Calculator*")
      (select-window main-window))))

(provide 'config-calculator))

;;:------------------------
;;; IRC
;;:------------------------

;; This block defines `lui-emoticons-alist', `lui-emojis-alist',
;; `lui-emoticon-to-emoji', `lui-ascii-to-emoji', `lui-org-to-irc',
;; `register-irc-auths', `auth-server-pass', `named-circe-prompt',
;; `lui-emoticons-alist', `lui-emojis-alist', `lui-emoticon-to-emoji',
;; `lui-ascii-to-emoji', `lui-org-to-irc', `register-irc-auths', and
;; `auth-server-pass'.

(confpkg-with-record '("load: irc" config)
(defun auth-server-pass (server)
  (if-let ((secret (plist-get (car (auth-source-search :host server)) :secret)))
      (if (functionp secret)
          (funcall secret) secret)
    (error "Could not fetch password for host %s" server)))

(defun register-irc-auths ()
  (require 'circe)
  (require 'dash)
  (let ((accounts (-filter (lambda (a) (string= "irc" (plist-get a :for)))
                           (auth-source-search :require '(:for) :max 10))))
    (appendq! circe-network-options
              (mapcar (lambda (entry)
                        (let* ((host (plist-get entry :host))
                               (label (or (plist-get entry :label) host))
                               (_ports (mapcar #'string-to-number
                                               (s-split "," (plist-get entry :port))))
                               (port (if (= 1 (length _ports)) (car _ports) _ports))
                               (user (plist-get entry :user))
                               (nick (or (plist-get entry :nick) user))
                               (channels (mapcar (lambda (c) (concat "#" c))
                                                 (s-split "," (plist-get entry :channels)))))
                          `(,label
                            :host ,host :port ,port :nick ,nick
                            :sasl-username ,user :sasl-password auth-server-pass
                            :channels ,channels)))
                      accounts))))
(after! circe
  (setq-default circe-use-tls t)
  (setq circe-notifications-alert-icon "/usr/share/icons/breeze/actions/24/network-connect.svg"
        lui-logging-directory (expand-file-name "irc" doom-etc-dir)
        lui-logging-file-format "{buffer}/%Y/%m-%d.txt"
        circe-format-self-say "{nick:+13s} ┃ {body}")

  (custom-set-faces!
    '(circe-my-message-face :weight unspecified))

  (enable-lui-logging-globally)
  (enable-circe-display-images)

  (defun lui-org-to-irc ()
    "Examine a buffer with simple org-mode formatting, and converts the empasis:
  *bold*, /italic/, and _underline_ to IRC semi-standard escape codes.
  =code= is converted to inverse (highlighted) text."
    (goto-char (point-min))
    (while (re-search-forward "\\_<\\(?1:[*/_=]\\)\\(?2:[^[:space:]]\\(?:.*?[^[:space:]]\\)?\\)\\1\\_>" nil t)
      (replace-match
       (concat (pcase (match-string 1)
                 ("*" "")
                 ("/" "")
                 ("_" "")
                 ("=" ""))
               (match-string 2)
               "") nil nil)))
  
  (add-hook 'lui-pre-input-hook #'lui-org-to-irc)

  (defun lui-ascii-to-emoji ()
    (goto-char (point-min))
    (while (re-search-forward "\\( \\)?::?\\([^[:space:]:]+\\):\\( \\)?" nil t)
      (replace-match
       (concat
        (match-string 1)
        (or (cdr (assoc (match-string 2) lui-emojis-alist))
            (concat ":" (match-string 2) ":"))
        (match-string 3))
       nil nil)))
  
  (defun lui-emoticon-to-emoji ()
    (dolist (emoticon lui-emoticons-alist)
      (goto-char (point-min))
      (while (re-search-forward (concat " " (car emoticon) "\\( \\)?") nil t)
        (replace-match (concat " "
                               (cdr (assoc (cdr emoticon) lui-emojis-alist))
                               (match-string 1))))))
  
  (define-minor-mode lui-emojify
    "Replace :emojis: and ;) emoticons with unicode emoji chars."
    :global t
    :init-value t
    (if lui-emojify
        (add-hook! lui-pre-input #'lui-ascii-to-emoji #'lui-emoticon-to-emoji)
      (remove-hook! lui-pre-input #'lui-ascii-to-emoji #'lui-emoticon-to-emoji)))
  (defvar lui-emojis-alist
    '(("grinning"                      . "😀")
      ("smiley"                        . "😃")
      ("smile"                         . "😄")
      ("grin"                          . "😁")
      ("laughing"                      . "😆")
      ("sweat_smile"                   . "😅")
      ("joy"                           . "😂")
      ("rofl"                          . "🤣")
      ("relaxed"                       . "☺️")
      ("blush"                         . "😊")
      ("innocent"                      . "😇")
      ("slight_smile"                  . "🙂")
      ("upside_down"                   . "🙃")
      ("wink"                          . "😉")
      ("relieved"                      . "😌")
      ("heart_eyes"                    . "😍")
      ("yum"                           . "😋")
      ("stuck_out_tongue"              . "😛")
      ("stuck_out_tongue_closed_eyes"  . "😝")
      ("stuck_out_tongue_wink"         . "😜")
      ("zanzy"                         . "🤪")
      ("raised_eyebrow"                . "🤨")
      ("monocle"                       . "🧐")
      ("nerd"                          . "🤓")
      ("cool"                          . "😎")
      ("star_struck"                   . "🤩")
      ("party"                         . "🥳")
      ("smirk"                         . "😏")
      ("unamused"                      . "😒")
      ("disapointed"                   . "😞")
      ("pensive"                       . "😔")
      ("worried"                       . "😟")
      ("confused"                      . "😕")
      ("slight_frown"                  . "🙁")
      ("frown"                         . "☹️")
      ("persevere"                     . "😣")
      ("confounded"                    . "😖")
      ("tired"                         . "😫")
      ("weary"                         . "😩")
      ("pleading"                      . "🥺")
      ("tear"                          . "😢")
      ("cry"                           . "😢")
      ("sob"                           . "😭")
      ("triumph"                       . "😤")
      ("angry"                         . "😠")
      ("rage"                          . "😡")
      ("exploding_head"                . "🤯")
      ("flushed"                       . "😳")
      ("hot"                           . "🥵")
      ("cold"                          . "🥶")
      ("scream"                        . "😱")
      ("fearful"                       . "😨")
      ("disapointed"                   . "😰")
      ("relieved"                      . "😥")
      ("sweat"                         . "😓")
      ("thinking"                      . "🤔")
      ("shush"                         . "🤫")
      ("liar"                          . "🤥")
      ("blank_face"                    . "😶")
      ("neutral"                       . "😐")
      ("expressionless"                . "😑")
      ("grimace"                       . "😬")
      ("rolling_eyes"                  . "🙄")
      ("hushed"                        . "😯")
      ("frowning"                      . "😦")
      ("anguished"                     . "😧")
      ("wow"                           . "😮")
      ("astonished"                    . "😲")
      ("sleeping"                      . "😴")
      ("drooling"                      . "🤤")
      ("sleepy"                        . "😪")
      ("dizzy"                         . "😵")
      ("zipper_mouth"                  . "🤐")
      ("woozy"                         . "🥴")
      ("sick"                          . "🤢")
      ("vomiting"                      . "🤮")
      ("sneeze"                        . "🤧")
      ("mask"                          . "😷")
      ("bandaged_head"                 . "🤕")
      ("money_face"                    . "🤑")
      ("cowboy"                        . "🤠")
      ("imp"                           . "😈")
      ("ghost"                         . "👻")
      ("alien"                         . "👽")
      ("robot"                         . "🤖")
      ("clap"                          . "👏")
      ("thumpup"                       . "👍")
      ("+1"                            . "👍")
      ("thumbdown"                     . "👎")
      ("-1"                            . "👎")
      ("ok"                            . "👌")
      ("pinch"                         . "🤏")
      ("left"                          . "👈")
      ("right"                         . "👉")
      ("down"                          . "👇")
      ("wave"                          . "👋")
      ("pray"                          . "🙏")
      ("eyes"                          . "👀")
      ("brain"                         . "🧠")
      ("facepalm"                      . "🤦")
      ("tada"                          . "🎉")
      ("fire"                          . "🔥")
      ("flying_money"                  . "💸")
      ("lighbulb"                      . "💡")
      ("heart"                         . "❤️")
      ("sparkling_heart"               . "💖")
      ("heartbreak"                    . "💔")
      ("100"                           . "💯")))
  
  (defvar lui-emoticons-alist
    '((":)"   . "slight_smile")
      (";)"   . "wink")
      (":D"   . "smile")
      ("=D"   . "grin")
      ("xD"   . "laughing")
      (";("   . "joy")
      (":P"   . "stuck_out_tongue")
      (";D"   . "stuck_out_tongue_wink")
      ("xP"   . "stuck_out_tongue_closed_eyes")
      (":("   . "slight_frown")
      (";("   . "cry")
      (";'("  . "sob")
      (">:("  . "angry")
      (">>:(" . "rage")
      (":o"   . "wow")
      (":O"   . "astonished")
      (":/"   . "confused")
      (":-/"  . "thinking")
      (":|"   . "neutral")
      (":-|"  . "expressionless")))

  (defun named-circe-prompt ()
    (lui-set-prompt
     (concat (propertize (format "%13s > " (circe-nick))
                         'face 'circe-prompt-face)
             "")))
  (add-hook 'circe-chat-mode-hook #'named-circe-prompt)

  (appendq! all-the-icons-mode-icon-alist
            '((circe-channel-mode all-the-icons-material "message" :face all-the-icons-lblue)
              (circe-server-mode all-the-icons-material "chat_bubble_outline" :face all-the-icons-purple))))

(defun auth-server-pass (server)
  (if-let ((secret (plist-get (car (auth-source-search :host server)) :secret)))
      (if (functionp secret)
          (funcall secret) secret)
    (error "Could not fetch password for host %s" server)))

(defun register-irc-auths ()
  (require 'circe)
  (require 'dash)
  (let ((accounts (-filter (lambda (a) (string= "irc" (plist-get a :for)))
                           (auth-source-search :require '(:for) :max 10))))
    (appendq! circe-network-options
              (mapcar (lambda (entry)
                        (let* ((host (plist-get entry :host))
                               (label (or (plist-get entry :label) host))
                               (_ports (mapcar #'string-to-number
                                               (s-split "," (plist-get entry :port))))
                               (port (if (= 1 (length _ports)) (car _ports) _ports))
                               (user (plist-get entry :user))
                               (nick (or (plist-get entry :nick) user))
                               (channels (mapcar (lambda (c) (concat "#" c))
                                                 (s-split "," (plist-get entry :channels)))))
                          `(,label
                            :host ,host :port ,port :nick ,nick
                            :sasl-username ,user :sasl-password auth-server-pass
                            :channels ,channels)))
                      accounts))))

(add-transient-hook! #'=irc (register-irc-auths))
(defun lui-org-to-irc ()
  "Examine a buffer with simple org-mode formatting, and converts the empasis:
*bold*, /italic/, and _underline_ to IRC semi-standard escape codes.
=code= is converted to inverse (highlighted) text."
  (goto-char (point-min))
  (while (re-search-forward "\\_<\\(?1:[*/_=]\\)\\(?2:[^[:space:]]\\(?:.*?[^[:space:]]\\)?\\)\\1\\_>" nil t)
    (replace-match
     (concat (pcase (match-string 1)
               ("*" "")
               ("/" "")
               ("_" "")
               ("=" ""))
             (match-string 2)
             "") nil nil)))

(add-hook 'lui-pre-input-hook #'lui-org-to-irc)
(defun lui-ascii-to-emoji ()
  (goto-char (point-min))
  (while (re-search-forward "\\( \\)?::?\\([^[:space:]:]+\\):\\( \\)?" nil t)
    (replace-match
     (concat
      (match-string 1)
      (or (cdr (assoc (match-string 2) lui-emojis-alist))
          (concat ":" (match-string 2) ":"))
      (match-string 3))
     nil nil)))

(defun lui-emoticon-to-emoji ()
  (dolist (emoticon lui-emoticons-alist)
    (goto-char (point-min))
    (while (re-search-forward (concat " " (car emoticon) "\\( \\)?") nil t)
      (replace-match (concat " "
                             (cdr (assoc (cdr emoticon) lui-emojis-alist))
                             (match-string 1))))))

(define-minor-mode lui-emojify
  "Replace :emojis: and ;) emoticons with unicode emoji chars."
  :global t
  :init-value t
  (if lui-emojify
      (add-hook! lui-pre-input #'lui-ascii-to-emoji #'lui-emoticon-to-emoji)
    (remove-hook! lui-pre-input #'lui-ascii-to-emoji #'lui-emoticon-to-emoji)))
(defvar lui-emojis-alist
  '(("grinning"                      . "😀")
    ("smiley"                        . "😃")
    ("smile"                         . "😄")
    ("grin"                          . "😁")
    ("laughing"                      . "😆")
    ("sweat_smile"                   . "😅")
    ("joy"                           . "😂")
    ("rofl"                          . "🤣")
    ("relaxed"                       . "☺️")
    ("blush"                         . "😊")
    ("innocent"                      . "😇")
    ("slight_smile"                  . "🙂")
    ("upside_down"                   . "🙃")
    ("wink"                          . "😉")
    ("relieved"                      . "😌")
    ("heart_eyes"                    . "😍")
    ("yum"                           . "😋")
    ("stuck_out_tongue"              . "😛")
    ("stuck_out_tongue_closed_eyes"  . "😝")
    ("stuck_out_tongue_wink"         . "😜")
    ("zanzy"                         . "🤪")
    ("raised_eyebrow"                . "🤨")
    ("monocle"                       . "🧐")
    ("nerd"                          . "🤓")
    ("cool"                          . "😎")
    ("star_struck"                   . "🤩")
    ("party"                         . "🥳")
    ("smirk"                         . "😏")
    ("unamused"                      . "😒")
    ("disapointed"                   . "😞")
    ("pensive"                       . "😔")
    ("worried"                       . "😟")
    ("confused"                      . "😕")
    ("slight_frown"                  . "🙁")
    ("frown"                         . "☹️")
    ("persevere"                     . "😣")
    ("confounded"                    . "😖")
    ("tired"                         . "😫")
    ("weary"                         . "😩")
    ("pleading"                      . "🥺")
    ("tear"                          . "😢")
    ("cry"                           . "😢")
    ("sob"                           . "😭")
    ("triumph"                       . "😤")
    ("angry"                         . "😠")
    ("rage"                          . "😡")
    ("exploding_head"                . "🤯")
    ("flushed"                       . "😳")
    ("hot"                           . "🥵")
    ("cold"                          . "🥶")
    ("scream"                        . "😱")
    ("fearful"                       . "😨")
    ("disapointed"                   . "😰")
    ("relieved"                      . "😥")
    ("sweat"                         . "😓")
    ("thinking"                      . "🤔")
    ("shush"                         . "🤫")
    ("liar"                          . "🤥")
    ("blank_face"                    . "😶")
    ("neutral"                       . "😐")
    ("expressionless"                . "😑")
    ("grimace"                       . "😬")
    ("rolling_eyes"                  . "🙄")
    ("hushed"                        . "😯")
    ("frowning"                      . "😦")
    ("anguished"                     . "😧")
    ("wow"                           . "😮")
    ("astonished"                    . "😲")
    ("sleeping"                      . "😴")
    ("drooling"                      . "🤤")
    ("sleepy"                        . "😪")
    ("dizzy"                         . "😵")
    ("zipper_mouth"                  . "🤐")
    ("woozy"                         . "🥴")
    ("sick"                          . "🤢")
    ("vomiting"                      . "🤮")
    ("sneeze"                        . "🤧")
    ("mask"                          . "😷")
    ("bandaged_head"                 . "🤕")
    ("money_face"                    . "🤑")
    ("cowboy"                        . "🤠")
    ("imp"                           . "😈")
    ("ghost"                         . "👻")
    ("alien"                         . "👽")
    ("robot"                         . "🤖")
    ("clap"                          . "👏")
    ("thumpup"                       . "👍")
    ("+1"                            . "👍")
    ("thumbdown"                     . "👎")
    ("-1"                            . "👎")
    ("ok"                            . "👌")
    ("pinch"                         . "🤏")
    ("left"                          . "👈")
    ("right"                         . "👉")
    ("down"                          . "👇")
    ("wave"                          . "👋")
    ("pray"                          . "🙏")
    ("eyes"                          . "👀")
    ("brain"                         . "🧠")
    ("facepalm"                      . "🤦")
    ("tada"                          . "🎉")
    ("fire"                          . "🔥")
    ("flying_money"                  . "💸")
    ("lighbulb"                      . "💡")
    ("heart"                         . "❤️")
    ("sparkling_heart"               . "💖")
    ("heartbreak"                    . "💔")
    ("100"                           . "💯")))

(defvar lui-emoticons-alist
  '((":)"   . "slight_smile")
    (";)"   . "wink")
    (":D"   . "smile")
    ("=D"   . "grin")
    ("xD"   . "laughing")
    (";("   . "joy")
    (":P"   . "stuck_out_tongue")
    (";D"   . "stuck_out_tongue_wink")
    ("xP"   . "stuck_out_tongue_closed_eyes")
    (":("   . "slight_frown")
    (";("   . "cry")
    (";'("  . "sob")
    (">:("  . "angry")
    (">>:(" . "rage")
    (":o"   . "wow")
    (":O"   . "astonished")
    (":/"   . "confused")
    (":-/"  . "thinking")
    (":|"   . "neutral")
    (":-|"  . "expressionless")))

(provide 'config-irc))

;;:------------------------
;;; Newsfeed
;;:------------------------

;; This block defines `elfeed-show-pdf', `elfeed-link-pdfs',
;; `elfeed-pdf-dir', `+rss/elfeed-show-refresh--better-style',
;; `+rss/elfeed-search-print-entry', `elfeed-show-author-face', and
;; `elfeed-show-title-face'.

(confpkg-with-record '("load: newsfeed" config)
(map! :map elfeed-search-mode-map
      :after elfeed-search
      [remap kill-this-buffer] "q"
      [remap kill-buffer] "q"
      :n doom-leader-key nil
      :n "q" #'+rss/quit
      :n "e" #'elfeed-update
      :n "r" #'elfeed-search-untag-all-unread
      :n "u" #'elfeed-search-tag-all-unread
      :n "s" #'elfeed-search-live-filter
      :n "RET" #'elfeed-search-show-entry
      :n "p" #'elfeed-show-pdf
      :n "+" #'elfeed-search-tag-all
      :n "-" #'elfeed-search-untag-all
      :n "S" #'elfeed-search-set-filter
      :n "b" #'elfeed-search-browse-url
      :n "y" #'elfeed-search-yank)
(map! :map elfeed-show-mode-map
      :after elfeed-show
      [remap kill-this-buffer] "q"
      [remap kill-buffer] "q"
      :n doom-leader-key nil
      :nm "q" #'+rss/delete-pane
      :nm "o" #'ace-link-elfeed
      :nm "RET" #'org-ref-elfeed-add
      :nm "n" #'elfeed-show-next
      :nm "N" #'elfeed-show-prev
      :nm "p" #'elfeed-show-pdf
      :nm "+" #'elfeed-show-tag
      :nm "-" #'elfeed-show-untag
      :nm "s" #'elfeed-show-new-live-search
      :nm "y" #'elfeed-show-yank)
(after! elfeed-search
  (set-evil-initial-state! 'elfeed-search-mode 'normal))
(after! elfeed-show-mode
  (set-evil-initial-state! 'elfeed-show-mode   'normal))

(after! evil-snipe
  (push 'elfeed-show-mode   evil-snipe-disabled-modes)
  (push 'elfeed-search-mode evil-snipe-disabled-modes))
(after! elfeed

  (elfeed-org)
  (use-package! elfeed-link)

  (setq elfeed-search-filter "@1-week-ago +unread"
        elfeed-search-print-entry-function '+rss/elfeed-search-print-entry
        elfeed-search-title-min-width 80
        elfeed-show-entry-switch #'pop-to-buffer
        elfeed-show-entry-delete #'+rss/delete-pane
        elfeed-show-refresh-function #'+rss/elfeed-show-refresh--better-style
        shr-max-image-proportion 0.6)

  (add-hook! 'elfeed-show-mode-hook (hide-mode-line-mode 1))
  (add-hook! 'elfeed-search-update-hook #'hide-mode-line-mode)

  (defface elfeed-show-title-face '((t (:weight ultrabold :slant italic :height 1.5)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (defface elfeed-show-author-face `((t (:weight light)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (set-face-attribute 'elfeed-search-title-face nil
                      :foreground 'nil
                      :weight 'light)

  (defadvice! +rss-elfeed-wrap-h-nicer ()
    "Enhances an elfeed entry's readability by wrapping it to a width of
`fill-column' and centering it with `visual-fill-column-mode'."
    :override #'+rss-elfeed-wrap-h
    (setq-local truncate-lines nil
                shr-width 120
                visual-fill-column-center-text t
                default-text-properties '(line-height 1.1))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (visual-fill-column-mode)
      ;; (setq-local shr-current-font '(:family "Merriweather" :height 1.2))
      (set-buffer-modified-p nil)))

  (defun +rss/elfeed-search-print-entry (entry)
    "Print ENTRY to the buffer."
    (let* ((elfeed-goodies/tag-column-width 40)
           (elfeed-goodies/feed-source-column-width 30)
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (concat (mapconcat 'identity tags ",")))
           (title-width (- (window-width) elfeed-goodies/feed-source-column-width
                           elfeed-goodies/tag-column-width 4))

           (tag-column (elfeed-format-column
                        tags-str (elfeed-clamp (length tags-str)
                                               elfeed-goodies/tag-column-width
                                               elfeed-goodies/tag-column-width)
                        :left))
           (feed-column (elfeed-format-column
                         feed-title (elfeed-clamp elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width)
                         :left)))

      (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
      (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
      (insert (propertize title 'face title-faces 'kbd-help title))
      (setq-local line-spacing 0.2)))

  (defun +rss/elfeed-show-refresh--better-style ()
    "Update the buffer to match the selected entry, using a mail-style."
    (interactive)
    (let* ((inhibit-read-only t)
           (title (elfeed-entry-title elfeed-show-entry))
           (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
           (author (elfeed-meta elfeed-show-entry :author))
           (link (elfeed-entry-link elfeed-show-entry))
           (tags (elfeed-entry-tags elfeed-show-entry))
           (tagsstr (mapconcat #'symbol-name tags ", "))
           (nicedate (format-time-string "%a, %e %b %Y %T %Z" date))
           (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
           (type (elfeed-entry-content-type elfeed-show-entry))
           (feed (elfeed-entry-feed elfeed-show-entry))
           (feed-title (elfeed-feed-title feed))
           (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
      (erase-buffer)
      (insert "\n")
      (insert (format "%s\n\n" (propertize title 'face 'elfeed-show-title-face)))
      (insert (format "%s\t" (propertize feed-title 'face 'elfeed-search-feed-face)))
      (when (and author elfeed-show-entry-author)
        (insert (format "%s\n" (propertize author 'face 'elfeed-show-author-face))))
      (insert (format "%s\n\n" (propertize nicedate 'face 'elfeed-log-date-face)))
      (when tags
        (insert (format "%s\n"
                        (propertize tagsstr 'face 'elfeed-search-tag-face))))
      ;; (insert (propertize "Link: " 'face 'message-header-name))
      ;; (elfeed-insert-link link link)
      ;; (insert "\n")
      (cl-loop for enclosure in (elfeed-entry-enclosures elfeed-show-entry)
               do (insert (propertize "Enclosure: " 'face 'message-header-name))
               do (elfeed-insert-link (car enclosure))
               do (insert "\n"))
      (insert "\n")
      (if content
          (if (eq type 'html)
              (elfeed-insert-html content base)
            (insert content))
        (insert (propertize "(empty)\n" 'face 'italic)))
      (goto-char (point-min))))

  )
(after! elfeed-show
  (require 'url)

  (defvar elfeed-pdf-dir
    (expand-file-name "pdfs/"
                      (file-name-directory (directory-file-name elfeed-enclosure-default-dir))))

  (defvar elfeed-link-pdfs
    '(("https://www.jstatsoft.org/index.php/jss/article/view/v0\\([^/]+\\)" . "https://www.jstatsoft.org/index.php/jss/article/view/v0\\1/v\\1.pdf")
      ("http://arxiv.org/abs/\\([^/]+\\)" . "https://arxiv.org/pdf/\\1.pdf"))
    "List of alists of the form (REGEX-FOR-LINK . FORM-FOR-PDF)")

  (defun elfeed-show-pdf (entry)
    (interactive
     (list (or elfeed-show-entry (elfeed-search-selected :ignore-region))))
    (let ((link (elfeed-entry-link entry))
          (feed-name (plist-get (elfeed-feed-meta (elfeed-entry-feed entry)) :title))
          (title (elfeed-entry-title entry))
          (file-view-function
           (lambda (f)
             (when elfeed-show-entry
               (elfeed-kill-buffer))
             (pop-to-buffer (find-file-noselect f))))
          pdf)

      (let ((file (expand-file-name
                   (concat (subst-char-in-string ?/ ?, title) ".pdf")
                   (expand-file-name (subst-char-in-string ?/ ?, feed-name)
                                     elfeed-pdf-dir))))
        (if (file-exists-p file)
            (funcall file-view-function file)
          (dolist (link-pdf elfeed-link-pdfs)
            (when (and (string-match-p (car link-pdf) link)
                       (not pdf))
              (setq pdf (replace-regexp-in-string (car link-pdf) (cdr link-pdf) link))))
          (if (not pdf)
              (message "No associated PDF for entry")
            (message "Fetching %s" pdf)
            (unless (file-exists-p (file-name-directory file))
              (make-directory (file-name-directory file) t))
            (url-copy-file pdf file)
            (funcall file-view-function file))))))

  )

(provide 'config-newsfeed))

;;:------------------------
;;; Mail
;;:------------------------

;; This block defines `+org-msg-goto-body-when-replying',
;; `+org-msg-goto-body', `+mu4e-org-ml-signature',
;; `+mu4e-goto-subject-not-to-once', `+mu4e-compose-org-ml-setup',
;; `+browse-url-orgmode-ml', `+mu4e-ml-message-link',
;; `+org-ml-transient-mu4e-action', `+org-ml-select-patch-thread',
;; `+org-ml-current-patches', `+org-ml-apply-patch', `+org-ml--cache',
;; `+org-ml--cache-timestamp', `+org-ml-max-age',
;; `+org-ml-target-dir', `+mu4e-insert-woof-header',
;; `+mu4e-get-woof-header', `+mu4e-evil-enter-insert-mode',
;; `+mu4e-account-sent-folder', `+mu4e-update-personal-addresses',
;; `mu4e-from-name', `mu4e-compose-from-mailto',
;; `+mu4e-header--folder-colors', `mu4e-reindex-maybe',
;; `mu4e-file-reindex-request', `mu4e-reindex-request--add-watcher',
;; `mu4e-reindex-request--last-time',
;; `mu4e-reindex-request--file-just-deleted',
;; `mu4e-reindex-request--file-watcher',
;; `mu4e-reindex-request-min-seperation', and
;; `mu4e-reindex-request-file'.

(confpkg-with-record '("hook: mail" set-hooks)
;; Begin pre
(setq +org-msg-accent-color "#1a5fb4")
;; End pre
  (with-eval-after-load 'mu4e
(confpkg-with-record '("load: mail" load-hooks)
(require 'config-mail)
)))

;;:------------------------
;;; !Pkg LSP
;;:------------------------

(confpkg-with-record '("load: pkg-lsp" config)
(use-package lsp-mode
  :custom
  (map! :leader
        (:prefix "t"
         :desc "lsp treemacs symbols" "s" #'lsp-treemacs-symbols))
  (lsp-enable-file-watchers nil)
  (lsp-treemacs-sync-mode)
  (lsp-ui-doc-position "Bottom")
  (lsp-keep-workspace-alive nil)
  (lsp-ui-doc-enable t)
  (lsp-ui-sideline-enable nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-segments '(symbols))
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  ;; use tree-sitter to high-light code instead
  (lsp-semantic-tokens-enable nil)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-show-with-cursor nil)
  )
(use-package lsp
  :defer t
  :custom
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("python3" "-m" "ffi_navigator.langserver"))
    :major-modes '(python-mode c++-mode)
    :server-id 'ffi-navigator
    :add-on? t))
  )

(provide 'config--pkg-lsp))

;;:------------------------
;;; !Pkg lsp-bridge
;;:------------------------

(confpkg-with-record '("load: pkg-lsp-bridge" config)
;; (customize-set-variable 'company-global-modes
;;                         (append company-global-modes '(python-mode)))

;; (use-package lsp-bridge
;;   :hook
;;   (python-mode . lsp-bridge-mode)
;;   :init
;;   (setq acm-enable-citre t))

;; (use-package lsp-bridge
;;   :delight (lsp-bridge-mode "")
;;   :config
;;   (require 'yasnippet)
;;   (yas-global-mode 1)

;;   (require 'lsp-bridge)
;;   (global-lsp-bridge-mode)
;;   (setq lsp-bridge-c-lsp-server "clangd"
;;         acm-enable-yas nil
;;         acm-enable-tabnine nil
;;         lsp-bridge-enable-hover-diagnostic t
;;         acm-enable-codeium t)
;;   ;; (setq lsp-bridge-signature-show-function 'lsp-bridge-signature-posframe)
;;   (map! :mode prog-mode
;;         (:leader
;;          :desc "Rename symbol" "c r" #'lsp-bridge-rename
;;          :desc "Diagnostics list" "c x" #'lsp-bridge-diagnostic-list
;;          :desc "Code actions" "c a" #'lsp-bridge-code-action
;;          :desc "Format document" "c f" #'lsp-bridge-code-format))

;;   (add-to-list '+lookup-definition-functions #'lsp-bridge-find-def)
;;   (add-to-list '+lookup-implementations-functions #'lsp-bridge-find-impl)
;;   (add-to-list '+lookup-references-functions #'lsp-bridge-find-references)
;;   (add-to-list '+lookup-type-definition-functions #'lsp-bridge-find-type-def)
;;   (add-to-list '+lookup-documentation-functions #'lsp-bridge-popup-documentation)

;;   (unless (display-graphic-p)
;;     (with-eval-after-load 'acm
;;       (require 'acm-terminal))))

(provide 'config--pkg-lsp-bridge))

;;:------------------------
;;; File Templates
;;:------------------------

(confpkg-with-record '("load: file-templates" config)
(set-file-template! "\\.tex$" :trigger "__" :mode 'latex-mode)
(set-file-template! "\\.org$" :trigger "__" :mode 'org-mode)
(set-file-template! "/LICEN[CS]E$" :trigger '+file-templates/insert-license)

(provide 'config-file-templates))

;;:------------------------
;;; Agda
;;:------------------------

(confpkg-with-record '("load: agda" config)
(setq auto-mode-alist
      (append
       '(("\\.agda\\'" . agda2-mode)
         ("\\.lagda.md\\'" . agda2-mode))
       auto-mode-alist))

(provide 'config-agda))

;;:------------------------
;;; CUDA Fortran
;;:------------------------

(confpkg-with-record '("load: cuda-fortran" config)
(setq auto-mode-alist
      (append
       '(("\\.cuf\\'" . f90-mode)
         ("\\.CUF\\'" . f90-mode))
       auto-mode-alist))

(provide 'config-cuda-fortran))

;;:------------------------
;;; !Pkg clang-format
;;:------------------------

(confpkg-with-record '("load: pkg-clang-format" config)
(use-package clang-format+
  :config
  (add-hook 'c-mode-common-hook #'clang-format+-mode)
  (setq clang-format+-context 'modification)
  (setq clang-format+-always-enable t))

(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode    ; elisp's mechanisms are good enough
            cpp-mode  ; use clang-format+ for C/C++
            c++-mode
            c-mode
            f90-mode
            org-msg-edit-mode)) ; doesn't need a formatter

(provide 'config--pkg-clang-format))

;;:------------------------
;;; CUDA
;;:------------------------

(confpkg-with-record '("load: cuda" config)
(setq auto-mode-alist
      (append
       '(("\\.cu\\'" . c++-mode))
       auto-mode-alist))

(provide 'config-cuda))

;;:------------------------
;;; !Pkg Grammarly
;;:------------------------

(confpkg-with-record '("load: pkg-grammarly" config)
;;(package! lsp-grammarly)
;; (use-package lsp-grammarly
;;   :hook (text-mode . (lambda ()
;;                        (require 'lsp-grammarly)
;;                        (lsp)))
;; )  ; or lsp-deferred

(provide 'config--pkg-grammarly))

;;:------------------------
;;; !Pkg just-mode
;;:------------------------

(confpkg-with-record '("load: pkg-just-mode" config)
(provide 'config--pkg-just-mode))

;;:------------------------
;;; !Pkg lean4
;;:------------------------

(confpkg-with-record '("load: pkg-lean-" config)
(add-hook 'lean-mode-hook
          (lambda ()  (setq-default sis-english-source "Lean")))
(use-package lean4-mode
  ;; to defer loading the package until required
  :commands (lean4-mode))

(provide 'config--pkg-lean-))

;;:------------------------
;;; !Pkg meson
;;:------------------------

(confpkg-with-record '("load: pkg-meson" config)
(use-package meson-mode
  :config
  (add-hook 'meson-mode-hook 'company-mode))
;; (setq auto-mode-alist
;;       (append
;;        '(
;;          ("\\meson.build\\'" . meson-mode)
;;          )
;;        auto-mode-alist))

(provide 'config--pkg-meson))

;;:------------------------
;;; !Pkg LLVM
;;:------------------------

(confpkg-with-record '("load: pkg-llvm" config)
;; Use my own fork of the llvm-tools which located in the llvm-project monorepo

(provide 'config--pkg-llvm))

;;:------------------------
;;; !Pkg sage-shell
;;:------------------------

(confpkg-with-record '("load: pkg-sage-shell" config)
(provide 'config--pkg-sage-shell))

;;:------------------------
;;; !Pkg OCaml
;;:------------------------

(confpkg-with-record '("load: pkg-ocaml" config)
(add-hook 'before-save-hook 'ocamlformat-before-save)

(provide 'config--pkg-ocaml))

;;:------------------------
;;; Rust
;;:------------------------

(confpkg-with-record '("load: rust" config)
(after! rustic
  (setq rustic-lsp-server 'rust-analyzer
        lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-cargo-load-out-dirs-from-check t
        lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-all-features t
        rustic-format-on-save t
)
)
(use-package dap-mode
  :config
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  (require 'dap-cpptools)
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
	    :gdbpath "rust-lldb"
         :target nil
         :cwd nil)))

(provide 'config-rust))

;;:------------------------
;;; !Pkg WebGPU
;;:------------------------

(confpkg-with-record '("load: pkg-webgpu" config)
(provide 'config--pkg-webgpu))

;;:------------------------
;;; !Pkg WebAssembly
;;:------------------------

(confpkg-with-record '("load: pkg-webassembly" config)
(provide 'config--pkg-webassembly))

;;:------------------------
;;; Plaintext
;;:------------------------

;; This block defines `+setup-text-mode-left-margin' and
;; `+text-mode-left-margin-width'.

(confpkg-with-record '("load: plaintext" config)
(after! text-mode
  (add-hook! 'text-mode-hook
    (unless (derived-mode-p 'org-mode)
      ;; Apply ANSI color codes
      (with-silent-modifications
        (ansi-color-apply-on-region (point-min) (point-max) t)))))
(defvar +text-mode-left-margin-width 1
  "The `left-margin-width' to be used in `text-mode' buffers.")

(defun +setup-text-mode-left-margin ()
  (when (and (derived-mode-p 'text-mode)
             (not (and (bound-and-true-p visual-fill-column-mode)
                       visual-fill-column-center-text))
             (eq (current-buffer) ; Check current buffer is active.
                 (window-buffer (frame-selected-window))))
    (setq left-margin-width (if display-line-numbers
                                0 +text-mode-left-margin-width))
    (set-window-buffer (get-buffer-window (current-buffer))
                       (current-buffer))))

(add-hook 'window-configuration-change-hook #'+setup-text-mode-left-margin)
(add-hook 'display-line-numbers-mode-hook #'+setup-text-mode-left-margin)
(add-hook 'text-mode-hook #'+setup-text-mode-left-margin)
(defadvice! +doom/toggle-line-numbers--call-hook-a ()
  :after #'doom/toggle-line-numbers
  (run-hooks 'display-line-numbers-mode-hook))
(remove-hook 'text-mode-hook #'display-line-numbers-mode)

(provide 'config-plaintext))

;;:------------------------
;;; !Pkg org-modern
;;:------------------------

(confpkg-with-record '("load: pkg-org-modern" config)
(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "✜" "✿" "✤" "◆" "▶" "✸" "○")
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((43 . "➤")
                          (45 . "–")
                          (42 . "•"))
        org-modern-todo-faces
        '(("TODO" :inverse-video t :inherit org-todo)
          ("PROJ" :inverse-video t :inherit +org-todo-project)
          ("STRT" :inverse-video t :inherit +org-todo-active)
          ("[-]"  :inverse-video t :inherit +org-todo-active)
          ("HOLD" :inverse-video t :inherit +org-todo-onhold)
          ("WAIT" :inverse-video t :inherit +org-todo-onhold)
          ("[?]"  :inverse-video t :inherit +org-todo-onhold)
          ("KILL" :inverse-video t :inherit +org-todo-cancel)
          ("NO"   :inverse-video t :inherit +org-todo-cancel))
        org-modern-footnote
        (cons nil (cadr org-script-display))
        org-modern-block-fringe nil
        org-modern-block-name
        '((t . t)
          ("src" "»" "«")
          ("example" "»–" "–«")
          ("quote" "❝" "❞")
          ("export" "⏩" "⏪"))
        org-modern-progress nil
        org-modern-priority nil
        org-modern-horizontal-rule (make-string 36 ?─)
        org-modern-keyword
        '((t . t)
          ("title" . "𝙏")
          ("subtitle" . "𝙩")
          ("author" . "𝘼")
          ("email" . #("" 0 1 (display (raise -0.14))))
          ("date" . "𝘿")
          ("property" . "☸")
          ("options" . "⌥")
          ("startup" . "⏻")
          ("macro" . "𝓜")
          ("bind" . #("" 0 1 (display (raise -0.1))))
          ("bibliography" . "")
          ("print_bibliography" . #("" 0 1 (display (raise -0.1))))
          ("cite_export" . "⮭")
          ("print_glossary" . #("ᴬᶻ" 0 1 (display (raise -0.1))))
          ("glossary_sources" . #("" 0 1 (display (raise -0.14))))
          ("include" . "⇤")
          ("setupfile" . "⇚")
          ("html_head" . "🅷")
          ("html" . "🅗")
          ("latex_class" . "🄻")
          ("latex_class_options" . #("🄻" 1 2 (display (raise -0.14))))
          ("latex_header" . "🅻")
          ("latex_header_extra" . "🅻⁺")
          ("latex" . "🅛")
          ("beamer_theme" . "🄱")
          ("beamer_color_theme" . #("🄱" 1 2 (display (raise -0.12))))
          ("beamer_font_theme" . "🄱𝐀")
          ("beamer_header" . "🅱")
          ("beamer" . "🅑")
          ("attr_latex" . "🄛")
          ("attr_html" . "🄗")
          ("attr_org" . "⒪")
          ("call" . #("" 0 1 (display (raise -0.15))))
          ("name" . "⁍")
          ("header" . "›")
          ("caption" . "☰")
          ("results" . "🠶")))
  (custom-set-faces! '(org-modern-statistics :inherit org-checkbox-statistics-todo)))
(after! spell-fu
  (cl-pushnew 'org-modern-tag (alist-get 'org-mode +spell-excluded-faces-alist)))

(provide 'config--pkg-org-modern))

;;:------------------------
;;; !Pkg org-appear
;;:------------------------

(confpkg-with-record '("load: pkg-org-appear" config)
(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(provide 'config--pkg-org-appear))

;;:------------------------
;;; !Pkg org-ol-tree
;;:------------------------

(confpkg-with-record '("load: pkg-org-ol-tree" config)
(use-package! org-ol-tree
  :commands org-ol-tree
  :config
  (setq org-ol-tree-ui-icon-set
        (if (and (display-graphic-p)
                 (fboundp 'all-the-icons-material))
            'all-the-icons
          'unicode))
  (org-ol-tree-ui--update-icon-set))

(map! :map org-mode-map
      :after org
      :localleader
      :desc "Outline" "O" #'org-ol-tree)

(provide 'config--pkg-org-ol-tree))

;;:------------------------
;;; !Pkg ob-julia
;;:------------------------

(confpkg-with-record '("load: pkg-ob-julia" config)
;; (package! ob-julia :recipe (:host github :repo "tecosaur/ob-julia" :files ("*.el" "julia")))
;; (use-package! ob-julia
;;   :commands org-babel-execute:julia
;;   :config
;;   (setq org-babel-julia-command-arguments
;;         `("--sysimage"
;;           ,(when-let ((img "~/.local/lib/julia.so")
;;                       (exists? (file-exists-p img)))
;;              (expand-file-name img))
;;           "--threads"
;;           ,(number-to-string (- (doom-system-cpus) 2))
;;           "--banner=no")))

(provide 'config--pkg-ob-julia))

;;:------------------------
;;; !Pkg ob-http
;;:------------------------

(confpkg-with-record '("load: pkg-ob-http" config)
(use-package! ob-http
  :commands org-babel-execute:http)

(provide 'config--pkg-ob-http))

;;:------------------------
;;; !Pkg org-transclusion
;;:------------------------

(confpkg-with-record '("load: pkg-org-transclusion" config)
(use-package! org-transclusion
  :commands org-transclusion-mode
  :init
  (map! :after org :map org-mode-map
        "<f12>" #'org-transclusion-mode))

(provide 'config--pkg-org-transclusion))

;;:------------------------
;;; !Pkg org-graph-view
;;:------------------------

(confpkg-with-record '("load: pkg-org-graph-view" config)
(provide 'config--pkg-org-graph-view))

;;:------------------------
;;; !Pkg org-chef
;;:------------------------

(confpkg-with-record '("load: pkg-org-chef" config)
(use-package! org-chef
  :commands (org-chef-insert-recipe org-chef-get-recipe-from-url))

(provide 'config--pkg-org-chef))

;;:------------------------
;;; !Pkg org-pandoc-import
;;:------------------------

(confpkg-with-record '("load: pkg-org-pandoc-import" config)
(use-package! org-pandoc-import
  :after org)

(provide 'config--pkg-org-pandoc-import))

;;:------------------------
;;; !Pkg org-glossary
;;:------------------------

;; This block defines `+org-glossary--latex-cdef'.

(confpkg-with-record '("load: pkg-org-glossary" config)
(use-package! org-glossary
  :hook (org-mode . org-glossary-mode)
  :config
  (setq org-glossary-collection-root "~/.config/doom/misc/glossaries/")
  (defun +org-glossary--latex-cdef (backend info term-entry form &optional ref-index plural-p capitalized-p extra-parameters)
    (org-glossary--export-template
     (if (plist-get term-entry :uses)
         "*%d*\\emsp{}%v\\ensp{}@@latex:\\labelcpageref{@@%b@@latex:}@@\n"
       "*%d*\\emsp{}%v\n")
     backend info term-entry ref-index
     plural-p capitalized-p extra-parameters))
  (org-glossary-set-export-spec
   'latex t
   :backref "gls-%K-use-%r"
   :backref-seperator ","
   :definition-structure #'+org-glossary--latex-cdef))

(provide 'config--pkg-org-glossary))

;;:------------------------
;;; !Pkg orgdiff
;;:------------------------

;; This block defines `+orgdiff-nicer-change-colours'.

(confpkg-with-record '("load: pkg-orgdiff" config)
(use-package! orgdiff
  :defer t
  :config
  (defun +orgdiff-nicer-change-colours ()
    (goto-char (point-min))
    ;; Set red/blue based on whether chameleon is being used
    (if (search-forward "%% make document follow Emacs theme" nil t)
        (setq red  (substring (doom-blend 'red 'fg 0.8) 1)
              blue (substring (doom-blend 'blue 'teal 0.6) 1))
      (setq red  "c82829"
            blue "00618a"))
    (when (and (search-forward "%DIF PREAMBLE EXTENSION ADDED BY LATEXDIFF" nil t)
               (search-forward "\\RequirePackage{color}" nil t))
      (when (re-search-forward "definecolor{red}{rgb}{1,0,0}" (cdr (bounds-of-thing-at-point 'line)) t)
        (replace-match (format "definecolor{red}{HTML}{%s}" red)))
      (when (re-search-forward "definecolor{blue}{rgb}{0,0,1}" (cdr (bounds-of-thing-at-point 'line)) t)
        (replace-match (format "definecolor{blue}{HTML}{%s}" blue)))))
  (add-to-list 'orgdiff-latexdiff-postprocess-hooks #'+orgdiff-nicer-change-colours))

(provide 'config--pkg-orgdiff))

;;:------------------------
;;; !Pkg org-music
;;:------------------------

(confpkg-with-record '("load: pkg-org-music" config)
(use-package! org-music
  :after org
  :config
  (setq org-music-mpris-player "Lollypop"
        org-music-track-search-method 'beets
        org-music-beets-db "~/Music/library.db"))

(provide 'config--pkg-org-music))

;;:------------------------
;;; Org Behaviour
;;:------------------------

;; This block defines `+org-export-yt', `+org-xkcd-complete',
;; `+org-xkcd-export', `+org-xkcd-image-fn', `+org-xkcd-open-fn',
;; `org-syntax-convert-keyword-case-to-lower',
;; `+yas/org-most-common-no-property-lang', `+yas/org-last-src-lang',
;; `+yas/org-src-lang', `+yas/org-prompt-header-arg',
;; `+yas/org-src-header-p', `unpackaged/org-return-dwim',
;; `unpackaged/org-element-descendant-of',
;; `unpackaged/org-export-new-named-reference',
;; `unpackaged/org-export-get-reference', `org-reference-contraction',
;; `org-reference-contraction-truncate-words',
;; `org-reference-contraction-joining-char',
;; `org-reference-contraction-stripped-words',
;; `org-reference-contraction-max-length',
;; `org-reference-contraction-max-words',
;; `org-capture-reinitialise-hook', `set-org-capture-templates',
;; `+org-capture-recipies', `+doct-iconify-capture-templates',
;; `+doct-icon-declaration-to-icon', `org-mks-pretty',
;; `org-capture-select-template-prettier',
;; `org-view-external-file-extensions',
;; `org-view-output-file-extensions', `org-view-output-file',
;; `org-babel-lang-list', `lsp-org-babel-enable',
;; `+org-insert-file-link', and `+org-export-remove-zero-width-space'.

(confpkg-with-record '("hook: org-behaviour" set-hooks)
  (with-eval-after-load 'org
(confpkg-with-record '("load: org-behaviour" load-hooks)
(setq org-directory "~/org" ; Let's put files here.
      org-agenda-files (list org-directory)                  ; Seems like the obvious place.
      org-use-property-inheritance t                         ; It's convenient to have properties inherited.
      org-log-done 'time                                     ; Having the time a item is done sounds convenient.
      org-list-allow-alphabetical t                          ; Have a. A. a) A) list bullets.
      org-catch-invisible-edits 'smart                       ; Try not to accidently do weird stuff in invisible regions.
      org-export-with-sub-superscripts '{}                   ; Don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}.
      org-export-allow-bind-keywords t                       ; Bind keywords can be handy
      org-image-actual-width '(0.9))                         ; Make the in-buffer display closer to the exported result..
(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "replace")
        (:exports . "code")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        (:comments . "link")))
;; (map! :map evil-org-mode-map
;;       :after evil-org
;;       :n "g <up>" #'org-backward-heading-same-level
;;       :n "g <down>" #'org-forward-heading-same-level
;;       :n "g <left>" #'org-up-element
;;       :n "g <right>" #'org-down-element)
(map! :map org-mode-map
      :nie "M-SPC M-SPC" (cmd! (insert "\u200B")))
(defun +org-export-remove-zero-width-space (text _backend _info)
  "Remove zero width spaces from TEXT."
  (unless (org-export-derived-backend-p 'org)
    (replace-regexp-in-string "\u200B" "" text)))

(after! ox
  (add-to-list 'org-export-filter-final-output-functions #'+org-export-remove-zero-width-space t))
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))
(defun +org-insert-file-link ()
  "Insert a file link.  At the prompt, enter the filename."
  (interactive)
  (insert (format "[[%s]]" (org-link-complete-file))))
(map! :after org
      :map org-mode-map
      :localleader
      "l f" #'+org-insert-file-link)
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
(defadvice! org-edit-latex-emv-after-insert ()
  :after #'org-cdlatex-environment-indent
  (org-edit-latex-environment))
(cl-defmacro lsp-org-babel-enable (lang)
  "Support LANG in org source code block."
  (setq centaur-lsp 'lsp-mode)
  (cl-check-type lang stringp)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
         (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
         (let ((file-name (->> info caddr (alist-get :file))))
           (unless file-name
             (setq file-name (make-temp-file "babel-lsp-")))
           (setq buffer-file-name file-name)
           (lsp-deferred)))
       (put ',intern-pre 'function-documentation
            (format "Enable lsp-mode in the buffer of org source block (%s)."
                    (upcase ,lang)))
       (if (fboundp ',edit-pre)
           (advice-add ',edit-pre :after ',intern-pre)
         (progn
           (defun ,edit-pre (info)
             (,intern-pre info))
           (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))
(defvar org-babel-lang-list
  '("go" "python" "ipython" "bash" "sh"))
(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enable ,lang)))
(map! :map org-mode-map
      :localleader
      :desc "View exported file" "v" #'org-view-output-file)

(defun org-view-output-file (&optional org-file-path)
  "Visit buffer open on the first output file (if any) found, using `org-view-output-file-extensions'"
  (interactive)
  (let* ((org-file-path (or org-file-path (buffer-file-name) ""))
         (dir (file-name-directory org-file-path))
         (basename (file-name-base org-file-path))
         (output-file nil))
    (dolist (ext org-view-output-file-extensions)
      (unless output-file
        (when (file-exists-p
               (concat dir basename "." ext))
          (setq output-file (concat dir basename "." ext)))))
    (if output-file
        (if (member (file-name-extension output-file) org-view-external-file-extensions)
            (browse-url-xdg-open output-file)
          (pop-to-buffer (or (find-buffer-visiting output-file)
                             (find-file-noselect output-file))))
      (message "No exported file found"))))

(defvar org-view-output-file-extensions '("pdf" "md" "rst" "txt" "tex" "html")
  "Search for output files with these extensions, in order, viewing the first that matches")
(defvar org-view-external-file-extensions '("html")
  "File formats that should be opened externally.")
(use-package! doct
  :commands doct)
(after! org-capture
  (defun org-capture-select-template-prettier (&optional keys)
    "Select a capture template, in a prettier way than default
  Lisp programs can force the template by setting KEYS to a string."
    (let ((org-capture-templates
           (or (org-contextualize-keys
                (org-capture-upgrade-templates org-capture-templates)
                org-capture-templates-contexts)
               '(("t" "Task" entry (file+headline "" "Tasks")
                  "* TODO %?\n  %u\n  %a")))))
      (if keys
          (or (assoc keys org-capture-templates)
              (error "No capture template referred to by \"%s\" keys" keys))
        (org-mks org-capture-templates
                 "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
                 "Template key: "
                 `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
  (advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)
  
  (defun org-mks-pretty (table title &optional prompt specials)
    "Select a member of an alist with multiple keys. Prettified.
  
  TABLE is the alist which should contain entries where the car is a string.
  There should be two types of entries.
  
  1. prefix descriptions like (\"a\" \"Description\")
     This indicates that `a' is a prefix key for multi-letter selection, and
     that there are entries following with keys like \"ab\", \"ax\"…
  
  2. Select-able members must have more than two elements, with the first
     being the string of keys that lead to selecting it, and the second a
     short description string of the item.
  
  The command will then make a temporary buffer listing all entries
  that can be selected with a single key, and all the single key
  prefixes.  When you press the key for a single-letter entry, it is selected.
  When you press a prefix key, the commands (and maybe further prefixes)
  under this key will be shown and offered for selection.
  
  TITLE will be placed over the selection in the temporary buffer,
  PROMPT will be used when prompting for a key.  SPECIALS is an
  alist with (\"key\" \"description\") entries.  When one of these
  is selected, only the bare key is returned."
    (save-window-excursion
      (let ((inhibit-quit t)
            (buffer (org-switch-to-buffer-other-window "*Org Select*"))
            (prompt (or prompt "Select: "))
            case-fold-search
            current)
        (unwind-protect
            (catch 'exit
              (while t
                (setq-local evil-normal-state-cursor (list nil))
                (erase-buffer)
                (insert title "\n\n")
                (let ((des-keys nil)
                      (allowed-keys '("\C-g"))
                      (tab-alternatives '("\s" "\t" "\r"))
                      (cursor-type nil))
                  ;; Populate allowed keys and descriptions keys
                  ;; available with CURRENT selector.
                  (let ((re (format "\\`%s\\(.\\)\\'"
                                    (if current (regexp-quote current) "")))
                        (prefix (if current (concat current " ") "")))
                    (dolist (entry table)
                      (pcase entry
                        ;; Description.
                        (`(,(and key (pred (string-match re))) ,desc)
                         (let ((k (match-string 1 key)))
                           (push k des-keys)
                           ;; Keys ending in tab, space or RET are equivalent.
                           (if (member k tab-alternatives)
                               (push "\t" allowed-keys)
                             (push k allowed-keys))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                        ;; Usable entry.
                        (`(,(and key (pred (string-match re))) ,desc . ,_)
                         (let ((k (match-string 1 key)))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                           (push k allowed-keys)))
                        (_ nil))))
                  ;; Insert special entries, if any.
                  (when specials
                    (insert "─────────────────────────\n")
                    (pcase-dolist (`(,key ,description) specials)
                      (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                      (push key allowed-keys)))
                  ;; Display UI and let user select an entry or
                  ;; a sub-level prefix.
                  (goto-char (point-min))
                  (unless (pos-visible-in-window-p (point-max))
                    (org-fit-window-to-buffer))
                  (let ((pressed (org--mks-read-key allowed-keys
                                                    prompt
                                                    (not (pos-visible-in-window-p (1- (point-max)))))))
                    (setq current (concat current pressed))
                    (cond
                     ((equal pressed "\C-g") (user-error "Abort"))
                     ;; Selection is a prefix: open a new menu.
                     ((member pressed des-keys))
                     ;; Selection matches an association: return it.
                     ((let ((entry (assoc current table)))
                        (and entry (throw 'exit entry))))
                     ;; Selection matches a special entry: return the
                     ;; selection prefix.
                     ((assoc current specials) (throw 'exit current))
                     (t (error "No entry available")))))))
          (when buffer (kill-buffer buffer))))))
  (advice-add 'org-mks :override #'org-mks-pretty)

  (defun +doct-icon-declaration-to-icon (declaration)
    "Convert :icon declaration to icon"
    (let ((name (pop declaration))
          (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
          (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
          (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
      (apply set `(,name :face ,face :v-adjust ,v-adjust))))

  (defun +doct-iconify-capture-templates (groups)
    "Add declaration's :icon to each template group in GROUPS."
    (let ((templates (doct-flatten-lists-in groups)))
      (setq doct-templates (mapcar (lambda (template)
                                     (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                                 (spec (plist-get (plist-get props :doct) :icon)))
                                       (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                      "\t"
                                                                      (nth 1 template))))
                                     template)
                                   templates))))

  (setq doct-after-conversion-functions '(+doct-iconify-capture-templates))

  (defvar +org-capture-recipies  "~/org/recipies.org")

  (defun set-org-capture-templates ()
    (setq org-capture-templates
          (doct `(("Personal todo" :keys "t"
                   :icon ("checklist" :set "octicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %?"
                              "%i %a"))
                  ("Personal note" :keys "n"
                   :icon ("sticky-note-o" :set "faicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* %?"
                              "%i %a"))
                  ("Email" :keys "e"
                   :icon ("envelope" :set "faicon" :color "blue")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %^{type|reply to|contact} %\\3 %? :email:"
                              "Send an email %^{urgancy|soon|ASAP|anon|at some point|eventually} to %^{recipiant}"
                              "about %^{topic}"
                              "%U %i %a"))
                  ("Interesting" :keys "i"
                   :icon ("eye" :set "faicon" :color "lcyan")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Interesting"
                   :type entry
                   :template ("* [ ] %{desc}%? :%{i-type}:"
                              "%i %a")
                   :children (("Webpage" :keys "w"
                               :icon ("globe" :set "faicon" :color "green")
                               :desc "%(org-cliplink-capture) "
                               :i-type "read:web")
                              ("Article" :keys "a"
                               :icon ("file-text" :set "octicon" :color "yellow")
                               :desc ""
                               :i-type "read:reaserch")
                              ("\tRecipie" :keys "r"
                               :icon ("spoon" :set "faicon" :color "dorange")
                               :file +org-capture-recipies
                               :headline "Unsorted"
                               :template "%(org-chef-get-recipe-from-url)")
                              ("Information" :keys "i"
                               :icon ("info-circle" :set "faicon" :color "blue")
                               :desc ""
                               :i-type "read:info")
                              ("Idea" :keys "I"
                               :icon ("bubble_chart" :set "material" :color "silver")
                               :desc ""
                               :i-type "idea")))
                  ("Tasks" :keys "k"
                   :icon ("inbox" :set "octicon" :color "yellow")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Tasks"
                   :type entry
                   :template ("* TODO %? %^G%{extra}"
                              "%i %a")
                   :children (("General Task" :keys "k"
                               :icon ("inbox" :set "octicon" :color "yellow")
                               :extra "")
                              ("Task with deadline" :keys "d"
                               :icon ("timer" :set "material" :color "orange" :v-adjust -0.1)
                               :extra "\nDEADLINE: %^{Deadline:}t")
                              ("Scheduled Task" :keys "s"
                               :icon ("calendar" :set "octicon" :color "orange")
                               :extra "\nSCHEDULED: %^{Start time:}t")))
                  ("Project" :keys "p"
                   :icon ("repo" :set "octicon" :color "silver")
                   :prepend t
                   :type entry
                   :headline "Inbox"
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :file ""
                   :custom (:time-or-todo "")
                   :children (("Project-local todo" :keys "t"
                               :icon ("checklist" :set "octicon" :color "green")
                               :time-or-todo "TODO"
                               :file +org-capture-project-todo-file)
                              ("Project-local note" :keys "n"
                               :icon ("sticky-note" :set "faicon" :color "yellow")
                               :time-or-todo "%U"
                               :file +org-capture-project-notes-file)
                              ("Project-local changelog" :keys "c"
                               :icon ("list" :set "faicon" :color "blue")
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-project-changelog-file)))
                  ("\tCentralised project templates"
                   :keys "o"
                   :type entry
                   :prepend t
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :children (("Project todo"
                               :keys "t"
                               :prepend nil
                               :time-or-todo "TODO"
                               :heading "Tasks"
                               :file +org-capture-central-project-todo-file)
                              ("Project note"
                               :keys "n"
                               :time-or-todo "%U"
                               :heading "Notes"
                               :file +org-capture-central-project-notes-file)
                              ("Project changelog"
                               :keys "c"
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-central-project-changelog-file)))))))

  (set-org-capture-templates)
  (unless (display-graphic-p)
    (add-hook 'server-after-make-frame-hook
              (defun org-capture-reinitialise-hook ()
                (when (display-graphic-p)
                  (set-org-capture-templates)
                  (remove-hook 'server-after-make-frame-hook
                               #'org-capture-reinitialise-hook))))))
(setf (alist-get 'height +org-capture-frame-parameters) 15)
;; (alist-get 'name +org-capture-frame-parameters) "❖ Capture") ;; ATM hardcoded in other places, so changing breaks stuff
(setq +org-capture-fn
      (lambda ()
        (interactive)
        (set-window-parameter nil 'mode-line-format 'none)
        (org-capture)))
(defvar org-reference-contraction-max-words 3
  "Maximum number of words in a reference reference.")
(defvar org-reference-contraction-max-length 35
  "Maximum length of resulting reference reference, including joining characters.")
(defvar org-reference-contraction-stripped-words
  '("the" "on" "in" "off" "a" "for" "by" "of" "and" "is" "to")
  "Superfluous words to be removed from a reference.")
(defvar org-reference-contraction-joining-char "-"
  "Character used to join words in the reference reference.")

(defun org-reference-contraction-truncate-words (words)
  "Using `org-reference-contraction-max-length' as the total character 'budget' for the WORDS
and truncate individual words to conform to this budget.

To arrive at a budget that accounts for words undershooting their requisite average length,
the number of characters in the budget freed by short words is distributed among the words
exceeding the average length.  This adjusts the per-word budget to be the maximum feasable for
this particular situation, rather than the universal maximum average.

This budget-adjusted per-word maximum length is given by the mathematical expression below:

max length = \\floor{ \\frac{total length - chars for seperators - \\sum_{word \\leq average length} length(word) }{num(words) > average length} }"
  ;; trucate each word to a max word length determined by
  ;;
  (let* ((total-length-budget (- org-reference-contraction-max-length  ; how many non-separator chars we can use
                                 (1- (length words))))
         (word-length-budget (/ total-length-budget                      ; max length of each word to keep within budget
                                org-reference-contraction-max-words))
         (num-overlong (-count (lambda (word)                            ; how many words exceed that budget
                                 (> (length word) word-length-budget))
                               words))
         (total-short-length (-sum (mapcar (lambda (word)                ; total length of words under that budget
                                             (if (<= (length word) word-length-budget)
                                                 (length word) 0))
                                           words)))
         (max-length (/ (- total-length-budget total-short-length)       ; max(max-length) that we can have to fit within the budget
                        num-overlong)))
    (mapcar (lambda (word)
              (if (<= (length word) max-length)
                  word
                (substring word 0 max-length)))
            words)))

(defun org-reference-contraction (reference-string)
  "Give a contracted form of REFERENCE-STRING that is only contains alphanumeric characters.
Strips 'joining' words present in `org-reference-contraction-stripped-words',
and then limits the result to the first `org-reference-contraction-max-words' words.
If the total length is > `org-reference-contraction-max-length' then individual words are
truncated to fit within the limit using `org-reference-contraction-truncate-words'."
  (let ((reference-words
         (-filter (lambda (word)
                    (not (member word org-reference-contraction-stripped-words)))
                  (split-string
                   (->> reference-string
                        downcase
                        (replace-regexp-in-string "\\[\\[[^]]+\\]\\[\\([^]]+\\)\\]\\]" "\\1") ; get description from org-link
                        (replace-regexp-in-string "[-/ ]+" " ") ; replace seperator-type chars with space
                        puny-encode-string
                        (replace-regexp-in-string "^xn--\\(.*?\\) ?-?\\([a-z0-9]+\\)$" "\\2 \\1") ; rearrange punycode
                        (replace-regexp-in-string "[^A-Za-z0-9 ]" "") ; strip chars which need %-encoding in a uri
                        ) " +"))))
    (when (> (length reference-words)
             org-reference-contraction-max-words)
      (setq reference-words
            (cl-subseq reference-words 0 org-reference-contraction-max-words)))

    (when (> (apply #'+ (1- (length reference-words))
                    (mapcar #'length reference-words))
             org-reference-contraction-max-length)
      (setq reference-words (org-reference-contraction-truncate-words reference-words)))

    (string-join reference-words org-reference-contraction-joining-char)))
(define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
  "Attempt to export Org as HTML with useful link IDs.
Instead of random IDs like \"#orga1b2c3\", use heading titles,
made unique when necessary."
  :global t
  (if unpackaged/org-export-html-with-useful-ids-mode
      (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
    (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))
(unpackaged/org-export-html-with-useful-ids-mode 1) ; ensure enabled, and advice run

(defun unpackaged/org-export-get-reference (datum info)
  "Like `org-export-get-reference', except uses heading titles instead of random numbers."
  (let ((cache (plist-get info :internal-references)))
    (or (car (rassq datum cache))
        (let* ((crossrefs (plist-get info :crossrefs))
               (cells (org-export-search-cells datum))
               ;; Preserve any pre-existing association between
               ;; a search cell and a reference, i.e., when some
               ;; previously published document referenced a location
               ;; within current file (see
               ;; `org-publish-resolve-external-link').
               ;;
               ;; However, there is no guarantee that search cells are
               ;; unique, e.g., there might be duplicate custom ID or
               ;; two headings with the same title in the file.
               ;;
               ;; As a consequence, before re-using any reference to
               ;; an element or object, we check that it doesn't refer
               ;; to a previous element or object.
               (new (or (cl-some
                         (lambda (cell)
                           (let ((stored (cdr (assoc cell crossrefs))))
                             (when stored
                               (let ((old (org-export-format-reference stored)))
                                 (and (not (assoc old cache)) stored)))))
                         cells)
                        (when (org-element-property :raw-value datum)
                          ;; Heading with a title
                          (unpackaged/org-export-new-named-reference datum cache))
                        (when (member (car datum) '(src-block table example fixed-width property-drawer))
                          ;; Nameable elements
                          (unpackaged/org-export-new-named-reference datum cache))
                        ;; NOTE: This probably breaks some Org Export
                        ;; feature, but if it does what I need, fine.
                        (org-export-format-reference
                         (org-export-new-reference cache))))
               (reference-string new))
          ;; Cache contains both data already associated to
          ;; a reference and in-use internal references, so as to make
          ;; unique references.
          (dolist (cell cells) (push (cons cell new) cache))
          ;; Retain a direct association between reference string and
          ;; DATUM since (1) not every object or element can be given
          ;; a search cell (2) it permits quick lookup.
          (push (cons reference-string datum) cache)
          (plist-put info :internal-references cache)
          reference-string))))

(defun unpackaged/org-export-new-named-reference (datum cache)
  "Return new reference for DATUM that is unique in CACHE."
  (cl-macrolet ((inc-suffixf (place)
                             `(progn
                                (string-match (rx bos
                                                  (minimal-match (group (1+ anything)))
                                                  (optional "--" (group (1+ digit)))
                                                  eos)
                                              ,place)
                                ;; HACK: `s1' instead of a gensym.
                                (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                           (match-string 2 ,place)))
                                        (suffix (if suffix
                                                    (string-to-number suffix)
                                                  0)))
                                  (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
    (let* ((headline-p (eq (car datum) 'headline))
           (title (if headline-p
                      (org-element-property :raw-value datum)
                    (or (org-element-property :name datum)
                        (concat (org-element-property :raw-value
                                                      (org-element-property :parent
                                                                            (org-element-property :parent datum)))))))
           ;; get ascii-only form of title without needing percent-encoding
           (ref (concat (org-reference-contraction (substring-no-properties title))
                        (unless (or headline-p (org-element-property :name datum))
                          (concat ","
                                  (pcase (car datum)
                                    ('src-block "code")
                                    ('example "example")
                                    ('fixed-width "mono")
                                    ('property-drawer "properties")
                                    (_ (symbol-name (car datum))))
                                  "--1"))))
           (parent (when headline-p (org-element-property :parent datum))))
      (while (--any (equal ref (car it))
                    cache)
        ;; Title not unique: make it so.
        (if parent
            ;; Append ancestor title.
            (setf title (concat (org-element-property :raw-value parent)
                                "--" title)
                  ;; get ascii-only form of title without needing percent-encoding
                  ref (org-reference-contraction (substring-no-properties title))
                  parent (when headline-p (org-element-property :parent parent)))
          ;; No more ancestors: add and increment a number.
          (inc-suffixf ref)))
      ref)))

(add-hook 'org-load-hook #'unpackaged/org-export-html-with-useful-ids-mode)
(defadvice! org-export-format-reference-a (reference)
  "Format REFERENCE into a string.

REFERENCE is a either a number or a string representing a reference,
as returned by `org-export-new-reference'."
  :override #'org-export-format-reference
  (if (stringp reference) reference (format "org%07x" reference)))
(defun unpackaged/org-element-descendant-of (type element)
  "Return non-nil if ELEMENT is a descendant of TYPE.
TYPE should be an element type, like `item' or `paragraph'.
ELEMENT should be a list like that returned by `org-element-context'."
  ;; MAYBE: Use `org-element-lineage'.
  (when-let* ((parent (org-element-property :parent element)))
    (or (eq type (car parent))
        (unpackaged/org-element-descendant-of type parent))))

;;;###autoload
(defun unpackaged/org-return-dwim (&optional default)
  "A helpful replacement for `org-return-indent'.  With prefix, call `org-return-indent'.

On headings, move point to position after entry content.  In
lists, insert a new item or end the list, with checkbox if
appropriate.  In tables, insert a new row or end the table."
  ;; Inspired by John Kitchin: http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
  (interactive "P")
  (if default
      (org-return t)
    (cond
     ;; Act depending on context around point.

     ;; NOTE: I prefer RET to not follow links, but by uncommenting this block, links will be
     ;; followed.

     ;; ((eq 'link (car (org-element-context)))
     ;;  ;; Link: Open it.
     ;;  (org-open-at-point-global))

     ((org-at-heading-p)
      ;; Heading: Move to position after entry content.
      ;; NOTE: This is probably the most interesting feature of this function.
      (let ((heading-start (org-entry-beginning-position)))
        (goto-char (org-entry-end-position))
        (cond ((and (org-at-heading-p)
                    (= heading-start (org-entry-beginning-position)))
               ;; Entry ends on its heading; add newline after
               (end-of-line)
               (insert "\n\n"))
              (t
               ;; Entry ends after its heading; back up
               (forward-line -1)
               (end-of-line)
               (when (org-at-heading-p)
                 ;; At the same heading
                 (forward-line)
                 (insert "\n")
                 (forward-line -1))
               (while (not (looking-back "\\(?:[[:blank:]]?\n\\)\\{3\\}" nil))
                 (insert "\n"))
               (forward-line -1)))))

     ((org-at-item-checkbox-p)
      ;; Checkbox: Insert new item with checkbox.
      (org-insert-todo-heading nil))

     ((org-in-item-p)
      ;; Plain list.  Yes, this gets a little complicated...
      (let ((context (org-element-context)))
        (if (or (eq 'plain-list (car context))  ; First item in list
                (and (eq 'item (car context))
                     (not (eq (org-element-property :contents-begin context)
                              (org-element-property :contents-end context))))
                (unpackaged/org-element-descendant-of 'item context))  ; Element in list item, e.g. a link
            ;; Non-empty item: Add new item.
            (org-insert-item)
          ;; Empty item: Close the list.
          ;; TODO: Do this with org functions rather than operating on the text. Can't seem to find the right function.
          (delete-region (line-beginning-position) (line-end-position))
          (insert "\n"))))

     ((when (fboundp 'org-inlinetask-in-task-p)
        (org-inlinetask-in-task-p))
      ;; Inline task: Don't insert a new heading.
      (org-return t))

     ((org-at-table-p)
      (cond ((save-excursion
               (beginning-of-line)
               ;; See `org-table-next-field'.
               (cl-loop with end = (line-end-position)
                        for cell = (org-element-table-cell-parser)
                        always (equal (org-element-property :contents-begin cell)
                                      (org-element-property :contents-end cell))
                        while (re-search-forward "|" end t)))
             ;; Empty row: end the table.
             (delete-region (line-beginning-position) (line-end-position))
             (org-return t))
            (t
             ;; Non-empty row: call `org-return-indent'.
             (org-return t))))
     (t
      ;; All other cases: call `org-return-indent'.
      (org-return t)))))

(map!
 :after evil-org
 :map evil-org-mode-map
 :i [return] #'unpackaged/org-return-dwim)
(defun +yas/org-src-header-p ()
  "Determine whether `point' is within a src-block header or header-args."
  (pcase (org-element-type (org-element-context))
    ('src-block (< (point) ; before code part of the src-block
                   (save-excursion (goto-char (org-element-property :begin (org-element-context)))
                                   (forward-line 1)
                                   (point))))
    ('inline-src-block (< (point) ; before code part of the inline-src-block
                          (save-excursion (goto-char (org-element-property :begin (org-element-context)))
                                          (search-forward "]{")
                                          (point))))
    ('keyword (string-match-p "^header-args" (org-element-property :value (org-element-context))))))
(defun +yas/org-prompt-header-arg (arg question values)
  "Prompt the user to set ARG header property to one of VALUES with QUESTION.
The default value is identified and indicated. If either default is selected,
or no selection is made: nil is returned."
  (let* ((src-block-p (not (looking-back "^#\\+property:[ \t]+header-args:.*" (line-beginning-position))))
         (default
           (or
            (cdr (assoc arg
                        (if src-block-p
                            (nth 2 (org-babel-get-src-block-info t))
                          (org-babel-merge-params
                           org-babel-default-header-args
                           (let ((lang-headers
                                  (intern (concat "org-babel-default-header-args:"
                                                  (+yas/org-src-lang)))))
                             (when (boundp lang-headers) (eval lang-headers t)))))))
            ""))
         default-value)
    (setq values (mapcar
                  (lambda (value)
                    (if (string-match-p (regexp-quote value) default)
                        (setq default-value
                              (concat value " "
                                      (propertize "(default)" 'face 'font-lock-doc-face)))
                      value))
                  values))
    (let ((selection (consult--read values :prompt question :default default-value)))
      (unless (or (string-match-p "(default)$" selection)
                  (string= "" selection))
        selection))))
(defun +yas/org-src-lang ()
  "Try to find the current language of the src/header at `point'.
Return nil otherwise."
  (let ((context (org-element-context)))
    (pcase (org-element-type context)
      ('src-block (org-element-property :language context))
      ('inline-src-block (org-element-property :language context))
      ('keyword (when (string-match "^header-args:\\([^ ]+\\)" (org-element-property :value context))
                  (match-string 1 (org-element-property :value context)))))))

(defun +yas/org-last-src-lang ()
  "Return the language of the last src-block, if it exists."
  (save-excursion
    (beginning-of-line)
    (when (re-search-backward "^[ \t]*#\\+begin_src" nil t)
      (org-element-property :language (org-element-context)))))

(defun +yas/org-most-common-no-property-lang ()
  "Find the lang with the most source blocks that has no global header-args, else nil."
  (let (src-langs header-langs)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+begin_src" nil t)
        (push (+yas/org-src-lang) src-langs))
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+property: +header-args" nil t)
        (push (+yas/org-src-lang) header-langs)))

    (setq src-langs
          (mapcar #'car
                  ;; sort alist by frequency (desc.)
                  (sort
                   ;; generate alist with form (value . frequency)
                   (cl-loop for (n . m) in (seq-group-by #'identity src-langs)
                            collect (cons n (length m)))
                   (lambda (a b) (> (cdr a) (cdr b))))))

    (car (cl-set-difference src-langs header-langs :test #'string=))))
(defun org-syntax-convert-keyword-case-to-lower ()
  "Convert all #+KEYWORDS to #+keywords."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0)
          (case-fold-search nil))
      (while (re-search-forward "^[ \t]*#\\+[A-Z_]+" nil t)
        (unless (s-matches-p "RESULTS" (match-string 0))
          (replace-match (downcase (match-string 0)) t)
          (setq count (1+ count))))
      (message "Replaced %d occurances" count))))
(org-link-set-parameters "xkcd"
                         :image-data-fun #'+org-xkcd-image-fn
                         :follow #'+org-xkcd-open-fn
                         :export #'+org-xkcd-export
                         :complete #'+org-xkcd-complete)

(defun +org-xkcd-open-fn (link)
  (+org-xkcd-image-fn nil link nil))

(defun +org-xkcd-image-fn (protocol link description)
  "Get image data for xkcd num LINK"
  (let* ((xkcd-info (+xkcd-fetch-info (string-to-number link)))
         (img (plist-get xkcd-info :img))
         (alt (plist-get xkcd-info :alt)))
    (message alt)
    (+org-image-file-data-fn protocol (xkcd-download img (string-to-number link)) description)))

(defun +org-xkcd-export (num desc backend _com)
  "Convert xkcd to html/LaTeX form"
  (let* ((xkcd-info (+xkcd-fetch-info (string-to-number num)))
         (img (plist-get xkcd-info :img))
         (alt (plist-get xkcd-info :alt))
         (title (plist-get xkcd-info :title))
         (file (xkcd-download img (string-to-number num))))
    (cond ((org-export-derived-backend-p backend 'html)
           (format "<img class='invertible' src='%s' title=\"%s\" alt='%s'>" img (subst-char-in-string ?\" ?“ alt) title))
          ((org-export-derived-backend-p backend 'latex)
           (format "\\begin{figure}[!htb]
  \\centering
  \\includegraphics[scale=0.4]{%s}%s
\\end{figure}" file (if (equal desc (format "xkcd:%s" num)) ""
                      (format "\n  \\caption*{\\label{xkcd:%s} %s}"
                              num
                              (or desc
                                  (format "\\textbf{%s} %s" title alt))))))
          (t (format "https://xkcd.com/%s" num)))))

(defun +org-xkcd-complete (&optional arg)
  "Complete xkcd using `+xkcd-stored-info'"
  (format "xkcd:%d" (+xkcd-select)))
(org-link-set-parameters "yt" :export #'+org-export-yt)
(defun +org-export-yt (path desc backend _com)
  (cond ((org-export-derived-backend-p backend 'html)
         (format "<iframe width='440' \
height='335' \
src='https://www.youtube.com/embed/%s' \
frameborder='0' \
allowfullscreen>%s</iframe>" path (or "" desc)))
        ((org-export-derived-backend-p backend 'latex)
         (format "\\href{https://youtu.be/%s}{%s}" path (or desc "youtube")))
        (t (format "https://youtu.be/%s" path))))
(defadvice! shut-up-org-problematic-hooks (orig-fn &rest args)
  :around #'org-fancy-priorities-mode
  (ignore-errors (apply orig-fn args)))

(provide 'config-org-behaviour))))

;;:------------------------
;;; Org Citation
;;:------------------------

;; This block defines `org-ref-to-org-cite' and
;; `+org-cite-csl-activate/enable'.

(confpkg-with-record '("load: org-citation" config)
(use-package! oc-csl-activate
  :after oc
  :config
  (setq org-cite-csl-activate-use-document-style t)
  (defun +org-cite-csl-activate/enable ()
    (interactive)
    (setq org-cite-activate-processor 'csl-activate)
    (add-hook! 'org-mode-hook '((lambda () (cursor-sensor-mode 1)) org-cite-csl-activate-render-all))
    (defadvice! +org-cite-csl-activate-render-all-silent (orig-fn)
      :around #'org-cite-csl-activate-render-all
      (with-silent-modifications (funcall orig-fn)))
    (when (eq major-mode 'org-mode)
      (with-silent-modifications
        (save-excursion
          (goto-char (point-min))
          (org-cite-activate (point-max)))
        (org-cite-csl-activate-render-all)))
    (fmakunbound #'+org-cite-csl-activate/enable)))
(after! citar
  (setq org-cite-global-bibliography
        (let ((libfile-search-names '("library.json" "Library.json" "library.bib" "Library.bib"))
              (libfile-dir "~/Zotero")
              paths)
          (dolist (libfile libfile-search-names)
            (when (and (not paths)
                       (file-exists-p (expand-file-name libfile libfile-dir)))
              (setq paths (list (expand-file-name libfile libfile-dir)))))
          paths)
        citar-bibliography org-cite-global-bibliography
        citar-symbols
        `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " "))))
(after! oc-csl
  (setq org-cite-csl-styles-dir "~/Zotero/styles"))
(after! oc
  (setq org-cite-export-processors '((t csl))))
(map! :after org
      :map org-mode-map
      :localleader
      :desc "Insert citation" "@" #'org-cite-insert)
(after! oc
  (defun org-ref-to-org-cite ()
    "Attempt to convert org-ref citations to org-cite syntax."
    (interactive)
    (let* ((cite-conversions '(("cite" . "//b") ("Cite" . "//bc")
                               ("nocite" . "/n")
                               ("citep" . "") ("citep*" . "//f")
                               ("parencite" . "") ("Parencite" . "//c")
                               ("citeauthor" . "/a/f") ("citeauthor*" . "/a")
                               ("citeyear" . "/na/b")
                               ("Citep" . "//c") ("Citealp" . "//bc")
                               ("Citeauthor" . "/a/cf") ("Citeauthor*" . "/a/c")
                               ("autocite" . "") ("Autocite" . "//c")
                               ("notecite" . "/l/b") ("Notecite" . "/l/bc")
                               ("pnotecite" . "/l") ("Pnotecite" . "/l/bc")))
           (cite-regexp (rx (regexp (regexp-opt (mapcar #'car cite-conversions) t))
                            ":" (group (+ (not (any "\n 	,.)]}")))))))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward cite-regexp nil t)
          (message (format "[cite%s:@%s]"
                                 (cdr (assoc (match-string 1) cite-conversions))
                                 (match-string 2)))
          (replace-match (format "[cite%s:@%s]"
                                 (cdr (assoc (match-string 1) cite-conversions))
                                 (match-string 2))))))))

(provide 'config-org-citation))

;;:------------------------
;;; !Pkg Org Super Agenda
;;:------------------------

(confpkg-with-record '("load: pkg-org-super-agenda" config)
(use-package! org-super-agenda
  :commands org-super-agenda-mode)
(after! org-agenda
  (let ((inhibit-message t))
    (org-super-agenda-mode)))

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-compact-blocks t)

(setq org-agenda-custom-commands
      '(("o" "Overview"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                          :time-grid t
                          :date today
                          :todo "TODAY"
                          :scheduled today
                          :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Next to do"
                           :todo "NEXT"
                           :order 1)
                          (:name "Important"
                           :tag "Important"
                           :priority "A"
                           :order 6)
                          (:name "Due Today"
                           :deadline today
                           :order 2)
                          (:name "Due Soon"
                           :deadline future
                           :order 8)
                          (:name "Overdue"
                           :deadline past
                           :face error
                           :order 7)
                          (:name "Assignments"
                           :tag "Assignment"
                           :order 10)
                          (:name "Issues"
                           :tag "Issue"
                           :order 12)
                          (:name "Emacs"
                           :tag "Emacs"
                           :order 13)
                          (:name "Projects"
                           :tag "Project"
                           :order 14)
                          (:name "Research"
                           :tag "Research"
                           :order 15)
                          (:name "To read"
                           :tag "Read"
                           :order 30)
                          (:name "Waiting"
                           :todo "WAITING"
                           :order 20)
                          (:name "University"
                           :tag "uni"
                           :order 32)
                          (:name "Trivial"
                           :priority<= "E"
                           :tag ("Trivial" "Unimportant")
                           :todo ("SOMEDAY" )
                           :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))

(provide 'config--pkg-org-super-agenda))

;;:------------------------
;;; !Pkg org-roam
;;:------------------------

(confpkg-with-record '("hook: pkg-org-roam" set-hooks)
  (with-eval-after-load 'org-roam
(confpkg-with-record '("load: pkg-org-roam" load-hooks)
(use-package org-roam
  :defer t
  :config
  (setq org-roam-directory org-directory
        org-roam-dailies-directory "journals/"
        org-roam-dailies-capture-templates '(("d" "default" entry "* %?" :target
                                             (file+head "%<%Y_%m_%d>.org" "#+title: %<%Y-%m-%d>\n")))))
(defadvice! doom-modeline--buffer-file-name-roam-aware-a (orig-fun)
  :around #'doom-modeline-buffer-file-name ; takes no args
  (if (s-contains-p org-roam-directory (or buffer-file-name ""))
      (replace-regexp-in-string
       "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
       "🢔(\\1-\\2-\\3) "
       (subst-char-in-string ?_ ?  buffer-file-name))
    (funcall orig-fun)))

(provide 'config--pkg-org-roam))))

;;:------------------------
;;; !Pkg org-roam-ui
;;:------------------------

;; This block defines `org-roam-ui-open'.

(confpkg-with-record '("load: pkg-org-roam-ui" config)
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :commands org-roam-ui-open
  ;; :hook (org-roam . org-roam-ui-mode)
  :config
  (require 'org-roam) ; in case autoloaded
  (defun org-roam-ui-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (unless org-roam-ui-mode (org-roam-ui-mode 1))
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-ui-port))))

(provide 'config--pkg-org-roam-ui))

;;:------------------------
;;; Flycheck org-lint
;;:------------------------

(confpkg-with-record '("hook: flycheck-org-lint" set-hooks)
  (with-eval-after-load 'org
(confpkg-with-record '("load: flycheck-org-lint" load-hooks)
;; (defconst flycheck-org-lint-form
;;   (flycheck-prepare-emacs-lisp-form
;;     (require 'org)
;;     (require 'org-attach)
;;     (let ((source (car command-line-args-left))
;;           (process-default-directory default-directory))
;;       (with-temp-buffer
;;         (insert-file-contents source 'visit)
;;         (setq buffer-file-name source)
;;         (setq default-directory process-default-directory)
;;         (delay-mode-hooks (org-mode))
;;         (setq delayed-mode-hooks nil)
;;         (dolist (err (org-lint))
;;           (let ((inf (cl-second err)))
;;             (princ (elt inf 0))
;;             (princ ": ")
;;             (princ (elt inf 2))
;;             (terpri)))))))

;; (defconst flycheck-org-lint-variables
;;   '(org-directory
;;     org-id-locations
;;     org-id-locations-file
;;     org-attach-id-dir
;;     org-attach-use-inheritance
;;     org-attach-id-to-path-function-list
;;     org-link-parameters)
;;   "Variables inherited by the org-lint subprocess.")

;; (defun flycheck-org-lint-variables-form ()
;;   (require 'org-attach)  ; Needed to make variables available
;;   `(progn
;;      ,@(seq-map (lambda (opt) `(setq-default ,opt ',(symbol-value opt)))
;;                 (seq-filter #'boundp flycheck-org-lint-variables))))

;; (eval ; To preveant eager macro expansion form loading flycheck early.
;;  '(flycheck-define-checker org-lint
;;    "Org buffer checker using `org-lint'."
;;    :command ("emacs" (eval flycheck-emacs-args)
;;              "--eval" (eval (concat "(add-to-list 'load-path \""
;;                                     (file-name-directory (locate-library "org"))
;;                                     "\")"))
;;              "--eval" (eval (flycheck-sexp-to-string
;;                              (flycheck-org-lint-variables-form)))
;;              "--eval" (eval (flycheck-sexp-to-string
;;                              (flycheck-org-lint-customisations-form)))
;;              "--eval" (eval flycheck-org-lint-form)
;;              "--" source)
;;    :error-patterns
;;    ((error line-start line ": " (message) line-end))
;;    :modes org-mode))
;; (add-to-list 'flycheck-checkers 'org-lint)
;; (defun flycheck-org-lint-customisations-form ()
;;   `(progn
;;      (require 'ox)
;;      (cl-pushnew '(:latex-cover-page nil "coverpage" nil)
;;                  (org-export-backend-options (org-export-get-backend 'latex)))
;;      (cl-pushnew '(:latex-font-set nil "fontset" nil)
;;                  (org-export-backend-options (org-export-get-backend 'latex)))))

(provide 'config-flycheck-org-lint))))

;;:------------------------
;;; Org Visuals
;;:------------------------

;; This block defines `+org-plot-gnuplot-term-properties',
;; `+org-plot-generate-theme', `+org-plot-term-size',
;; `+org-refresh-latex-images-previews-h', and
;; `locally-defer-font-lock'.

(confpkg-with-record '("hook: org-visuals" set-hooks)
  (with-eval-after-load 'org
(confpkg-with-record '("load: org-visuals" load-hooks)
(add-hook 'org-mode-hook #'+org-pretty-mode)
(custom-set-faces!
  '(outline-1 :height 1.25)
  '(outline-2 :height 1.15)
  '(outline-3 :height 1.12)
  '(outline-4 :height 1.09)
  '(outline-5 :height 1.06)
  '(outline-6 :height 1.03)
  )
(custom-set-faces!
  '(org-document-title :height 1.2))
(setq org-agenda-deadline-faces
      '((1.001 . error)
        (1.0 . org-warning)
        (0.5 . org-upcoming-deadline)
        (0.0 . org-upcoming-distant-deadline)))
(setq org-fontify-quote-and-verse-blocks t)
(defun locally-defer-font-lock ()
  "Set jit-lock defer and stealth, when buffer is over a certain size."
  (when (> (buffer-size) 50000)
    (setq-local jit-lock-defer-time 0.05
                jit-lock-stealth-time 1)))

(add-hook 'org-mode-hook #'locally-defer-font-lock)
(defadvice! +org-indent--reduced-text-prefixes ()
  :after #'org-indent--compute-prefixes
  (setq org-indent--text-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (when (> org-indent-indentation-per-level 0)
    (dotimes (n org-indent--deepest-level)
      (aset org-indent--text-line-prefixes
            n
            (org-add-props
                (concat (make-string (* n (1- org-indent-indentation-per-level))
                                     ?\s)
                        (if (> n 0)
                             (char-to-string org-indent-boundary-char)
                          "\u200b"))
                nil 'face 'org-indent)))))
(setq org-inline-src-prettify-results '("⟨" . "⟩"))
(setq doom-themes-org-fontify-special-tags nil)
(setq org-ellipsis " ▾ "
      org-hide-leading-stars t
      org-priority-highest ?A
      org-priority-lowest ?E
      org-priority-faces
      '((?A . 'all-the-icons-red)
        (?B . 'all-the-icons-orange)
        (?C . 'all-the-icons-yellow)
        (?D . 'all-the-icons-green)
        (?E . 'all-the-icons-blue)))
(appendq! +ligatures-extra-symbols
          (list :list_property "∷"
                :em_dash       "—"
                :ellipses      "…"
                :arrow_right   "→"
                :arrow_left    "←"
                :arrow_lr      "↔"
                :properties    "⚙"
                :end           "∎"
                :priority_a    #("⚑" 0 1 (face all-the-icons-red))
                :priority_b    #("⬆" 0 1 (face all-the-icons-orange))
                :priority_c    #("■" 0 1 (face all-the-icons-yellow))
                :priority_d    #("⬇" 0 1 (face all-the-icons-green))
                :priority_e    #("❓" 0 1 (face all-the-icons-blue))))

(defadvice! +org-init-appearance-h--no-ligatures-a ()
  :after #'+org-init-appearance-h
  (set-ligatures! 'org-mode nil)
  (set-ligatures! 'org-mode
    :list_property "::"
    :em_dash       "---"
    :ellipsis      "..."
    :arrow_right   "->"
    :arrow_left    "<-"
    :arrow_lr      "<->"
    :properties    ":PROPERTIES:"
    :end           ":END:"
    :priority_a    "[#A]"
    :priority_b    "[#B]"
    :priority_c    "[#C]"
    :priority_d    "[#D]"
    :priority_e    "[#E]"))
;; (package! org-pretty-tags :pin "5c7521651b35ae9a7d3add4a66ae8cc176ae1c76")
;; (use-package org-pretty-tags
;; :config
;;  (setq org-pretty-tags-surrogate-strings
;;        `(("uni"        . ,(all-the-icons-faicon   "graduation-cap" :face 'all-the-icons-purple  :v-adjust 0.01))
;;          ("ucc"        . ,(all-the-icons-material "computer"       :face 'all-the-icons-silver  :v-adjust 0.01))
;;          ("assignment" . ,(all-the-icons-material "library_books"  :face 'all-the-icons-orange  :v-adjust 0.01))
;;          ("test"       . ,(all-the-icons-material "timer"          :face 'all-the-icons-red     :v-adjust 0.01))
;;          ("lecture"    . ,(all-the-icons-fileicon "keynote"        :face 'all-the-icons-orange  :v-adjust 0.01))
;;          ("email"      . ,(all-the-icons-faicon   "envelope"       :face 'all-the-icons-blue    :v-adjust 0.01))
;;          ("read"       . ,(all-the-icons-octicon  "book"           :face 'all-the-icons-lblue   :v-adjust 0.01))
;;          ("article"    . ,(all-the-icons-octicon  "file-text"      :face 'all-the-icons-yellow  :v-adjust 0.01))
;;          ("web"        . ,(all-the-icons-faicon   "globe"          :face 'all-the-icons-green   :v-adjust 0.01))
;;          ("info"       . ,(all-the-icons-faicon   "info-circle"    :face 'all-the-icons-blue    :v-adjust 0.01))
;;          ("issue"      . ,(all-the-icons-faicon   "bug"            :face 'all-the-icons-red     :v-adjust 0.01))
;;          ("someday"    . ,(all-the-icons-faicon   "calendar-o"     :face 'all-the-icons-cyan    :v-adjust 0.01))
;;          ("idea"       . ,(all-the-icons-octicon  "light-bulb"     :face 'all-the-icons-yellow  :v-adjust 0.01))
;;          ("emacs"      . ,(all-the-icons-fileicon "emacs"          :face 'all-the-icons-lpurple :v-adjust 0.01))))
;;  (org-pretty-tags-global-mode))
(setq org-highlight-latex-and-related '(native script entities))
(require 'org-src)
(add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))
;; (add-hook 'org-mode-hook #'org-latex-preview-auto-mode)
(setq org-latex-preview-header
      (concat
       "\\documentclass{article}
[DEFAULT-PACKAGES]
[PACKAGES]
\\usepackage{xcolor}"
       "\n% Custom font\n\\usepackage{arev}\n\n"
       "%% Maths-related packages
% More maths environments, commands, and symbols.
\\usepackage{amsmath, amssymb}
% Slanted fractions with \\sfrac{a}{b}, in text and maths.
\\usepackage{xfrac}
% Visually cancel expressions with \\cancel{value} and \\cancelto{expression}{value}
\\usepackage[makeroom]{cancel}
% Improvements on amsmath and utilities for mathematical typesetting
\\usepackage{mathtools}

% Deliminators
\\DeclarePairedDelimiter{\\abs}{\\lvert}{\\rvert}
\\DeclarePairedDelimiter{\\norm}{\\lVert}{\\rVert}

\\DeclarePairedDelimiter{\\ceil}{\\lceil}{\\rceil}
\\DeclarePairedDelimiter{\\floor}{\\lfloor}{\\rfloor}
\\DeclarePairedDelimiter{\\round}{\\lfloor}{\\rceil}

\\newcommand{\\RR}[1][]{\\ensuremath{\\ifstrempty{#1}{\\mathbb{R}}{\\mathbb{R}^{#1}}}} % Real numbers
\\newcommand{\\NN}[1][]{\\ensuremath{\\ifstrempty{#1}{\\mathbb{N}}{\\mathbb{N}^{#1}}}} % Natural numbers
\\newcommand{\\ZZ}[1][]{\\ensuremath{\\ifstrempty{#1}{\\mathbb{Z}}{\\mathbb{Z}^{#1}}}} % Integer numbers
\\newcommand{\\QQ}[1][]{\\ensuremath{\\ifstrempty{#1}{\\mathbb{Q}}{\\mathbb{Q}^{#1}}}} % Rational numbers
\\newcommand{\\CC}[1][]{\\ensuremath{\\ifstrempty{#1}{\\mathbb{C}}{\\mathbb{C}^{#1}}}} % Complex numbers

% Easy derivatives
\\ProvideDocumentCommand\\dv{o m g}{%
  \\IfNoValueTF{#3}{%
    \\dv[#1]{}{#2}}{%
    \\IfNoValueTF{#1}{%
      \\frac{\\dd #2}{\\dd #3}%
    }{\\frac{\\dd[#1] #2}{\\dd {#3}^{#1}}}}}
% Easy partial derivatives
\\ExplSyntaxOn
\\ProvideDocumentCommand\\pdv{o m g}{%
  \\IfNoValueTF{#3}{\\pdv[#1]{}{#2}}%
  {\\ifnum\\clist_count:n{#3}<2
    \\IfValueTF{#1}{\\frac{\\partial^{#1} #2}{\\partial {#3}^{#1}}}%
    {\\frac{\\partial #2}{\\partial #3}}
    \\else
    \\frac{\\IfValueTF{#1}{\\partial^{#1}}{\\partial^{\\clist_count:n{#3}}}#2}%
    {\\clist_map_inline:nn{#3}{\\partial ##1 \\,}\\!}
    \\fi}}
\\ExplSyntaxOff

% Laplacian
\\DeclareMathOperator{\\Lap}{\\mathcal{L}}

% Statistics
\\DeclareMathOperator{\\Var}{Var} % varience
\\DeclareMathOperator{\\Cov}{Cov} % covarience
\\newcommand{\\EE}{\\ensuremath{\\mathbb{E}}} % expected value
\\DeclareMathOperator{\\E}{E} % expected value

% I prefer the slanted \\leq/\\geq
\\let\\barleq\\leq % Save them in case they're every wanted
\\let\\bargeq\\geq
\\renewcommand{\\leq}{\\leqslant}
\\renewcommand{\\geq}{\\geqslant}

% Redefine the matrix environment to allow for alignment
% via an optional argument, and use r as the default.
\\makeatletter
\\renewcommand*\\env@matrix[1][r]{\\hskip -\\arraycolsep%
    \\let\\@ifnextchar\\new@ifnextchar
    \\array{*\\c@MaxMatrixCols #1}}
\\makeatother

% Slanted roman \"d\" for derivatives
\\ifcsname pdfoutput\\endcsname
  \\ifnum\\pdfoutput>0 % PDF
    \\newsavebox\\diffdbox{}
    \\newcommand{\\slantedromand}{{\\mathpalette\\makesl{d}}}
    \\newcommand{\\makesl}[2]{%
      \\begingroup
      \\sbox{\\diffdbox}{$\\mathsurround=0pt#1\\mathrm{#2}$}%
      \\pdfsave%
      \\pdfsetmatrix{1 0 0.2 1}%
      \\rlap{\\usebox{\\diffdbox}}%
      \\pdfrestore%
      \\hskip\\wd\\diffdbox%
      \\endgroup}
  \\else % DVI
    \\newcommand{\\slantedromand}{d} % fallback
  \\fi
\\else % Also DVI
  \\newcommand{\\slantedromand}{d} % fallback
\\fi

% Derivative d^n, nicely spaced
\\makeatletter
\\newcommand{\\dd}[1][]{\\mathop{}\\!%
  \\expandafter\\ifx\\expandafter&\\detokenize{#1}&% \\ifstrempty from etoolbox
    \\slantedromand\\@ifnextchar^{\\hspace{0.2ex}}{\\hspace{0.1ex}}
  \\else
    \\slantedromand\\hspace{0.2ex}^{#1}
  \\fi}
\\makeatother

\\NewCommandCopy{\\daccent}{\\d}
\\renewcommand{\\d}{\\ifmmode\\dd\\else\\daccent\\fi}"))
(plist-put org-format-latex-options :background "Transparent")
;; (plist-put org-format-latex-options :zoom 0.95) ; Calibrated based on the TeX font and org-buffer font.
(defun +org-refresh-latex-images-previews-h ()
  (dolist (buffer (doom-buffers-in-mode 'org-mode (buffer-list)))
    (with-current-buffer buffer
      (+org--toggle-inline-images-in-subtree (point-min) (point-max) 'refresh)
      (unless (eq org-latex-preview-default-process 'dvisvgm)
        (org-clear-latex-preview (point-min) (point-max))
        (org--latex-preview-region (point-min) (point-max))))))

(add-hook 'doom-load-theme-hook #'+org-refresh-latex-images-previews-h)
(defvar +org-plot-term-size '(1050 . 650)
  "The size of the GNUPlot terminal, in the form (WIDTH . HEIGHT).")

(after! org-plot
  (defun +org-plot-generate-theme (_type)
    "Use the current Doom theme colours to generate a GnuPlot preamble."
    (format "
fgt = \"textcolor rgb '%s'\" # foreground text
fgat = \"textcolor rgb '%s'\" # foreground alt text
fgl = \"linecolor rgb '%s'\" # foreground line
fgal = \"linecolor rgb '%s'\" # foreground alt line

# foreground colors
set border lc rgb '%s'
# change text colors of  tics
set xtics @fgt
set ytics @fgt
# change text colors of labels
set title @fgt
set xlabel @fgt
set ylabel @fgt
# change a text color of key
set key @fgt

# line styles
set linetype 1 lw 2 lc rgb '%s' # red
set linetype 2 lw 2 lc rgb '%s' # blue
set linetype 3 lw 2 lc rgb '%s' # green
set linetype 4 lw 2 lc rgb '%s' # magenta
set linetype 5 lw 2 lc rgb '%s' # orange
set linetype 6 lw 2 lc rgb '%s' # yellow
set linetype 7 lw 2 lc rgb '%s' # teal
set linetype 8 lw 2 lc rgb '%s' # violet

# border styles
set tics out nomirror
set border 3

# palette
set palette maxcolors 8
set palette defined ( 0 '%s',\
1 '%s',\
2 '%s',\
3 '%s',\
4 '%s',\
5 '%s',\
6 '%s',\
7 '%s' )
"
            (doom-color 'fg)
            (doom-color 'fg-alt)
            (doom-color 'fg)
            (doom-color 'fg-alt)
            (doom-color 'fg)
            ;; colours
            (doom-color 'red)
            (doom-color 'blue)
            (doom-color 'green)
            (doom-color 'magenta)
            (doom-color 'orange)
            (doom-color 'yellow)
            (doom-color 'teal)
            (doom-color 'violet)
            ;; duplicated
            (doom-color 'red)
            (doom-color 'blue)
            (doom-color 'green)
            (doom-color 'magenta)
            (doom-color 'orange)
            (doom-color 'yellow)
            (doom-color 'teal)
            (doom-color 'violet)))

  (defun +org-plot-gnuplot-term-properties (_type)
    (format "background rgb '%s' size %s,%s"
            (doom-color 'bg) (car +org-plot-term-size) (cdr +org-plot-term-size)))

  (setq org-plot/gnuplot-script-preamble #'+org-plot-generate-theme)
  (setq org-plot/gnuplot-term-extra #'+org-plot-gnuplot-term-properties))

(provide 'config-org-visuals))))

;;:------------------------
;;; Org exports
;;:------------------------

;; This block defines `+org-export-babel-mask-org-config',
;; `+org-mode--fontlock-only-mode',
;; `org-latex-format-headline-acronymised',
;; `org-html-format-headline-acronymised', and
;; `org-export-filter-text-acronym'.

(confpkg-with-record '("hook: org-exports" set-hooks)
  (with-eval-after-load 'ox
(confpkg-with-record '("load: org-exports" load-hooks)
(setq org-export-headline-levels 5) ; I like nesting
(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))
(setq org-export-creator-string
      (format "Emacs %s (Org mode %s–%s)" emacs-version (org-release) (org-git-version)))
(defun org-export-filter-text-acronym (text backend _info)
  "Wrap suspected acronyms in acronyms-specific formatting.
Treat sequences of 2+ capital letters (optionally succeeded by \"s\") as an acronym.
Ignore if preceeded by \";\" (for manual prevention) or \"\\\" (for LaTeX commands).

TODO abstract backend implementations."
  (let ((base-backend
         (cond
          ((org-export-derived-backend-p backend 'latex) 'latex)
          ;; Markdown is derived from HTML, but we don't want to format it
          ((org-export-derived-backend-p backend 'md) nil)
          ((org-export-derived-backend-p backend 'html) 'html)))
        (case-fold-search nil))
    (when base-backend
      (replace-regexp-in-string
       "[;\\\\]?\\b[A-Z][A-Z]+s?\\(?:[^A-Za-z]\\|\\b\\)"
       (lambda (all-caps-str)
         (cond ((equal (aref all-caps-str 0) ?\\) all-caps-str)                ; don't format LaTeX commands
               ((equal (aref all-caps-str 0) ?\;) (substring all-caps-str 1))  ; just remove not-acronym indicator char ";"
               (t (let* ((final-char (if (string-match-p "[^A-Za-z]" (substring all-caps-str -1 (length all-caps-str)))
                                         (substring all-caps-str -1 (length all-caps-str))
                                       nil)) ; needed to re-insert the [^A-Za-z] at the end
                         (trailing-s (equal (aref all-caps-str (- (length all-caps-str) (if final-char 2 1))) ?s))
                         (acr (if final-char
                                  (substring all-caps-str 0 (if trailing-s -2 -1))
                                (substring all-caps-str 0 (+ (if trailing-s -1 (length all-caps-str)))))))
                    (pcase base-backend
                      ('latex (concat "\\acr{" (s-downcase acr) "}" (when trailing-s "\\acrs{}") final-char))
                      ('html (concat "<span class='acr'>" acr "</span>" (when trailing-s "<small>s</small>") final-char)))))))
       text t t))))

(add-to-list 'org-export-filter-plain-text-functions
             #'org-export-filter-text-acronym)

;; We won't use `org-export-filter-headline-functions' because it
;; passes (and formats) the entire section contents. That's no good.

(defun org-html-format-headline-acronymised (todo todo-type priority text tags info)
  "Like `org-html-format-headline-default-function', but with acronym formatting."
  (org-html-format-headline-default-function
   todo todo-type priority (org-export-filter-text-acronym text 'html info) tags info))
(setq org-html-format-headline-function #'org-html-format-headline-acronymised)

(defun org-latex-format-headline-acronymised (todo todo-type priority text tags info)
  "Like `org-latex-format-headline-default-function', but with acronym formatting."
  (org-latex-format-headline-default-function
   todo todo-type priority (org-export-filter-text-acronym text 'latex info) tags info))
(setq org-latex-format-headline-function #'org-latex-format-headline-acronymised)
(defun +org-mode--fontlock-only-mode ()
  "Just apply org-mode's font-lock once."
  (let (org-mode-hook
        org-hide-leading-stars
        org-hide-emphasis-markers)
    (org-set-font-lock-defaults)
    (font-lock-ensure))
  (setq-local major-mode #'fundamental-mode))

(defun +org-export-babel-mask-org-config (_backend)
  "Use `+org-mode--fontlock-only-mode' instead of `org-mode'."
  (setq-local org-src-lang-modes
              (append org-src-lang-modes
                      (list (cons "org" #'+org-mode--fontlock-only)))))

(add-hook 'org-export-before-processing-hook #'+org-export-babel-mask-org-config)

(provide 'config-org-exports))))

;;:------------------------
;;; ox-html
;;:------------------------

;; This block defines `org-url-unfurl-metadata',
;; `org-url-fancy-export', `org-export-html-headline-anchor',
;; `org-html-block-collapsable', `mode-name-to-lang-name',
;; `org-html-export-collapsed', `org-html-reload-fancy-style',
;; `org-html-meta-tags-fancy', and
;; `org-html-meta-tags-opengraph-image'.

(confpkg-with-record '("hook: ox-html" set-hooks)
  (with-eval-after-load 'ox-html
(confpkg-with-record '("load: ox-html" load-hooks)
(define-minor-mode org-fancy-html-export-mode
  "Toggle my fabulous org export tweaks. While this mode itself does a little bit,
the vast majority of the change in behaviour comes from switch statements in:
 - `org-html-template-fancier'
 - `org-html--build-meta-info-extended'
 - `org-html-src-block-collapsable'
 - `org-html-block-collapsable'
 - `org-html-table-wrapped'
 - `org-html--format-toc-headline-colapseable'
 - `org-html--toc-text-stripped-leaves'
 - `org-export-html-headline-anchor'"
  :global t
  :init-value t
  (if org-fancy-html-export-mode
      (setq org-html-style-default org-html-style-fancy
            org-html-meta-tags #'org-html-meta-tags-fancy
            org-html-checkbox-type 'html-span)
    (setq org-html-style-default org-html-style-plain
          org-html-meta-tags #'org-html-meta-tags-default
          org-html-checkbox-type 'html)))
(defadvice! org-html-template-fancier (orig-fn contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options. Adds a few extra things to the body
compared to the default implementation."
  :around #'org-html-template
  (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
      (funcall orig-fn contents info)
    (concat
     (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
       (let* ((xml-declaration (plist-get info :html-xml-declaration))
              (decl (or (and (stringp xml-declaration) xml-declaration)
                        (cdr (assoc (plist-get info :html-extension)
                                    xml-declaration))
                        (cdr (assoc "html" xml-declaration))
                        "")))
         (when (not (or (not decl) (string= "" decl)))
           (format "%s\n"
                   (format decl
                           (or (and org-html-coding-system
                                    (fboundp 'coding-system-get)
                                    (coding-system-get org-html-coding-system 'mime-charset))
                               "iso-8859-1"))))))
     (org-html-doctype info)
     "\n"
     (concat "<html"
             (cond ((org-html-xhtml-p info)
                    (format
                     " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
                     (plist-get info :language) (plist-get info :language)))
                   ((org-html-html5-p info)
                    (format " lang=\"%s\"" (plist-get info :language))))
             ">\n")
     "<head>\n"
     (org-html--build-meta-info info)
     (org-html--build-head info)
     (org-html--build-mathjax-config info)
     "</head>\n"
     "<body>\n<input type='checkbox' id='theme-switch'><div id='page'><label id='switch-label' for='theme-switch'></label>"
     (let ((link-up (org-trim (plist-get info :html-link-up)))
           (link-home (org-trim (plist-get info :html-link-home))))
       (unless (and (string= link-up "") (string= link-home ""))
         (format (plist-get info :html-home/up-format)
                 (or link-up link-home)
                 (or link-home link-up))))
     ;; Preamble.
     (org-html--build-pre/postamble 'preamble info)
     ;; Document contents.
     (let ((div (assq 'content (plist-get info :html-divs))))
       (format "<%s id=\"%s\">\n" (nth 1 div) (nth 2 div)))
     ;; Document title.
     (when (plist-get info :with-title)
       (let ((title (and (plist-get info :with-title)
                         (plist-get info :title)))
             (subtitle (plist-get info :subtitle))
             (html5-fancy (org-html--html5-fancy-p info)))
         (when title
           (format
            (if html5-fancy
                "<header class=\"page-header\">%s\n<h1 class=\"title\">%s</h1>\n%s</header>"
              "<h1 class=\"title\">%s%s</h1>\n")
            (if (or (plist-get info :with-date)
                    (plist-get info :with-author))
                (concat "<div class=\"page-meta\">"
                        (when (plist-get info :with-date)
                          (org-export-data (plist-get info :date) info))
                        (when (and (plist-get info :with-date) (plist-get info :with-author)) ", ")
                        (when (plist-get info :with-author)
                          (org-export-data (plist-get info :author) info))
                        "</div>\n")
              "")
            (org-export-data title info)
            (if subtitle
                (format
                 (if html5-fancy
                     "<p class=\"subtitle\" role=\"doc-subtitle\">%s</p>\n"
                   (concat "\n" (org-html-close-tag "br" nil info) "\n"
                           "<span class=\"subtitle\">%s</span>\n"))
                 (org-export-data subtitle info))
              "")))))
     contents
     (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
     ;; Postamble.
     (org-html--build-pre/postamble 'postamble info)
     ;; Possibly use the Klipse library live code blocks.
     (when (plist-get info :html-klipsify-src)
       (concat "<script>" (plist-get info :html-klipse-selection-script)
               "</script><script src=\""
               org-html-klipse-js
               "\"></script><link rel=\"stylesheet\" type=\"text/css\" href=\""
               org-html-klipse-css "\"/>"))
     ;; Closing document.
     "</div>\n</body>\n</html>")))
(defadvice! org-html-toc-linked (depth info &optional scope)
  "Build a table of contents.

Just like `org-html-toc', except the header is a link to \"#\".

DEPTH is an integer specifying the depth of the table.  INFO is
a plist used as a communication channel.  Optional argument SCOPE
is an element defining the scope of the table.  Return the table
of contents as a string, or nil if it is empty."
  :override #'org-html-toc
  (let ((toc-entries
         (mapcar (lambda (headline)
                   (cons (org-html--format-toc-headline headline info)
                         (org-export-get-relative-level headline info)))
                 (org-export-collect-headlines info depth scope))))
    (when toc-entries
      (let ((toc (concat "<div id=\"text-table-of-contents\">"
                         (org-html--toc-text toc-entries)
                         "</div>\n")))
        (if scope toc
          (let ((outer-tag (if (org-html--html5-fancy-p info)
                               "nav"
                             "div")))
            (concat (format "<%s id=\"table-of-contents\">\n" outer-tag)
                    (let ((top-level (plist-get info :html-toplevel-hlevel)))
                      (format "<h%d><a href=\"#\" style=\"color:inherit; text-decoration: none;\">%s</a></h%d>\n"
                              top-level
                              (org-html--translate "Table of Contents" info)
                              top-level))
                    toc
                    (format "</%s>\n" outer-tag))))))))
(defvar org-html-meta-tags-opengraph-image
  '(:image "https://tecosaur.com/resources/org/nib.png"
    :type "image/png"
    :width "200"
    :height "200"
    :alt "Green fountain pen nib")
  "Plist of og:image:PROP properties and their value, for use in `org-html-meta-tags-fancy'.")

(defun org-html-meta-tags-fancy (info)
  "Use the INFO plist to construct the meta tags, as described in `org-html-meta-tags'."
  (let ((title (org-html-plain-text
                (org-element-interpret-data (plist-get info :title)) info))
        (author (and (plist-get info :with-author)
                     (let ((auth (plist-get info :author)))
                       ;; Return raw Org syntax.
                       (and auth (org-html-plain-text
                                  (org-element-interpret-data auth) info))))))
    (append
     (list
      (when (org-string-nw-p author)
        (list "name" "author" author))
      (when (org-string-nw-p (plist-get info :description))
        (list "name" "description"
              (plist-get info :description)))
      '("name" "generator" "org mode")
      '("name" "theme-color" "#77aa99")
      '("property" "og:type" "article")
      (list "property" "og:title" title)
      (let ((subtitle (org-export-data (plist-get info :subtitle) info)))
        (when (org-string-nw-p subtitle)
          (list "property" "og:description" subtitle))))
     (when org-html-meta-tags-opengraph-image
       (list (list "property" "og:image" (plist-get org-html-meta-tags-opengraph-image :image))
             (list "property" "og:image:type" (plist-get org-html-meta-tags-opengraph-image :type))
             (list "property" "og:image:width" (plist-get org-html-meta-tags-opengraph-image :width))
             (list "property" "og:image:height" (plist-get org-html-meta-tags-opengraph-image :height))
             (list "property" "og:image:alt" (plist-get org-html-meta-tags-opengraph-image :alt))))
     (list
      (when (org-string-nw-p author)
        (list "property" "og:article:author:first_name" (car (s-split-up-to " " author 2))))
      (when (and (org-string-nw-p author) (s-contains-p " " author))
        (list "property" "og:article:author:last_name" (cadr (s-split-up-to " " author 2))))
      (list "property" "og:article:published_time"
            (format-time-string
             "%FT%T%z"
             (or
              (when-let ((date-str (cadar (org-collect-keywords '("DATE")))))
                (unless (string= date-str (format-time-string "%F"))
                  (ignore-errors (encode-time (org-parse-time-string date-str)))))
              (if buffer-file-name
                  (file-attribute-modification-time (file-attributes buffer-file-name))
                (current-time)))))
      (when buffer-file-name
        (list "property" "og:article:modified_time"
              (format-time-string "%FT%T%z" (file-attribute-modification-time (file-attributes buffer-file-name)))))))))

(unless (functionp #'org-html-meta-tags-default)
  (defalias 'org-html-meta-tags-default #'ignore))
(setq org-html-meta-tags #'org-html-meta-tags-fancy)
(setq org-html-style-plain org-html-style-default
      org-html-htmlize-output-type 'css
      org-html-doctype "html5"
      org-html-html5-fancy t)

(defun org-html-reload-fancy-style ()
  (interactive)
  (setq org-html-style-fancy
        (concat (f-read-text (expand-file-name "misc/org-export-header.html" doom-user-dir))
                "<script>\n"
                (f-read-text (expand-file-name "misc/org-css/main.js" doom-user-dir))
                "</script>\n<style>\n"
                (f-read-text (expand-file-name "misc/org-css/main.min.css" doom-user-dir))
                "</style>"))
  (when org-fancy-html-export-mode
    (setq org-html-style-default org-html-style-fancy)))
(org-html-reload-fancy-style)
(defvar org-html-export-collapsed nil)
(eval '(cl-pushnew '(:collapsed "COLLAPSED" "collapsed" org-html-export-collapsed t)
                   (org-export-backend-options (org-export-get-backend 'html))))
(add-to-list 'org-default-properties "EXPORT_COLLAPSED")
(defadvice! org-html-src-block-collapsable (orig-fn src-block contents info)
  "Wrap the usual <pre> block in a <details>"
  :around #'org-html-src-block
  (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
      (funcall orig-fn src-block contents info)
    (let* ((properties (cadr src-block))
           (lang (mode-name-to-lang-name
                  (plist-get properties :language)))
           (name (plist-get properties :name))
           (ref (org-export-get-reference src-block info))
           (collapsed-p (member (or (org-export-read-attribute :attr_html src-block :collapsed)
                                    (plist-get info :collapsed))
                                '("y" "yes" "t" t "true" "all"))))
      (format
       "<details id='%s' class='code'%s><summary%s>%s</summary>
<div class='gutter'>
<a href='#%s'>#</a>
<button title='Copy to clipboard' onclick='copyPreToClipbord(this)'>⎘</button>\
</div>
%s
</details>"
       ref
       (if collapsed-p "" " open")
       (if name " class='named'" "")
       (concat
        (when name (concat "<span class=\"name\">" name "</span>"))
        "<span class=\"lang\">" lang "</span>")
       ref
       (if name
           (replace-regexp-in-string (format "<pre\\( class=\"[^\"]+\"\\)? id=\"%s\">" ref) "<pre\\1>"
                                     (funcall orig-fn src-block contents info))
         (funcall orig-fn src-block contents info))))))

(defun mode-name-to-lang-name (mode)
  (or (cadr (assoc mode
                   '(("asymptote" "Asymptote")
                     ("awk" "Awk")
                     ("C" "C")
                     ("clojure" "Clojure")
                     ("css" "CSS")
                     ("D" "D")
                     ("ditaa" "ditaa")
                     ("dot" "Graphviz")
                     ("calc" "Emacs Calc")
                     ("emacs-lisp" "Emacs Lisp")
                     ("fortran" "Fortran")
                     ("gnuplot" "gnuplot")
                     ("haskell" "Haskell")
                     ("hledger" "hledger")
                     ("java" "Java")
                     ("js" "Javascript")
                     ("latex" "LaTeX")
                     ("ledger" "Ledger")
                     ("lisp" "Lisp")
                     ("lilypond" "Lilypond")
                     ("lua" "Lua")
                     ("matlab" "MATLAB")
                     ("mscgen" "Mscgen")
                     ("ocaml" "Objective Caml")
                     ("octave" "Octave")
                     ("org" "Org mode")
                     ("oz" "OZ")
                     ("plantuml" "Plantuml")
                     ("processing" "Processing.js")
                     ("python" "Python")
                     ("R" "R")
                     ("ruby" "Ruby")
                     ("sass" "Sass")
                     ("scheme" "Scheme")
                     ("screen" "Gnu Screen")
                     ("sed" "Sed")
                     ("sh" "shell")
                     ("sql" "SQL")
                     ("sqlite" "SQLite")
                     ("forth" "Forth")
                     ("io" "IO")
                     ("J" "J")
                     ("makefile" "Makefile")
                     ("maxima" "Maxima")
                     ("perl" "Perl")
                     ("picolisp" "Pico Lisp")
                     ("scala" "Scala")
                     ("shell" "Shell Script")
                     ("ebnf2ps" "ebfn2ps")
                     ("cpp" "C++")
                     ("abc" "ABC")
                     ("coq" "Coq")
                     ("groovy" "Groovy")
                     ("bash" "bash")
                     ("csh" "csh")
                     ("ash" "ash")
                     ("dash" "dash")
                     ("ksh" "ksh")
                     ("mksh" "mksh")
                     ("posh" "posh")
                     ("ada" "Ada")
                     ("asm" "Assembler")
                     ("caml" "Caml")
                     ("delphi" "Delphi")
                     ("html" "HTML")
                     ("idl" "IDL")
                     ("mercury" "Mercury")
                     ("metapost" "MetaPost")
                     ("modula-2" "Modula-2")
                     ("pascal" "Pascal")
                     ("ps" "PostScript")
                     ("prolog" "Prolog")
                     ("simula" "Simula")
                     ("tcl" "tcl")
                     ("tex" "LaTeX")
                     ("plain-tex" "TeX")
                     ("verilog" "Verilog")
                     ("vhdl" "VHDL")
                     ("xml" "XML")
                     ("nxml" "XML")
                     ("conf" "Configuration File"))))
      mode))
(defun org-html-block-collapsable (orig-fn block contents info)
  "Wrap the usual block in a <details>"
  (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
      (funcall orig-fn block contents info)
    (let ((ref (org-export-get-reference block info))
          (type (pcase (car block)
                  ('property-drawer "Properties")))
          (collapsed-default (pcase (car block)
                               ('property-drawer t)
                               (_ nil)))
          (collapsed-value (org-export-read-attribute :attr_html block :collapsed))
          (collapsed-p (or (member (org-export-read-attribute :attr_html block :collapsed)
                                   '("y" "yes" "t" t "true"))
                           (member (plist-get info :collapsed) '("all")))))
      (format
       "<details id='%s' class='code'%s>
<summary%s>%s</summary>
<div class='gutter'>\
<a href='#%s'>#</a>
<button title='Copy to clipboard' onclick='copyPreToClipbord(this)'>⎘</button>\
</div>
%s\n
</details>"
       ref
       (if (or collapsed-p collapsed-default) "" " open")
       (if type " class='named'" "")
       (if type (format "<span class='type'>%s</span>" type) "")
       ref
       (funcall orig-fn block contents info)))))

(advice-add 'org-html-example-block   :around #'org-html-block-collapsable)
(advice-add 'org-html-fixed-width     :around #'org-html-block-collapsable)
(advice-add 'org-html-property-drawer :around #'org-html-block-collapsable)
(autoload #'highlight-numbers--turn-on "highlight-numbers")
(add-hook 'htmlize-before-hook #'highlight-numbers--turn-on)
(defadvice! org-html-table-wrapped (orig-fn table contents info)
  "Wrap the usual <table> in a <div>"
  :around #'org-html-table
  (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
      (funcall orig-fn table contents info)
    (let* ((name (plist-get (cadr table) :name))
           (ref (org-export-get-reference table info)))
      (format "<div id='%s' class='table'>
<div class='gutter'><a href='#%s'>#</a></div>
<div class='tabular'>
%s
</div>\
</div>"
              ref ref
              (if name
                  (replace-regexp-in-string (format "<table id=\"%s\"" ref) "<table"
                                            (funcall orig-fn table contents info))
                (funcall orig-fn table contents info))))))
(defadvice! org-html--format-toc-headline-colapseable (orig-fn headline info)
  "Add a label and checkbox to `org-html--format-toc-headline's usual output,
to allow the TOC to be a collapseable tree."
  :around #'org-html--format-toc-headline
  (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
      (funcall orig-fn headline info)
    (let ((id (or (org-element-property :CUSTOM_ID headline)
                  (org-export-get-reference headline info))))
      (format "<input type='checkbox' id='toc--%s'/><label for='toc--%s'>%s</label>"
              id id (funcall orig-fn headline info)))))
(defadvice! org-html--toc-text-stripped-leaves (orig-fn toc-entries)
  "Remove label"
  :around #'org-html--toc-text
  (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
      (funcall orig-fn toc-entries)
    (replace-regexp-in-string "<input [^>]+><label [^>]+>\\(.+?\\)</label></li>" "\\1</li>"
                              (funcall orig-fn toc-entries))))
(setq org-html-text-markup-alist
      '((bold . "<b>%s</b>")
        (code . "<code>%s</code>")
        (italic . "<i>%s</i>")
        (strike-through . "<del>%s</del>")
        (underline . "<span class=\"underline\">%s</span>")
        (verbatim . "<kbd>%s</kbd>")))
(appendq! org-html-checkbox-types
          '((html-span
             (on . "<span class='checkbox'></span>")
             (off . "<span class='checkbox'></span>")
             (trans . "<span class='checkbox'></span>"))))
(setq org-html-checkbox-type 'html-span)
(pushnew! org-html-special-string-regexps
          '("-&gt;" . "&#8594;")
          '("&lt;-" . "&#8592;"))
(defun org-export-html-headline-anchor (text backend info)
  (when (and (org-export-derived-backend-p backend 'html)
             (not (org-export-derived-backend-p backend 're-reveal))
             org-fancy-html-export-mode)
    (unless (bound-and-true-p org-msg-export-in-progress)
      (replace-regexp-in-string
       "<h\\([0-9]\\) id=\"\\([a-z0-9-]+\\)\">\\(.*[^ ]\\)<\\/h[0-9]>" ; this is quite restrictive, but due to `org-reference-contraction' I can do this
       "<h\\1 id=\"\\2\">\\3<a aria-hidden=\"true\" href=\"#\\2\">#</a> </h\\1>"
       text))))

(add-to-list 'org-export-filter-headline-functions
             'org-export-html-headline-anchor)
(org-link-set-parameters "Https"
                         :follow (lambda (url arg) (browse-url (concat "https:" url) arg))
                         :export #'org-url-fancy-export)
(defun org-url-fancy-export (url _desc backend)
  (let ((metadata (org-url-unfurl-metadata (concat "https:" url))))
    (cond
     ((org-export-derived-backend-p backend 'html)
      (concat
       "<div class=\"link-preview\">"
       (format "<a href=\"%s\">" (concat "https:" url))
       (when (plist-get metadata :image)
         (format "<img src=\"%s\"/>" (plist-get metadata :image)))
       "<small>"
       (replace-regexp-in-string "//\\(?:www\\.\\)?\\([^/]+\\)/?.*" "\\1" url)
       "</small><p>"
       (when (plist-get metadata :title)
         (concat "<b>" (org-html-encode-plain-text (plist-get metadata :title)) "</b></br>"))
       (when (plist-get metadata :description)
         (org-html-encode-plain-text (plist-get metadata :description)))
       "</p></a></div>"))
     (t url))))
(setq org-url-unfurl-metadata--cache nil)
(defun org-url-unfurl-metadata (url)
  (cdr (or (assoc url org-url-unfurl-metadata--cache)
           (car (push
                 (cons
                  url
                  (let* ((head-data
                          (-filter #'listp
                                   (cdaddr
                                    (with-current-buffer (progn (message "Fetching metadata from %s" url)
                                                                (url-retrieve-synchronously url t t 5))
                                      (goto-char (point-min))
                                      (delete-region (point-min) (- (search-forward "<head") 6))
                                      (delete-region (search-forward "</head>") (point-max))
                                      (goto-char (point-min))
                                      (while (re-search-forward "<script[^\u2800]+?</script>" nil t)
                                        (replace-match ""))
                                      (goto-char (point-min))
                                      (while (re-search-forward "<style[^\u2800]+?</style>" nil t)
                                        (replace-match ""))
                                      (libxml-parse-html-region (point-min) (point-max))))))
                         (meta (delq nil
                                     (mapcar
                                      (lambda (tag)
                                        (when (eq 'meta (car tag))
                                          (cons (or (cdr (assoc 'name (cadr tag)))
                                                    (cdr (assoc 'property (cadr tag))))
                                                (cdr (assoc 'content (cadr tag))))))
                                      head-data))))
                    (let ((title (or (cdr (assoc "og:title" meta))
                                     (cdr (assoc "twitter:title" meta))
                                     (nth 2 (assq 'title head-data))))
                          (description (or (cdr (assoc "og:description" meta))
                                           (cdr (assoc "twitter:description" meta))
                                           (cdr (assoc "description" meta))))
                          (image (or (cdr (assoc "og:image" meta))
                                     (cdr (assoc "twitter:image" meta)))))
                      (when image
                        (setq image (replace-regexp-in-string
                                     "^/" (concat "https://" (replace-regexp-in-string "//\\([^/]+\\)/?.*" "\\1" url) "/")
                                     (replace-regexp-in-string
                                      "^//" "https://"
                                      image))))
                      (list :title title :description description :image image))))
                 org-url-unfurl-metadata--cache)))))
;; (setq-default org-html-with-latex `dvisvgm)
(setq org-html-mathjax-options
      '((path "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js" )
        (scale "1")
        (autonumber "ams")
        (multlinewidth "85%")
        (tagindent ".8em")
        (tagside "right")))

(setq org-html-mathjax-template
      "<script>
MathJax = {
  chtml: {
    scale: %SCALE
  },
  svg: {
    scale: %SCALE,
    fontCache: \"global\"
  },
  tex: {
    tags: \"%AUTONUMBER\",
    multlineWidth: \"%MULTLINEWIDTH\",
    tagSide: \"%TAGSIDE\",
    tagIndent: \"%TAGINDENT\"
  }
};
</script>
<script id=\"MathJax-script\" async
        src=\"%PATH\"></script>")

(provide 'config-ox-html))))

;;:------------------------
;;; ox-latex
;;:------------------------

;; This block defines `org-latex-convert-extra-special-strings',
;; `org-latex-extra-special-string-regexps',
;; `+org-latex-correct-latin-abbreviation-spaces',
;; `+org-latex-abbreviations',
;; `+org-latex-non-ascii-char-substitutions',
;; `+org-latex-replace-non-ascii-chars',
;; `+org-pdflatex-inputenc-encoded-chars',
;; `org-latex-julia-mono-fontspec', `org-latex-condensed-lists',
;; `org-latex-condense-lists', `org-latex-cover-page-p',
;; `org-latex-cover-page-maketitle',
;; `org-latex-subtitle-coverpage-format',
;; `org-latex-cover-page-wordcount-threshold', `org-latex-cover-page',
;; `org-latex-fontsets', `org-latex-fontset',
;; `org-latex-fontset-entry', `org-latex-default-fontset',
;; `org-latex-par-sep', `org-latex-italic-quotes',
;; `org-latex-use-microtype', `org-latex-embed-files',
;; `org-latex-embed-extra-files', `org-latex-box-preamble',
;; `org-latex-checkbox-preamble', `org-latex-caption-preamble',
;; `org-latex-embed-files-preamble', `org-latex-maths-preamble', and
;; `+org-export-latex-fancy-item-checkboxes'.

(confpkg-with-record '("hook: ox-latex" set-hooks)
  (with-eval-after-load 'ox-latex
(confpkg-with-record '("load: ox-latex" load-hooks)
;; org-latex-compilers = ("pdflatex" "xelatex" "lualatex"), which are the possible values for %latex
(setq org-latex-compiler "lualatex")
(setq org-latex-pdf-process '("LC_ALL=zh_CN.UTF-8 latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))
(defun +org-export-latex-fancy-item-checkboxes (text backend info)
  (when (org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string
     "\\\\item\\[{$\\\\\\(\\w+\\)$}\\]"
     (lambda (fullmatch)
       (concat "\\\\item[" (pcase (substring fullmatch 9 -3) ; content of capture group
                             ("square"   "\\\\checkboxUnchecked")
                             ("boxminus" "\\\\checkboxTransitive")
                             ("boxtimes" "\\\\checkboxChecked")
                             (_ (substring fullmatch 9 -3))) "]"))
     text)))

(add-to-list 'org-export-filter-item-functions
             '+org-export-latex-fancy-item-checkboxes)
(after! ox-latex
  (let* ((article-sections '(("\\section{%s}" . "\\section*{%s}")
                             ("\\subsection{%s}" . "\\subsection*{%s}")
                             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                             ("\\paragraph{%s}" . "\\paragraph*{%s}")
                             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
         (book-sections (append '(("\\chapter{%s}" . "\\chapter*{%s}"))
                                article-sections))
         (hanging-secnum-preamble "\\renewcommand\\sectionformat{\\llap{\\thesection\\autodot\\enskip}}
\\renewcommand\\subsectionformat{\\llap{\\thesubsection\\autodot\\enskip}}
\\renewcommand\\subsubsectionformat{\\llap{\\thesubsubsection\\autodot\\enskip}}")
         (big-chap-preamble ))
    (setcdr (assoc "article" org-latex-classes)
            `(,(concat "\\documentclass{scrartcl}" hanging-secnum-preamble)
              ,@article-sections))
    (add-to-list 'org-latex-classes
                 `("report" ,(concat "\\documentclass{scrartcl}" hanging-secnum-preamble)
                   ,@article-sections))
    (add-to-list 'org-latex-classes
                 `("book" ,(concat "\\documentclass[twoside=false]{scrbook}"
                                   big-chap-preamble hanging-secnum-preamble)
                   ,@book-sections))
    (add-to-list 'org-latex-classes
                 `("blank" "[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
                   ,@article-sections))
    (add-to-list 'org-latex-classes
                 `("bmc-article" "\\documentclass[article,code,maths]{bmc}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
                   ,@article-sections))
    (add-to-list 'org-latex-classes
                 `("bmc" "\\documentclass[code,maths]{bmc}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
                   ,@book-sections))))

(setq org-latex-tables-booktabs t
      org-latex-hyperref-template
      "\\providecolor{url}{HTML}{0077bb}
\\providecolor{link}{HTML}{882255}
\\providecolor{cite}{HTML}{999933}
\\hypersetup{
  pdfauthor={%a},
  pdftitle={%t},
  pdfkeywords={%k},
  pdfsubject={%d},
  pdfcreator={%c},
  pdflang={%L},
  breaklinks=true,
  colorlinks=true,
  linkcolor=link,
  urlcolor=url,
  citecolor=cite
}
\\urlstyle{same}
%% hide links styles in toc
\\NewCommandCopy{\\oldtoc}{\\tableofcontents}
\\renewcommand{\\tableofcontents}{\\begingroup\\hypersetup{hidelinks}\\oldtoc\\endgroup}"
      org-latex-reference-command "\\cref{%s}")
(defvar org-latex-maths-preamble
  "%% Maths-related packages
% More maths environments, commands, and symbols.
\\usepackage{amsmath, amssymb}
% Slanted fractions with \\sfrac{a}{b}, in text and maths.
\\usepackage{xfrac}
% Visually cancel expressions with \\cancel{value} and \\cancelto{expression}{value}
\\usepackage[makeroom]{cancel}
% Improvements on amsmath and utilities for mathematical typesetting
\\usepackage{mathtools}

% Deliminators
\\DeclarePairedDelimiter{\\abs}{\\lvert}{\\rvert}
\\DeclarePairedDelimiter{\\norm}{\\lVert}{\\rVert}

\\DeclarePairedDelimiter{\\ceil}{\\lceil}{\\rceil}
\\DeclarePairedDelimiter{\\floor}{\\lfloor}{\\rfloor}
\\DeclarePairedDelimiter{\\round}{\\lfloor}{\\rceil}

\\newcommand{\\RR}[1][]{\\ensuremath{\\ifstrempty{#1}{\\mathbb{R}}{\\mathbb{R}^{#1}}}} % Real numbers
\\newcommand{\\NN}[1][]{\\ensuremath{\\ifstrempty{#1}{\\mathbb{N}}{\\mathbb{N}^{#1}}}} % Natural numbers
\\newcommand{\\ZZ}[1][]{\\ensuremath{\\ifstrempty{#1}{\\mathbb{Z}}{\\mathbb{Z}^{#1}}}} % Integer numbers
\\newcommand{\\QQ}[1][]{\\ensuremath{\\ifstrempty{#1}{\\mathbb{Q}}{\\mathbb{Q}^{#1}}}} % Rational numbers
\\newcommand{\\CC}[1][]{\\ensuremath{\\ifstrempty{#1}{\\mathbb{C}}{\\mathbb{C}^{#1}}}} % Complex numbers

% Easy derivatives
\\ProvideDocumentCommand\\dv{o m g}{%
  \\IfNoValueTF{#3}{%
    \\dv[#1]{}{#2}}{%
    \\IfNoValueTF{#1}{%
      \\frac{\\dd #2}{\\dd #3}%
    }{\\frac{\\dd[#1] #2}{\\dd {#3}^{#1}}}}}
% Easy partial derivatives
\\ExplSyntaxOn
\\ProvideDocumentCommand\\pdv{o m g}{%
  \\IfNoValueTF{#3}{\\pdv[#1]{}{#2}}%
  {\\ifnum\\clist_count:n{#3}<2
    \\IfValueTF{#1}{\\frac{\\partial^{#1} #2}{\\partial {#3}^{#1}}}%
    {\\frac{\\partial #2}{\\partial #3}}
    \\else
    \\frac{\\IfValueTF{#1}{\\partial^{#1}}{\\partial^{\\clist_count:n{#3}}}#2}%
    {\\clist_map_inline:nn{#3}{\\partial ##1 \\,}\\!}
    \\fi}}
\\ExplSyntaxOff

% Laplacian
\\DeclareMathOperator{\\Lap}{\\mathcal{L}}

% Statistics
\\DeclareMathOperator{\\Var}{Var} % varience
\\DeclareMathOperator{\\Cov}{Cov} % covarience
\\newcommand{\\EE}{\\ensuremath{\\mathbb{E}}} % expected value
\\DeclareMathOperator{\\E}{E} % expected value

% I prefer the slanted \\leq/\\geq
\\let\\barleq\\leq % Save them in case they're every wanted
\\let\\bargeq\\geq
\\renewcommand{\\leq}{\\leqslant}
\\renewcommand{\\geq}{\\geqslant}

% Redefine the matrix environment to allow for alignment
% via an optional argument, and use r as the default.
\\makeatletter
\\renewcommand*\\env@matrix[1][r]{\\hskip -\\arraycolsep%
    \\let\\@ifnextchar\\new@ifnextchar
    \\array{*\\c@MaxMatrixCols #1}}
\\makeatother

% Slanted roman \"d\" for derivatives
\\ifcsname pdfoutput\\endcsname
  \\ifnum\\pdfoutput>0 % PDF
    \\newsavebox\\diffdbox{}
    \\newcommand{\\slantedromand}{{\\mathpalette\\makesl{d}}}
    \\newcommand{\\makesl}[2]{%
      \\begingroup
      \\sbox{\\diffdbox}{$\\mathsurround=0pt#1\\mathrm{#2}$}%
      \\pdfsave%
      \\pdfsetmatrix{1 0 0.2 1}%
      \\rlap{\\usebox{\\diffdbox}}%
      \\pdfrestore%
      \\hskip\\wd\\diffdbox%
      \\endgroup}
  \\else % DVI
    \\newcommand{\\slantedromand}{d} % fallback
  \\fi
\\else % Also DVI
  \\newcommand{\\slantedromand}{d} % fallback
\\fi

% Derivative d^n, nicely spaced
\\makeatletter
\\newcommand{\\dd}[1][]{\\mathop{}\\!%
  \\expandafter\\ifx\\expandafter&\\detokenize{#1}&% \\ifstrempty from etoolbox
    \\slantedromand\\@ifnextchar^{\\hspace{0.2ex}}{\\hspace{0.1ex}}
  \\else
    \\slantedromand\\hspace{0.2ex}^{#1}
  \\fi}
\\makeatother

\\NewCommandCopy{\\daccent}{\\d}
\\renewcommand{\\d}{\\ifmmode\\dd\\else\\daccent\\fi}"
  "Preamble that sets up a bunch of mathematical conveniences.")
(defvar org-latex-embed-files-preamble
  "\\usepackage[main,include]{embedall}
\\IfFileExists{./\\jobname.org}{\\embedfile[desc=The original file]{\\jobname.org}}{}"
  "Preamble that embeds files within the pdf.")

(defvar org-latex-caption-preamble
  "\\usepackage{subcaption}
\\usepackage[hypcap=true]{caption}
\\setkomafont{caption}{\\sffamily\\small}
\\setkomafont{captionlabel}{\\upshape\\bfseries}
\\captionsetup{justification=raggedright,singlelinecheck=true}
\\usepackage{capt-of} % required by Org"
  "Preamble that improves captions.")

(defvar org-latex-checkbox-preamble
  "\\newcommand{\\checkboxUnchecked}{$\\square$}
\\newcommand{\\checkboxTransitive}{\\rlap{\\raisebox{-0.1ex}{\\hspace{0.35ex}\\Large\\textbf -}}$\\square$}
\\newcommand{\\checkboxChecked}{\\rlap{\\raisebox{0.2ex}{\\hspace{0.35ex}\\scriptsize \\ding{52}}}$\\square$}"
  "Preamble that improves checkboxes.")

(defvar org-latex-box-preamble
  "\\ExplSyntaxOn
\\NewCoffin\\Content
\\NewCoffin\\SideRule
\\NewDocumentCommand{\\defsimplebox}{O{\\ding{117}} O{0.36em} m m m}{%
  % #1 ding, #2 ding offset, #3 name, #4 colour, #5 default label
  \\definecolor{#3}{HTML}{#4}
  \\NewDocumentEnvironment{#3}{ O{#5} }
  {
    \\vcoffin_set:Nnw \\Content { \\linewidth }
    \\noindent \\ignorespaces
    \\par\\vspace{-0.7\\baselineskip}%
    \\textcolor{#3}{#1}~\\textcolor{#3}{\\textbf{##1}}%
    \\vspace{-0.8\\baselineskip}
    \\begin{addmargin}[1em]{1em}
    }
    {
    \\end{addmargin}
    \\vspace{-0.5\\baselineskip}
    \\vcoffin_set_end:
    \\SetHorizontalCoffin\\SideRule{\\color{#3}\\rule{1pt}{\\CoffinTotalHeight\\Content}}
    \\JoinCoffins*\\Content[l,t]\\SideRule[l,t](#2,-0.7em)
    \\noindent\\TypesetCoffin\\Content
    \\vspace*{\\CoffinTotalHeight\\Content}\\bigskip
    \\vspace{-2\\baselineskip}
  }
}
\\ExplSyntaxOff"
  "Preamble that provides a macro for custom boxes.")
;; (defun org-latex-embed-extra-files ()
;;   "Return a string that uses embedfile to embed all tangled files."
;;   (mapconcat
;;    (lambda (file-desc)
;;      (format "\\IfFileExists{%1$s}{\\embedfile[desc=%2$s]{%1$s}}{}"
;;              (thread-last (car file-desc)
;;                           (replace-regexp-in-string "\\\\" "\\\\\\\\")
;;                           (replace-regexp-in-string "~" "\\\\string~"))
;;              (cdr file-desc)))
;;    (append
;;     (let (tangle-fspecs) ; All files being tangled to.
;;       (org-element-cache-map
;;        (lambda (src)
;;          (when (and (not (org-in-commented-heading-p nil src))
;;                     (not (org-in-archived-heading-p nil src)))
;;            (when-let ((lang (org-element-property :language src))
;;                       (params
;;                        (apply
;;                         #'org-babel-merge-params
;;                         (append
;;                          (org-with-point-at (org-element-property :begin src)
;;                            (org-babel-params-from-properties lang t))
;;                          (mapcar
;;                           (lambda (h)
;;                             (org-babel-parse-header-arguments h t))
;;                           (cons (org-element-property :parameters src)
;;                                 (org-element-property :header src))))))
;;                       (tangle-value
;;                        (pcase (alist-get :tangle params)
;;                          ((and (pred stringp) (pred (string-match-p "^(.*)$")) expr)
;;                           (eval (read expr)))
;;                          (val val)))
;;                       (tangle-file
;;                        (pcase tangle-value
;;                          ((or "no" (guard (member (alist-get :export-embed params) '("no" "nil"))))
;;                           nil)
;;                          ("yes"
;;                           (file-name-with-extension
;;                            (file-name-nondirectory (buffer-file-name))
;;                            (or (alist-get lang org-babel-tangle-lang-exts nil nil #'equal)
;;                                lang)))
;;                          (val val))))
;;              (unless (assoc tangle-file tangle-fspecs)
;;                (push
;;                 (cons tangle-file (format "Tangled %s file" lang))
;;                 tangle-fspecs)))))
;;        :granularity 'element
;;        :restrict-elements '(src-block))
;;       (nreverse tangle-fspecs))
;;     (let (extra-files)
;;       (save-excursion
;;         (goto-char (point-min))
;;         (while (re-search-forward "^[ \t]*#\\+embed:" nil t)
;;           (let* ((file-desc (split-string (org-element-property :value (org-element-at-point)) " :desc\\(?:ription\\)? ")))
;;             (push (cons (car file-desc) (or (cdr file-desc) "Extra file")) extra-files))))
;;       (nreverse extra-files)))
;;    "\n"))

(defun org-latex-embed-extra-files ()
  "Return a string that uses embedfile to embed all tangled files."
  ;; (mapconcat
  ;;  (lambda (file-desc)
  ;;    (format "\\IfFileExists{%1$s}{\\embedfile[desc=%2$s]{%1$s}}{}"
  ;;            (thread-last (car file-desc)
  ;;              (replace-regexp-in-string "\\\\" "\\\\\\\\")
  ;;              (replace-regexp-in-string "~" "\\\\string~"))
  ;;            (cdr file-desc)))
  ;;  (append
  ;;   (mapcar (lambda (f-block)
  ;;             (let ((file-lang (cons (or (car f-block) (caddr (cadr f-block))) (caadr f-block))))
  ;;               (cons (car file-lang) (format "Tangled %s file" (cdr file-lang)))))
  ;;           (org-babel-tangle-collect-blocks)) ; all files being tangled to
  ;;   (let (extra-files)
  ;;     (save-excursion
  ;;       (goto-char (point-min))
  ;;       (while (re-search-forward "^[ \t]*#\\+embed:" nil t)
  ;;         (let* ((file-desc (split-string (org-element-property :value (org-element-at-point)) " :desc\\(?:ription\\)? ")))
  ;;           (push (cons (car file-desc) (or (cdr file-desc) "Extra file")) extra-files))))
  ;;     (nreverse extra-files)))
   ;; "\n")
)
(defvar org-latex-embed-files nil
  "Embed the source .org, .tex, and any tangled files.")
(defvar org-latex-use-microtype t
  "Use the microtype pakage.")
(defvar org-latex-italic-quotes t
  "Make \"quote\" environments italic.")
(defvar org-latex-par-sep t
  "Vertically seperate paragraphs, and remove indentation.")

(org-export-update-features 'latex
  ((image caption)
   :condition "\\[\\[xkcd:"))
(org-export-update-features 'latex
  (maths
   :snippet org-latex-maths-preamble
   :order 0.2)
  (cleveref
   :condition "cref:\\|\\cref{\\|\\[\\[[^\\]+\n?[^\\]\\]\\]"
   :snippet "\\usepackage[capitalize]{cleveref}"
   :order 1)
  (caption
   :snippet org-latex-caption-preamble
   :order 2.1)
  (microtype
   :condition org-latex-use-microtype
   :snippet "\\usepackage[activate={true,nocompatibility},final,tracking=true,kerning=true,spacing=true,factor=2000]{microtype}\n"
   ;; :snippet "\\usepackage[activate={true,nocompatibility},final,tracking=true,factor=2000]{microtype}\n"
   :order 0.1)
  (embed-files
   :condition org-latex-embed-files
   :snippet org-latex-embed-files-preamble
   :order -2)
  ;; (embed-tangled
  ;;  :condition (and org-latex-embed-files
  ;;                  "^[ \t]*#\\+embed\\|^[ \t]*#\\+begin_src\\|^[ \t]*#\\+BEGIN_SRC")
  ;;  :requires embed-files
  ;;  :snippet org-latex-embed-extra-files
  ;;  :order -1)
  (acronym
   :condition "[;\\\\]?\\b[A-Z][A-Z]+s?[^A-Za-z]"
   :snippet "\\newcommand{\\acr}[1]{\\protect\\textls*[110]{\\scshape #1}}\n\\newcommand{\\acrs}{\\protect\\scalebox{.91}[.84]{\\hspace{0.15ex}s}}"
   :order 0.4)
  (box-drawing
   :condition "[\u2500-\u259F]"
   :snippet "\\usepackage{pmboxdraw}"
   :order 0.05)
  (italic-quotes
   :condition (and org-latex-italic-quotes "^[ \t]*#\\+begin_quote\\|\\\\begin{quote}")
   :snippet "\\renewcommand{\\quote}{\\list{}{\\rightmargin\\leftmargin}\\item\\relax\\em}\n"
   :order 0.5)
  (par-sep
   :condition org-latex-par-sep
   :snippet "\\setlength{\\parskip}{\\baselineskip}\n\\setlength{\\parindent}{0pt}\n"
   :order 0.5)
  (.pifont
   :snippet "\\usepackage{pifont}")
  (.xcoffins
   :snippet "\\usepackage{xcoffins}")
  (checkbox
   :condition "^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\|[A-Za-z]+[.)]\\) \\[[ -X]\\]"
   :requires .pifont
   :snippet (concat (unless (memq 'maths features)
                      "\\usepackage{amssymb} % provides \\square")
                    org-latex-checkbox-preamble)
   :order 3)
  (.fancy-box
   :requires (.pifont .xcoffins)
   :snippet org-latex-box-preamble
   :order 3.9)
  (box-warning
   :condition "^[ \t]*#\\+begin_warning\\|\\\\begin{warning}"
   :requires .fancy-box
   :snippet "\\defsimplebox{warning}{e66100}{Warning}"
   :order 4)
  (box-info
   :condition "^[ \t]*#\\+begin_info\\|\\\\begin{info}"
   :requires .fancy-box
   :snippet "\\defsimplebox{info}{3584e4}{Information}"
   :order 4)
  (box-notes
   :condition "^[ \t]*#\\+begin_notes\\|\\\\begin{notes}"
   :requires .fancy-box
   :snippet "\\defsimplebox{notes}{26a269}{Notes}"
   :order 4)
  (box-success
   :condition "^[ \t]*#\\+begin_success\\|\\\\begin{success}"
   :requires .fancy-box
   :snippet "\\defsimplebox{success}{26a269}{\\vspace{-\\baselineskip}}"
   :order 4)
  (box-error
   :condition "^[ \t]*#\\+begin_error\\|\\\\begin{error}"
   :requires .fancy-box
   :snippet "\\defsimplebox{error}{c01c28}{Important}"
   :order 4))
(setq org-latex-packages-alist
      '(("" "xcolor" t)))
(defvar org-latex-default-fontset 'alegreya
  "Fontset from `org-latex-fontsets' to use by default.
As cm (computer modern) is TeX's default, that causes nothing
to be added to the document.

If \"nil\" no custom fonts will ever be used.")

(eval '(cl-pushnew '(:latex-font-set nil "fontset" org-latex-default-fontset)
                   (org-export-backend-options (org-export-get-backend 'latex))))
(defun org-latex-fontset-entry ()
  "Get the fontset spec of the current file.
Has format \"name\" or \"name-style\" where 'name' is one of
the cars in `org-latex-fontsets'."
  (let ((fontset-spec
         (symbol-name
          (or (car (delq nil
                         (mapcar
                          (lambda (opt-line)
                            (plist-get (org-export--parse-option-keyword opt-line 'latex)
                                       :latex-font-set))
                          (cdar (org-collect-keywords '("OPTIONS"))))))
              org-latex-default-fontset))))
    (cons (intern (car (split-string fontset-spec "-")))
          (when (cadr (split-string fontset-spec "-"))
            (intern (concat ":" (cadr (split-string fontset-spec "-"))))))))

(defun org-latex-fontset (&rest desired-styles)
  "Generate a LaTeX preamble snippet which applies the current fontset for DESIRED-STYLES."
  (let* ((fontset-spec (org-latex-fontset-entry))
         (fontset (alist-get (car fontset-spec) org-latex-fontsets)))
    (if fontset
        (concat
         (mapconcat
          (lambda (style)
            (when (plist-get fontset style)
              (concat (plist-get fontset style) "\n")))
          desired-styles
          "")
         (when (memq (cdr fontset-spec) desired-styles)
           (pcase (cdr fontset-spec)
             (:serif "\\renewcommand{\\familydefault}{\\rmdefault}\n")
             (:sans "\\renewcommand{\\familydefault}{\\sfdefault}\n")
             (:mono "\\renewcommand{\\familydefault}{\\ttdefault}\n"))))
      (error "Font-set %s is not provided in org-latex-fontsets" (car fontset-spec)))))
(org-export-update-features 'latex
  (custom-font
   :condition org-latex-default-fontset
   :snippet (org-latex-fontset :serif :sans :mono)
   :order 0)
  (custom-maths-font
   :condition t
   :when (custom-font maths)
   :snippet (org-latex-fontset :maths)
   :order 0.3))
(defvar org-latex-fontsets
  '((cm nil) ; computer modern
    (## nil) ; no font set
    (alegreya
     :serif "\\usepackage[osf]{Alegreya}"
     :sans "\\usepackage{AlegreyaSans}"
     :mono "\\usepackage[scale=0.88]{sourcecodepro}"
     :maths "\\usepackage[varbb]{newpxmath}")
    (biolinum
     :serif "\\usepackage[osf]{libertineRoman}"
     :sans "\\usepackage[sfdefault,osf]{biolinum}"
     :mono "\\usepackage[scale=0.88]{sourcecodepro}"
     :maths "\\usepackage[libertine,varvw]{newtxmath}")
    (fira
     :sans "\\usepackage[sfdefault,scale=0.85]{FiraSans}"
     :mono "\\usepackage[scale=0.80]{FiraMono}"
     :maths "\\usepackage{newtxsf} % change to firamath in future?")
    (kp
     :serif "\\usepackage{kpfonts}")
    (newpx
     :serif "\\usepackage{newpxtext}"
     :sans "\\usepackage{gillius}"
     :mono "\\usepackage[scale=0.9]{sourcecodepro}"
     :maths "\\usepackage[varbb]{newpxmath}")
    (noto
     :serif "\\usepackage[osf]{noto-serif}"
     :sans "\\usepackage[osf]{noto-sans}"
     :mono "\\usepackage[scale=0.96]{noto-mono}"
     :maths "\\usepackage{notomath}")
    (plex
     :serif "\\usepackage{plex-serif}"
     :sans "\\usepackage{plex-sans}"
     :mono "\\usepackage[scale=0.95]{plex-mono}"
     :maths "\\usepackage{newtxmath}") ; may be plex-based in future
    (source
     :serif "\\usepackage[osf,semibold]{sourceserifpro}"
     :sans "\\usepackage[osf,semibold]{sourcesanspro}"
     :mono "\\usepackage[scale=0.92]{sourcecodepro}"
     :maths "\\usepackage{newtxmath}") ; may be sourceserifpro-based in future
    (times
     :serif "\\usepackage{newtxtext}"
     :maths "\\usepackage{newtxmath}"))
  "Alist of fontset specifications.
Each car is the name of the fontset (which cannot include \"-\").

Each cdr is a plist with (optional) keys :serif, :sans, :mono, and :maths.
A key's value is a LaTeX snippet which loads such a font.")
(org-export-update-features 'latex
  (alegreya-typeface
   :condition (string= (car (org-latex-fontset-entry)) "alegreya")
   :snippet nil)
  (alegreya-tabular-figures
   :condition t
   :when (alegreya-typeface table)
   :snippet "\
\\makeatletter
% tabular lining figures in tables
\\renewcommand{\\tabular}{\\AlegreyaTLF\\let\\@halignto\\@empty\\@tabular}
\\makeatother"
   :order 0.5))
(org-export-update-features 'latex
  (alegreya-latex-symbol
    :condition "LaTeX"
    :when alegreya-typeface
    :snippet "\
\\makeatletter
% Kerning around the A needs adjusting
\\DeclareRobustCommand{\\LaTeX}{L\\kern-.24em%
        {\\sbox\\z@ T%
         \\vbox to\\ht\\z@{\\hbox{\\check@mathfonts
                              \\fontsize\\sf@size\\z@
                              \\math@fontsfalse\\selectfont
                              A}%
                        \\vss}%
        }%
        \\kern-.10em%
        \\TeX}
\\makeatother"
    :order 0.5))
(defvar org-latex-cover-page 'auto
  "When t, use a cover page by default.
When auto, use a cover page when the document's wordcount exceeds
`org-latex-cover-page-wordcount-threshold'.

Set with #+option: coverpage:{yes,auto,no} in org buffers.")
(defvar org-latex-cover-page-wordcount-threshold 5000
  "Document word count at which a cover page will be used automatically.
This condition is applied when cover page option is set to auto.")
(defvar org-latex-subtitle-coverpage-format "\\\\\\bigskip\n\\LARGE\\mdseries\\itshape\\color{black!80} %s\\par"
  "Variant of `org-latex-subtitle-format' to use with the cover page.")
(defvar org-latex-cover-page-maketitle
  "\\usepackage{tikz}
\\usetikzlibrary{shapes.geometric}
\\usetikzlibrary{calc}

\\newsavebox\\orgicon
\\begin{lrbox}{\\orgicon}
  \\begin{tikzpicture}[y=0.80pt, x=0.80pt, inner sep=0pt, outer sep=0pt]
    \\path[fill=black!6] (16.15,24.00) .. controls (15.58,24.00) and (13.99,20.69) .. (12.77,18.06)arc(215.55:180.20:2.19) .. controls (12.33,19.91) and (11.27,19.09) .. (11.43,18.05) .. controls (11.36,18.09) and (10.17,17.83) .. (10.17,17.82) .. controls (9.94,18.75) and (9.37,19.44) .. (9.02,18.39) .. controls (8.32,16.72) and (8.14,15.40) .. (9.13,13.80) .. controls (8.22,9.74) and (2.18,7.75) .. (2.81,4.47) .. controls (2.99,4.47) and (4.45,0.99) .. (9.15,2.41) .. controls (14.71,3.99) and (17.77,0.30) .. (18.13,0.04) .. controls (18.65,-0.49) and (16.78,4.61) .. (12.83,6.90) .. controls (10.49,8.18) and (11.96,10.38) .. (12.12,11.15) .. controls (12.12,11.15) and (14.00,9.84) .. (15.36,11.85) .. controls (16.58,11.53) and (17.40,12.07) .. (18.46,11.69) .. controls (19.10,11.41) and (21.79,11.58) .. (20.79,13.08) .. controls (20.79,13.08) and (21.71,13.90) .. (21.80,13.99) .. controls (21.97,14.75) and (21.59,14.91) .. (21.47,15.12) .. controls (21.44,15.60) and (21.04,15.79) .. (20.55,15.44) .. controls (19.45,15.64) and (18.36,15.55) .. (17.83,15.59) .. controls (16.65,15.76) and (15.67,16.38) .. (15.67,16.38) .. controls (15.40,17.19) and (14.82,17.01) .. (14.09,17.32) .. controls (14.70,18.69) and (14.76,19.32) .. (15.50,21.32) .. controls (15.76,22.37) and (16.54,24.00) .. (16.15,24.00) -- cycle(7.83,16.74) .. controls (6.83,15.71) and (5.72,15.70) .. (4.05,15.42) .. controls (2.75,15.19) and (0.39,12.97) .. (0.02,10.68) .. controls (-0.02,10.07) and (-0.06,8.50) .. (0.45,7.18) .. controls (0.94,6.05) and (1.27,5.45) .. (2.29,4.85) .. controls (1.41,8.02) and (7.59,10.18) .. (8.55,13.80) -- (8.55,13.80) .. controls (7.73,15.00) and (7.80,15.64) .. (7.83,16.74) -- cycle;
  \\end{tikzpicture}
\\end{lrbox}

\\makeatletter
\\g@addto@macro\\tableofcontents{\\clearpage}
\\renewcommand\\maketitle{
  \\thispagestyle{empty}
  \\hyphenpenalty=10000 % hyphens look bad in titles
  \\renewcommand{\\baselinestretch}{1.1}
  \\NewCommandCopy{\\oldtoday}{\\today}
  \\renewcommand{\\today}{\\LARGE\\number\\year\\\\\\large%
    \\ifcase \\month \\or Jan\\or Feb\\or Mar\\or Apr\\or May \\or Jun\\or Jul\\or Aug\\or Sep\\or Oct\\or Nov\\or Dec\\fi
    ~\\number\\day}
  \\begin{tikzpicture}[remember picture,overlay]
    %% Background Polygons %%
    \\foreach \\i in {2.5,...,22} % bottom left
    {\\node[rounded corners,black!3.5,draw,regular polygon,regular polygon sides=6, minimum size=\\i cm,ultra thick] at ($(current page.west)+(2.5,-4.2)$) {} ;}
    \\foreach \\i in {0.5,...,22} % top left
    {\\node[rounded corners,black!5,draw,regular polygon,regular polygon sides=6, minimum size=\\i cm,ultra thick] at ($(current page.north west)+(2.5,2)$) {} ;}
    \\node[rounded corners,fill=black!4,regular polygon,regular polygon sides=6, minimum size=5.5 cm,ultra thick] at ($(current page.north west)+(2.5,2)$) {};
    \\foreach \\i in {0.5,...,24} % top right
    {\\node[rounded corners,black!2,draw,regular polygon,regular polygon sides=6, minimum size=\\i cm,ultra thick] at ($(current page.north east)+(0,-8.5)$) {} ;}
    \\node[fill=black!3,rounded corners,regular polygon,regular polygon sides=6, minimum size=2.5 cm,ultra thick] at ($(current page.north east)+(0,-8.5)$) {};
    \\foreach \\i in {21,...,3} % bottom right
    {\\node[black!3,rounded corners,draw,regular polygon,regular polygon sides=6, minimum size=\\i cm,ultra thick] at ($(current page.south east)+(-1.5,0.75)$) {} ;}
    \\node[fill=black!3,rounded corners,regular polygon,regular polygon sides=6, minimum size=2 cm,ultra thick] at ($(current page.south east)+(-1.5,0.75)$) {};
    \\node[align=center, scale=1.4] at ($(current page.south east)+(-1.5,0.75)$) {\\usebox\\orgicon};
    %% Text %%
    \\node[left, align=right, black, text width=0.8\\paperwidth, minimum height=3cm, rounded corners,font=\\Huge\\bfseries] at ($(current page.north east)+(-2,-8.5)$)
    {\\@title};
    \\node[left, align=right, black, text width=0.8\\paperwidth, minimum height=2cm, rounded corners, font=\\Large] at ($(current page.north east)+(-2,-11.8)$)
    {\\scshape \\@author};
    \\renewcommand{\\baselinestretch}{0.75}
    \\node[align=center,rounded corners,fill=black!3,text=black,regular polygon,regular polygon sides=6, minimum size=2.5 cm,inner sep=0, font=\\Large\\bfseries ] at ($(current page.west)+(2.5,-4.2)$)
    {\\@date};
  \\end{tikzpicture}
  \\let\\today\\oldtoday
  \\clearpage}
\\makeatother"
  "LaTeX preamble snippet that sets \\maketitle to produce a cover page.")

(eval '(cl-pushnew '(:latex-cover-page nil "coverpage" org-latex-cover-page)
                   (org-export-backend-options (org-export-get-backend 'latex))))

(defun org-latex-cover-page-p ()
  "Whether a cover page should be used when exporting this Org file."
  (pcase (or (car
              (delq nil
                    (mapcar
                     (lambda (opt-line)
                       (plist-get (org-export--parse-option-keyword opt-line 'latex) :latex-cover-page))
                     (cdar (org-collect-keywords '("OPTIONS"))))))
             org-latex-cover-page)
    ((or 't 'yes) t)
    ('auto (when (> (count-words (point-min) (point-max)) org-latex-cover-page-wordcount-threshold) t))
    (_ nil)))

(defadvice! org-latex-set-coverpage-subtitle-format-a (contents info)
  "Set the subtitle format when a cover page is being used."
  :before #'org-latex-template
  (when (org-latex-cover-page-p)
    (setf info (plist-put info :latex-subtitle-format org-latex-subtitle-coverpage-format))))

(org-export-update-features 'latex
  (cover-page
   :condition (org-latex-cover-page-p)
   :snippet org-latex-cover-page-maketitle
   :order 9))
(defvar org-latex-condense-lists t
  "Reduce the space between list items.")
(defvar org-latex-condensed-lists
  "\\newcommand{\\setuplistspacing}{\\setlength{\\itemsep}{-0.5ex}\\setlength{\\parskip}{1.5ex}\\setlength{\\parsep}{0pt}}
\\let\\olditem\\itemize\\renewcommand{\\itemize}{\\olditem\\setuplistspacing}
\\let\\oldenum\\enumerate\\renewcommand{\\enumerate}{\\oldenum\\setuplistspacing}
\\let\\olddesc\\description\\renewcommand{\\description}{\\olddesc\\setuplistspacing}"
  "LaTeX preamble snippet that reduces the space between list items.")

(org-export-update-features 'latex
  (condensed-lists
   :condition (and org-latex-condense-lists "^[ \t]*[-+]\\|^[ \t]*[1Aa][.)] ")
   :snippet org-latex-condensed-lists
   :order 0.7))
(use-package! engrave-faces-latex
  :after ox-latex)
(setq org-latex-listings 'engraved)
(org-export-update-features 'latex
  (no-protrusion-in-code
   :condition t
   :when microtype
   :snippet "\\ifcsname Code\\endcsname\n  \\let\\oldcode\\Code\\renewcommand{\\Code}{\\microtypesetup{protrusion=false}\\oldcode}\n\\fi"
   :order 98.5))
(defadvice! org-latex-example-block-engraved (orig-fn example-block contents info)
  "Like `org-latex-example-block', but supporting an engraved backend"
  :around #'org-latex-example-block
  (let ((output-block (funcall orig-fn example-block contents info)))
    (if (eq 'engraved (plist-get info :latex-listings))
        (format "\\begin{Code}[alt]\n%s\n\\end{Code}" output-block)
      output-block)))
(defadvice! org-latex-pick-compiler (_contents info)
  :before #'org-latex-template
  :before #'org-beamer-template
  (when (and (memq 'code (plist-get info :features))
             (memq 'julia-code (plist-get info :features))
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "[^\x00-\x7F\u200b]" nil t)))
    (setf info (plist-put
                (if (member #'+org-latex-replace-non-ascii-chars (plist-get info :filter-final-output))
                    (plist-put info :filter-final-output
                               (delq #'+org-latex-replace-non-ascii-chars (plist-get info :filter-final-output)))
                  info)
                :latex-compiler "lualatex"))))
(defvar org-latex-julia-mono-fontspec
  "\\ifcsname directlua\\endcsname
  \\usepackage{fontspec}
  \\newfontfamily\\JuliaMono{JuliaMono-Regular.ttf}[Path=/usr/share/fonts/truetype/, Extension=.ttf]
  \\newfontface\\JuliaMonoRegular{JuliaMono-Regular}
  \\setmonofont{JuliaMonoRegular}[Contextuals=Alternate, Scale=MatchLowercase]
\\fi"
  "LaTeX preamble snippet that sets LuaLaTeX's fontspec to use Julia Mono.")

(org-export-update-features 'latex
  (julia-code
   :condition "^[ \t]*#\\+begin_src julia\\|^[ \t]*#\\+BEGIN_SRC julia\\|src_julia"
   :when code
   :snippet org-latex-julia-mono-fontspec
   :order 0)
  (microtype-lualatex
   :condition t
   :when (microtype julia-code)
   :prevents microtype
   :snippet "\\usepackage[activate={true,nocompatibility},final,tracking=true,factor=2000]{microtype}\n"
   :order 0.1)
  (custom-font-no-mono
   :condition t
   :prevents custom-font
   :snippet (org-latex-fontset :serif :sans)
   :order 0))
(defvar +org-pdflatex-inputenc-encoded-chars
  "[[:ascii:]\u00A0-\u01F0\u0218-\u021BȲȳȷˆˇ˜˘˙˛˝\u0400-\u04FFḂḃẞ\u200B\u200C\u2010-\u201E†‡•…‰‱‹›※‽⁄⁎⁒₡₤₦₩₫€₱℃№℗℞℠™Ω℧℮←↑→↓〈〉␢␣◦◯♪⟨⟩Ḡḡ\uFB00-\uFB06\u2500-\u259F]")

(defun +org-latex-replace-non-ascii-chars (text backend info)
  "Replace non-ascii chars with \\char\"XYZ forms."
  (when (and (org-export-derived-backend-p backend 'latex)
             (string= (plist-get info :latex-compiler) "pdflatex"))
    (let (case-replace)
      (replace-regexp-in-string "[^[:ascii:]]"
                                (lambda (nonascii)
                                  (if (or (string-match-p +org-pdflatex-inputenc-encoded-chars nonascii)
                                          (string-match-p org-latex-emoji--rx nonascii))
                                      nonascii
                                    (or (cdr (assoc nonascii +org-latex-non-ascii-char-substitutions))
                                        "¿")))
                                text))))

(add-to-list 'org-export-filter-plain-text-functions #'+org-latex-replace-non-ascii-chars t)
(defvar +org-latex-non-ascii-char-substitutions
   '(("ɑ" . "\\\\(\\\\alpha\\\\)")
     ("β" . "\\\\(\\\\beta\\\\)")
     ("γ" . "\\\\(\\\\gamma\\\\)")
     ("δ" . "\\\\(\\\\delta\\\\)")
     ("ε" . "\\\\(\\\\epsilon\\\\)")
     ("ϵ" . "\\\\(\\\\varepsilon\\\\)")
     ("ζ" . "\\\\(\\\\zeta\\\\)")
     ("η" . "\\\\(\\\\eta\\\\)")
     ("θ" . "\\\\(\\\\theta\\\\)")
     ("ϑ" . "\\\\(\\\\vartheta\\\\)")
     ("ι" . "\\\\(\\\\iota\\\\)")
     ("κ" . "\\\\(\\\\kappa\\\\)")
     ("λ" . "\\\\(\\\\lambda\\\\)")
     ("μ" . "\\\\(\\\\mu\\\\)")
     ("ν" . "\\\\(\\\\nu\\\\)")
     ("ξ" . "\\\\(\\\\xi\\\\)")
     ("π" . "\\\\(\\\\pi\\\\)")
     ("ϖ" . "\\\\(\\\\varpi\\\\)")
     ("ρ" . "\\\\(\\\\rho\\\\)")
     ("ϱ" . "\\\\(\\\\varrho\\\\)")
     ("σ" . "\\\\(\\\\sigma\\\\)")
     ("ς" . "\\\\(\\\\varsigma\\\\)")
     ("τ" . "\\\\(\\\\tau\\\\)")
     ("υ" . "\\\\(\\\\upsilon\\\\)")
     ("ϕ" . "\\\\(\\\\phi\\\\)")
     ("φ" . "\\\\(\\\\varphi\\\\)")
     ("ψ" . "\\\\(\\\\psi\\\\)")
     ("ω" . "\\\\(\\\\omega\\\\)")
     ("Γ" . "\\\\(\\\\Gamma\\\\)")
     ("Δ" . "\\\\(\\\\Delta\\\\)")
     ("Θ" . "\\\\(\\\\Theta\\\\)")
     ("Λ" . "\\\\(\\\\Lambda\\\\)")
     ("Ξ" . "\\\\(\\\\Xi\\\\)")
     ("Π" . "\\\\(\\\\Pi\\\\)")
     ("Σ" . "\\\\(\\\\Sigma\\\\)")
     ("Υ" . "\\\\(\\\\Upsilon\\\\)")
     ("Φ" . "\\\\(\\\\Phi\\\\)")
     ("Ψ" . "\\\\(\\\\Psi\\\\)")
     ("Ω" . "\\\\(\\\\Omega\\\\)")
     ("א" . "\\\\(\\\\aleph\\\\)")
     ("ב" . "\\\\(\\\\beth\\\\)")
     ("ד" . "\\\\(\\\\daleth\\\\)")
     ("ג" . "\\\\(\\\\gimel\\\\)")))
(defvar +org-latex-abbreviations
  '(;; Latin
    "cf." "e.g." "etc." "et al." "i.e." "v." "vs." "viz." "n.b."
    ;; Corperate
    "inc." "govt." "ltd." "pty." "dept."
    ;; Temporal
    "est." "c."
    ;; Honorifics
    "Prof." "Dr." "Mr." "Mrs." "Ms." "Miss." "Sr." "Jr."
    ;; Components of a work
    "ed." "vol." "sec." "chap." "pt." "pp." "op." "no."
    ;; Common usage
    "approx." "misc." "min." "max.")
  "A list of abbreviations that should be spaced correctly when exporting to LaTeX.")

(defun +org-latex-correct-latin-abbreviation-spaces (text backend _info)
  "Normalise spaces after Latin abbreviations."
  (when (org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string (rx (group (or line-start space)
                                         (regexp (regexp-opt-group +org-latex-abbreviations)))
                                  (or line-end space))
                              "\\1\\\\ "
                              text)))

(add-to-list 'org-export-filter-paragraph-functions #'+org-latex-correct-latin-abbreviation-spaces t)
(defvar org-latex-extra-special-string-regexps
  '(("<->" . "\\\\(\\\\leftrightarrow{}\\\\)")
    ("->" . "\\\\textrightarrow{}")
    ("<-" . "\\\\textleftarrow{}")))

(defun org-latex-convert-extra-special-strings (string)
  "Convert special characters in STRING to LaTeX."
  (dolist (a org-latex-extra-special-string-regexps string)
    (let ((re (car a))
          (rpl (cdr a)))
      (setq string (replace-regexp-in-string re rpl string t)))))

(defadvice! org-latex-plain-text-extra-special-a (orig-fn text info)
  "Make `org-latex-plain-text' handle some extra special strings."
  :around #'org-latex-plain-text
  (let ((output (funcall orig-fn text info)))
    (when (plist-get info :with-special-strings)
      (setq output (org-latex-convert-extra-special-strings output)))
    output))
(setq org-latex-text-markup-alist
      '((bold . "\\textbf{%s}")
        (code . protectedtexttt)
        (italic . "\\emph{%s}")
        (strike-through . "\\sout{%s}")
        (underline . "\\uline{%s}")
        (verbatim . verb)))
;; No missing LaTeX packags detected

(provide 'config-ox-latex))))

;;:------------------------
;;; ox-latex-emoji
;;:------------------------

;; This block defines `org-latex-emoji-install--convert',
;; `org-latex-emoji-install--install',
;; `org-latex-emoji-install--download', `org-latex-emoji-install',
;; `org-latex-emoji-setup', `org-latex-emoji-fill-preamble',
;; `org-latex-emoji-declaration', `org-latex-emoji-utf16',
;; `org-latex-emoji-preamble', `org-latex-emoji-sets', and
;; `org-latex-emoji--rx'.

(confpkg-with-record '("hook: ox-latex-emoji" set-hooks)
  (with-eval-after-load 'ox-latex
(confpkg-with-record '("load: ox-latex-emoji" load-hooks)
(defvar org-latex-emoji--rx
  (let (emojis)
    (map-char-table
     (lambda (char set)
       (when (eq set 'emoji)
         (push (copy-tree char) emojis)))
     char-script-table)
    (rx-to-string `(any ,@emojis)))
  "A regexp to find all emoji-script characters.")
(defconst org-latex-emoji-base-dir
  (expand-file-name "emojis/" doom-cache-dir)
  "Directory where emojis should be saved and look for.")

(defvar org-latex-emoji-sets
  '(("twemoji" :url "https://github.com/twitter/twemoji/archive/refs/tags/v14.0.2.zip"
     :folder "twemoji-14.0.2/assets/svg" :height "1.8ex" :offset "-0.3ex")
    ("twemoji-bw" :url "https://github.com/youdly/twemoji-color-font/archive/refs/heads/v11-release.zip"
     :folder "twemoji-color-font-11-release/assets/builds/svg-bw" :height "1.8ex" :offset "-0.3ex")
    ("openmoji" :url "https://github.com/hfg-gmuend/openmoji/releases/latest/download/openmoji-svg-color.zip"
     :height "2.2ex" :offset "-0.45ex")
    ("openmoji-bw" :url "https://github.com/hfg-gmuend/openmoji/releases/latest/download/openmoji-svg-black.zip"
     :height "2.2ex" :offset "-0.45ex")
    ("emojione" :url "https://github.com/joypixels/emojione/archive/refs/tags/v2.2.7.zip"
     :folder "emojione-2.2.7/assets/svg") ; Warning, poor coverage
    ("noto" :url "https://github.com/googlefonts/noto-emoji/archive/refs/tags/v2.038.zip"
     :folder "noto-emoji-2.038/svg" :file-regexp "^emoji_u\\([0-9a-f_]+\\)"
     :height "2.0ex" :offset "-0.3ex"))
  "An alist of plistst of emoji sets.
Specified with the minimal form:
  (\"SET-NAME\" :url \"URL\")
The following optional parameters are supported:
  :folder (defaults to \"\")
  The folder within the archive where the emojis exist.
  :file-regexp (defaults to nil)
  Pattern with the emoji code point as the first capture group.
  :height (defaults to \"1.8ex\")
  Height of the emojis to be used.
  :offset (defaults to \"-0.3ex\")
  Baseline offset of the emojis.")

(defconst org-latex-emoji-keyword
  "LATEX_EMOJI_SET"
  "Keyword used to set the emoji set from `org-latex-emoji-sets'.")

(defvar org-latex-emoji-preamble "\\usepackage{accsupp}
\\usepackage{transparent}
\\newsavebox\\emojibox

\\NewDocumentCommand\\DeclareEmoji{m m}{% UTF-8 codepoint, UTF-16 codepoint
  \\DeclareUnicodeCharacter{#1}{%
    \\sbox\\emojibox{\\raisebox{OFFSET}{%
        \\includegraphics[height=HEIGHT]{EMOJI-FOLDER/#1}}}%
    \\usebox\\emojibox
    \\llap{%
      \\resizebox{\\wd\\emojibox}{\\height}{%
        \\BeginAccSupp{method=hex,unicode,ActualText=#2}%
        \\texttransparent{0}{X}%
        \\EndAccSupp{}}}}}"
  "LaTeX preamble snippet that will allow for emojis to be declared.
Contains the string \"EMOJI-FOLDER\" which should be replaced with
the path to the emoji folder.")

(defun org-latex-emoji-utf16 (char)
  "Return the pair of UTF-16 surrogates that represent CHAR."
  (list
   (+ #xD7C0 (ash char -10))
   (+ #xDC00 (logand char #x03FF))))

(defun org-latex-emoji-declaration (char)
  "Construct the LaTeX command declaring CHAR as an emoji."
  (format "\\DeclareEmoji{%X}{%s} %% %s"
          char
          (if (< char #xFFFF)
              (format "%X" char)
            (apply #'format "%X%X" (org-latex-emoji-utf16 char)))
          (capitalize (get-char-code-property char 'name))))

(defun org-latex-emoji-fill-preamble (emoji-folder &optional height offset svg-p)
  "Fill in `org-latex-emoji-preamble' with EMOJI-FOLDER, HEIGHT, and OFFSET.
If SVG-P is set \"includegraphics\" will be replaced with \"includesvg\"."
  (let* (case-fold-search
         (filled-preamble
          (replace-regexp-in-string
           "HEIGHT"
           (or height "1.8ex")
           (replace-regexp-in-string
            "OFFSET"
            (or offset "-0.3ex")
            (replace-regexp-in-string
             "EMOJI-FOLDER"
             (directory-file-name
              (if (getenv "HOME")
                  (replace-regexp-in-string
                   (regexp-quote (getenv "HOME"))
                   "\\string~"
                   emoji-folder t t)
                emoji-folder))
             org-latex-emoji-preamble t t)
            t t)
           t t)))
    (if svg-p
        (replace-regexp-in-string
         "includegraphics" "includesvg"
         filled-preamble t t)
      filled-preamble)))

(defun org-latex-emoji-setup (&optional info)
  "Construct a preamble snippet to set up emojis based on INFO."
  (let* ((emoji-set
          (or (org-element-map
                  (plist-get info :parse-tree)
                  'keyword
                (lambda (keyword)
                  (and (string= (org-element-property :key keyword)
                                org-latex-emoji-keyword)
                       (org-element-property :value keyword)))
                info t)
              (caar org-latex-emoji-sets)))
         (emoji-spec (cdr (assoc emoji-set org-latex-emoji-sets)))
         (emoji-folder
          (expand-file-name emoji-set org-latex-emoji-base-dir))
         (emoji-svg-only
          (and (file-exists-p emoji-folder)
               (not (cl-some
                     (lambda (path)
                       (not (string= (file-name-extension path) "svg")))
                     (directory-files emoji-folder nil "\\....$"))))))
    (cond
     ((not emoji-spec)
      (error "Emoji set `%s' is unknown. Try one of: %s" emoji-set
             (string-join (mapcar #'car org-latex-emoji-sets) ", ")))
     ((not (file-exists-p emoji-folder))
      (if (and (not noninteractive)
               (yes-or-no-p (format "Emoji set `%s' is not installed, would you like to install it?" emoji-set)))
          (org-latex-emoji-install
           emoji-set
           (or (executable-find "cairosvg") (executable-find "inkscape")))
        (error "Emoji set `%s' is not installed" emoji-set))))
    (concat
     (org-latex-emoji-fill-preamble
      emoji-folder (plist-get emoji-spec :height)
      (plist-get emoji-spec :offset) emoji-svg-only)
     "\n\n"
     (mapconcat
      #'org-latex-emoji-declaration
      (let (unicode-cars)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward org-latex-emoji--rx nil t)
            (push (aref (match-string 0) 0) unicode-cars)))
        (cl-delete-duplicates unicode-cars))
      "\n")
     "\n")))

(org-export-update-features 'latex
  (emoji
   :condition (save-excursion
                (goto-char (point-min))
                (re-search-forward org-latex-emoji--rx nil t))
   :requires image
   :snippet org-latex-emoji-setup
   :order 3))
(org-export-update-features 'latex
  (emoji-lualatex-hack
   :condition t
   :when (emoji julia-code) ; LuaLaTeX is used with julia-code.
   :snippet
   "\\usepackage{newunicodechar}
\\newcommand{\\DeclareUnicodeCharacter}[2]{%
    \\begingroup\\lccode`|=\\string\"#1\\relax
    \\lowercase{\\endgroup\\newunicodechar{|}}{#2}}"
   :order 2.9))
(defun org-latex-emoji-install (set &optional convert)
  "Dowload, convert, and install emojis for use with LaTeX."
  (interactive
   (list (completing-read "Emoji set to install: "
                          (mapcar
                           (lambda (set-spec)
                             (if (file-exists-p (expand-file-name (car set-spec) org-latex-emoji-base-dir))
                                 (propertize (car set-spec) 'face 'font-lock-doc-face)
                               (car set-spec)))
                           org-latex-emoji-sets)
                          nil t)
         (if (or (executable-find "cairosvg") (executable-find "inkscape"))
             (yes-or-no-p "Would you like to create .pdf forms of the Emojis (strongly recommended)?")
           (message "Install `cairosvg' (recommended) or `inkscape' to convert to PDF forms")
           nil)))
  (let ((emoji-folder (expand-file-name set org-latex-emoji-base-dir)))
    (when (or (not (file-exists-p emoji-folder))
              (and (not noninteractive)
                   (yes-or-no-p "Emoji folder already present, would you like to re-download?")
                   (progn (delete-directory emoji-folder) t)))
      (let* ((spec (cdr (assoc set org-latex-emoji-sets)))
             (dir (org-latex-emoji-install--download set (plist-get spec :url)))
             (svg-dir (expand-file-name (or (plist-get spec :folder) "") dir)))
        (org-latex-emoji-install--install
         set svg-dir (plist-get spec :file-regexp))))
    (when convert
      (org-latex-emoji-install--convert (file-name-as-directory emoji-folder))))
  (message "Emojis set `%s' installed." set))

(defun org-latex-emoji-install--download (name url)
  "Download the emoji archive URL for the set NAME."
  (let* ((dest-folder (make-temp-file (format "%s-" name) t)))
    (message "Downloading %s..." name)
    (let ((default-directory dest-folder))
      (call-process "curl" nil nil nil "-sL" url "--output" "emojis.zip")
      (message "Unzipping")
      (call-process "unzip" nil nil nil "emojis.zip")
      dest-folder)))

(defun org-latex-emoji-install--install (name dir &optional filename-regexp)
  "Install the emoji files in DIR to the NAME set folder.
If a FILENAME-REGEXP, only files matching this regexp will be moved,
and they will be renamed to the first capture group of the regexp."
  (message "Installing %s emojis into emoji directory" name)
  (let ((images (append (directory-files dir t ".*.svg")
                        (directory-files dir t ".*.pdf")))
        (emoji-dir (file-name-as-directory
                    (expand-file-name name org-latex-emoji-base-dir))))
    (unless (file-exists-p emoji-dir)
      (make-directory emoji-dir t))
    (mapc
     (lambda (image)
       (if filename-regexp
           (when (string-match filename-regexp (file-name-nondirectory image))
             (rename-file image
                          (expand-file-name
                           (file-name-with-extension
                            (upcase (match-string 1 (file-name-nondirectory image)))
                            (file-name-extension image))
                           emoji-dir)
                          t))
         (rename-file image
                      (expand-file-name
                       (file-name-with-extension
                        (upcase (file-name-nondirectory image))
                        (file-name-extension image))
                       emoji-dir)
                      t)))
     images)
    (message "%d emojis installed" (length images))))

(defun org-latex-emoji-install--convert (dir)
  "Convert all .svg files in DIR to .pdf forms.
Uses cairosvg if possible, falling back to inkscape."
  (let ((default-directory dir))
    (if (executable-find "cairosvg") ; cairo's PDFs are ~10% smaller
        (let* ((images (directory-files dir nil ".*.svg"))
               (num-images (length images))
               (index 0)
               (max-threads (1- (string-to-number (shell-command-to-string "nproc"))))
               (threads 0))
          (while (< index num-images)
            (setf threads (1+ threads))
            (let (message-log-max)
              (message "Converting emoji %d/%d (%s)" (1+ index) num-images (nth index images)))
            (make-process :name "cairosvg"
                          :command (list "cairosvg" (nth index images) "-o" (concat (file-name-sans-extension (nth index images)) ".pdf"))
                          :sentinel (lambda (proc msg)
                                      (when (memq (process-status proc) '(exit signal))
                                        (setf threads (1- threads)))))
            (setq index (1+ index))
            (while (> threads max-threads)
              (sleep-for 0.01)))
          (while (> threads 0)
            (sleep-for 0.01)))
      (message "Cairosvg not found. Proceeding with inkscape as a fallback.")
      (shell-command "inkscape --batch-process --export-type='pdf' *.svg"))
    (message "Finished conversion!")))

(provide 'ox-latex-emoji))))

;;:------------------------
;;; !Pkg ox-chameleon
;;:------------------------

(confpkg-with-record '("load: pkg-ox-chameleon" config)
(use-package! ox-chameleon
  :after ox)

(provide 'config--pkg-ox-chameleon))

;;:------------------------
;;; ox-beamer
;;:------------------------

;; This block defines `org-beamer-metropolis-tweaks' and
;; `org-beamer-p'.

(confpkg-with-record '("hook: ox-beamer" set-hooks)
  (with-eval-after-load 'ox-beamer
(confpkg-with-record '("load: ox-beamer" load-hooks)
(setq org-beamer-theme "[progressbar=foot]metropolis")
(defun org-beamer-p (info)
  (eq 'beamer (and (plist-get info :back-end)
                   (org-export-backend-name (plist-get info :back-end)))))

(org-export-update-features 'beamer
  (beamer-setup
   :condition t
   :requires .missing-koma
   :prevents (italic-quotes condensed-lists cover-page)))

(org-export-update-features 'latex
  (.missing-koma
   :snippet "\\usepackage{scrextend}"
   :order 2))

(defvar org-beamer-metropolis-tweaks
  "\\NewCommandCopy{\\moldusetheme}{\\usetheme}
\\renewcommand*{\\usetheme}[2][]{\\moldusetheme[#1]{#2}
  \\setbeamertemplate{items}{$\\bullet$}
  \\setbeamerfont{block title}{size=\\normalsize, series=\\bfseries\\parbox{0pt}{\\rule{0pt}{4ex}}}}

\\makeatletter
\\newcommand{\\setmetropolislinewidth}{
  \\setlength{\\metropolis@progressinheadfoot@linewidth}{1.2px}}
\\makeatother

\\usepackage{etoolbox}
\\AtEndPreamble{\\setmetropolislinewidth}"
  "LaTeX preamble snippet that tweaks the Beamer metropolis theme styling.")

(org-export-update-features 'beamer
  (beamer-metropolis
   :condition (string-match-p "metropolis$" (plist-get info :beamer-theme))
   :snippet org-beamer-metropolis-tweaks
   :order 3))
(setq org-beamer-frame-level 2)

(provide 'config-ox-beamer))))

;;:------------------------
;;; !Pkg org-re-reveal
;;:------------------------

(confpkg-with-record '("hook: pkg-org-re-reveal" set-hooks)
  (with-eval-after-load 'org-re-reveal
(confpkg-with-record '("load: pkg-org-re-reveal" load-hooks)
(setq org-re-reveal-theme "white"
      org-re-reveal-transition "slide"
      org-re-reveal-plugins '(markdown notes math search zoom))

(provide 'config--pkg-org-re-reveal))))

;;:------------------------
;;; ox-ascii
;;:------------------------

;; This block defines `org-ascii-convert-latex'.

(confpkg-with-record '("hook: ox-ascii" set-hooks)
  (with-eval-after-load 'ox-ascii
(confpkg-with-record '("load: ox-ascii" load-hooks)
(setq org-ascii-charset 'utf-8)
(when (executable-find "latex2text")
  (after! ox-ascii
    (defvar org-ascii-convert-latex t
      "Use latex2text to convert LaTeX elements to unicode.")

    (defadvice! org-ascii-latex-environment-unicode-a (latex-environment _contents info)
      "Transcode a LATEX-ENVIRONMENT element from Org to ASCII, converting to unicode.
CONTENTS is nil.  INFO is a plist holding contextual
information."
      :override #'org-ascii-latex-environment
      (when (plist-get info :with-latex)
        (org-ascii--justify-element
         (org-remove-indentation
          (let* ((latex (org-element-property :value latex-environment))
                 (unicode (and (eq (plist-get info :ascii-charset) 'utf-8)
                               org-ascii-convert-latex
                               (doom-call-process "latex2text" "-q" "--code" latex))))
            (if (= (car unicode) 0) ; utf-8 set, and sucessfully ran latex2text
                (cdr unicode) latex)))
         latex-environment info)))

    (defadvice! org-ascii-latex-fragment-unicode-a (latex-fragment _contents info)
      "Transcode a LATEX-FRAGMENT object from Org to ASCII, converting to unicode.
CONTENTS is nil.  INFO is a plist holding contextual
information."
      :override #'org-ascii-latex-fragment
      (when (plist-get info :with-latex)
        (let* ((latex (org-element-property :value latex-fragment))
               (unicode (and (eq (plist-get info :ascii-charset) 'utf-8)
                             org-ascii-convert-latex
                             (doom-call-process "latex2text" "-q" "--code" latex))))
          (if (and unicode (= (car unicode) 0)) ; utf-8 set, and sucessfully ran latex2text
              (cdr unicode) latex))))))

(provide 'config-ox-ascii))))

;;:------------------------
;;; ox-md
;;:------------------------

;; This block defines `org-utf8-entity', `org-md-latex-environment',
;; and `org-md-latex-fragment'.

(confpkg-with-record '("hook: ox-md" set-hooks)
  (with-eval-after-load 'ox-md
(confpkg-with-record '("load: ox-md" load-hooks)
(defadvice! org-md-plain-text-unicode-a (orig-fn text info)
  "Locally rebind `org-html-special-string-regexps'"
  :around #'org-md-plain-text
  (let ((org-html-special-string-regexps
         '(("\\\\-" . "-")
           ("---\\([^-]\\|$\\)" . "—\\1")
           ("--\\([^-]\\|$\\)" . "–\\1")
           ("\\.\\.\\." . "…")
           ("<->" . "⟷")
           ("->" . "→")
           ("<-" . "←"))))
    (funcall orig-fn text (plist-put info :with-smart-quotes nil))))
(after! ox-md
  (defun org-md-latex-fragment (latex-fragment _contents info)
    "Transcode a LATEX-FRAGMENT object from Org to Markdown."
    (let ((frag (org-element-property :value latex-fragment)))
      (cond
       ((string-match-p "^\\\\(" frag)
        (concat "$" (substring frag 2 -2) "$"))
       ((string-match-p "^\\\\\\[" frag)
        (concat "$$" (substring frag 2 -2) "$$"))
       (t (message "unrecognised fragment: %s" frag)
          frag))))

  (defun org-md-latex-environment (latex-environment contents info)
    "Transcode a LATEX-ENVIRONMENT object from Org to Markdown."
    (concat "$$\n"
            (org-html-latex-environment latex-environment contents info)
            "$$\n"))

  (defun org-utf8-entity (entity _contents _info)
    "Transcode an ENTITY object from Org to utf-8.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
    (org-element-property :utf-8 entity))

  ;; We can't let this be immediately parsed and evaluated,
  ;; because eager macro-expansion tries to call as-of-yet
  ;; undefined functions.
  ;; NOTE in the near future this shouldn't be required
  (eval
   '(dolist (extra-transcoder
             '((latex-fragment . org-md-latex-fragment)
               (latex-environment . org-md-latex-environment)
               (entity . org-utf8-entity)))
      (unless (member extra-transcoder (org-export-backend-transcoders
                                        (org-export-get-backend 'md)))
        (push extra-transcoder (org-export-backend-transcoders
                                (org-export-get-backend 'md)))))))

(provide 'config-ox-md))))

;;:------------------------
;;; !Pkg ox-gfm
;;:------------------------

(confpkg-with-record '("load: pkg-ox-gfm" config)
(use-package! ox-gfm
  :after ox)

(provide 'config--pkg-ox-gfm))

;;:------------------------
;;; Org Babel
;;:------------------------

;; This block defines `org-babel-auto-async-languages'.

(confpkg-with-record '("load: org-babel" config)
(add-transient-hook! #'org-babel-execute-src-block
  (require 'ob-async))

(defvar org-babel-auto-async-languages '()
  "Babel languages which should be executed asyncronously by default.")

(defadvice! org-babel-get-src-block-info-eager-async-a (orig-fn &optional light datum)
  "Eagarly add an :async parameter to the src information, unless it seems problematic.
This only acts o languages in `org-babel-auto-async-languages'.
Not added when either:
+ session is not \"none\"
+ :sync is set"
  :around #'org-babel-get-src-block-info
  (let ((result (funcall orig-fn light datum)))
    (when (and (string= "none" (cdr (assoc :session (caddr result))))
               (member (car result) org-babel-auto-async-languages)
               (not (assoc :async (caddr result))) ; don't duplicate
               (not (assoc :sync (caddr result))))
      (push '(:async) (caddr result)))
    result))

(provide 'config-org-babel))

;;:------------------------
;;; Org ESS
;;:------------------------

(confpkg-with-record '("load: org-ess" config)
(setq ess-eval-visibly 'nowait)
(setq ess-R-font-lock-keywords
      '((ess-R-fl-keyword:keywords . t)
        (ess-R-fl-keyword:constants . t)
        (ess-R-fl-keyword:modifiers . t)
        (ess-R-fl-keyword:fun-defs . t)
        (ess-R-fl-keyword:assign-ops . t)
        (ess-R-fl-keyword:%op% . t)
        (ess-fl-keyword:fun-calls . t)
        (ess-fl-keyword:numbers . t)
        (ess-fl-keyword:operators . t)
        (ess-fl-keyword:delimiters . t)
        (ess-fl-keyword:= . t)
        (ess-R-fl-keyword:F&T . t)))
(after! org
  (add-to-list '+org-babel-mode-alist '(jags . ess-jags)))

(provide 'config-org-ess))

;;:------------------------
;;; LaTeX
;;:------------------------

;; This block defines `unimportant-latex-face',
;; `TeX-string-single-token-p', `TeX-fold-parenthesize-as-necessary',
;; `string-offset-apply-roman-char-exceptions',
;; `string-offset-roman-char-exceptions', `string-offset-roman-chars',
;; `tec/tex-delim-yas-expand', `tec/tex-next-char-smart-close-delim',
;; `tec/tex-close-delim-from-char', `tec/tex-open-delim-from-char',
;; `tec/get-open-delim-char', `tec/tex-delim-dot-second',
;; `tec/tex-last-delim-char', `tec/yas-latex-preamble-if', and
;; `tec/yas-latex-get-class-choice'.

(confpkg-with-record '("load: latex" config)
(setq TeX-save-query nil
      TeX-show-compilation t
      TeX-command-extra-options "-shell-escape")
(after! latex
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t)))
(setq +latex-viewers '(pdf-tools evince zathura okular skim sumatrapdf))
(setq tec/yas-latex-template-preamble "
\\usepackage[pdfa,unicode=true,hidelinks]{hyperref}

\\usepackage[dvipsnames,svgnames,table,hyperref]{xcolor}
\\renewcommand{\\UrlFont}{\\ttfamily\\small}

\\usepackage[a-2b]{pdfx} % why not be archival

\\usepackage[T1]{fontenc}
\\usepackage[osf]{newpxtext}  % Palatino
\\usepackage{gillius}
\\usepackage[scale=0.9]{sourcecodepro}

\\usepackage[varbb]{newpxmath}
\\usepackage{mathtools}
\\usepackage{amssymb}

\\usepackage[activate={true,nocompatibility},final,tracking=true,kerning=true,spacing=true,factor=2000]{microtype}
% microtype makes text look nicer

\\usepackage{graphicx} % include graphics

\\usepackage{booktabs} % nice table rules
")

(defun tec/yas-latex-get-class-choice ()
  "Prompt user for LaTeX class choice"
  (setq tec/yas-latex-class-choice (completing-read "Select document class: " '("article" "scrartcl" "bmc"))))

(defun tec/yas-latex-preamble-if ()
  "Based on class choice prompt for insertion of default preamble"
  (if (equal tec/yas-latex-class-choice "bmc") 'nil
    (eq (read-char-choice "Include default preamble? [Type y/n]" '(?y ?n)) ?y)))
(after! tex
  (defvar tec/tex-last-delim-char nil
    "Last open delim expanded in a tex document")
  (defvar tec/tex-delim-dot-second t
    "When the `tec/tex-last-delim-char' is . a second character (this) is prompted for")
  (defun tec/get-open-delim-char ()
    "Exclusivly read next char to tec/tex-last-delim-char"
    (setq tec/tex-delim-dot-second nil)
    (setq tec/tex-last-delim-char (read-char-exclusive "Opening deliminator, recognises: 9 ( [ { < | ."))
    (when (eql ?. tec/tex-last-delim-char)
      (setq tec/tex-delim-dot-second (read-char-exclusive "Other deliminator, recognises: 0 9 (  ) [ ] { } < > |"))))
  (defun tec/tex-open-delim-from-char (&optional open-char)
    "Find the associated opening delim as string"
    (unless open-char (setq open-char (if (eql ?. tec/tex-last-delim-char)
                                          tec/tex-delim-dot-second
                                        tec/tex-last-delim-char)))
    (pcase open-char
      (?\( "(")
      (?9  "(")
      (?\[ "[")
      (?\{ "\\{")
      (?<  "<")
      (?|  (if tec/tex-delim-dot-second "." "|"))
      (_   ".")))
  (defun tec/tex-close-delim-from-char (&optional open-char)
    "Find the associated closing delim as string"
    (if tec/tex-delim-dot-second
        (pcase tec/tex-delim-dot-second
          (?\) ")")
          (?0  ")")
          (?\] "]")
          (?\} "\\}")
          (?\> ">")
          (?|  "|")
          (_   "."))
      (pcase (or open-char tec/tex-last-delim-char)
        (?\( ")")
        (?9  ")")
        (?\[ "]")
        (?\{ "\\}")
        (?<  ">")
        (?\) ")")
        (?0  ")")
        (?\] "]")
        (?\} "\\}")
        (?\> ">")
        (?|  "|")
        (_   "."))))
  (defun tec/tex-next-char-smart-close-delim (&optional open-char)
    (and (bound-and-true-p smartparens-mode)
         (eql (char-after) (pcase (or open-char tec/tex-last-delim-char)
                             (?\( ?\))
                             (?\[ ?\])
                             (?{ ?})
                             (?< ?>)))))
  (defun tec/tex-delim-yas-expand (&optional open-char)
    (yas-expand-snippet (yas-lookup-snippet "_deliminators" 'latex-mode) (point) (+ (point) (if (tec/tex-next-char-smart-close-delim open-char) 2 1)))))
(after! latex
  (setcar (assoc "⋆" LaTeX-fold-math-spec-list) "★")) ;; make \star bigger

(setq TeX-fold-math-spec-list
      `(;; missing/better symbols
        ("≤" ("le"))
        ("≥" ("ge"))
        ("≠" ("ne"))
        ;; convenience shorts -- these don't work nicely ATM
        ;; ("‹" ("left"))
        ;; ("›" ("right"))
        ;; private macros
        ("ℝ" ("RR"))
        ("ℕ" ("NN"))
        ("ℤ" ("ZZ"))
        ("ℚ" ("QQ"))
        ("ℂ" ("CC"))
        ("ℙ" ("PP"))
        ("ℍ" ("HH"))
        ("𝔼" ("EE"))
        ("𝑑" ("dd"))
        ;; known commands
        ("" ("phantom"))
        (,(lambda (num den) (if (and (TeX-string-single-token-p num) (TeX-string-single-token-p den))
                                (concat num "／" den)
                              (concat "❪" num "／" den "❫"))) ("frac"))
        (,(lambda (arg) (concat "√" (TeX-fold-parenthesize-as-necessary arg))) ("sqrt"))
        (,(lambda (arg) (concat "⭡" (TeX-fold-parenthesize-as-necessary arg))) ("vec"))
        ("‘{1}’" ("text"))
        ;; private commands
        ("|{1}|" ("abs"))
        ("‖{1}‖" ("norm"))
        ("⌊{1}⌋" ("floor"))
        ("⌈{1}⌉" ("ceil"))
        ("⌊{1}⌉" ("round"))
        ("𝑑{1}/𝑑{2}" ("dv"))
        ("∂{1}/∂{2}" ("pdv"))
        ;; fancification
        ("{1}" ("mathrm"))
        (,(lambda (word) (string-offset-roman-chars 119743 word)) ("mathbf"))
        (,(lambda (word) (string-offset-roman-chars 119951 word)) ("mathcal"))
        (,(lambda (word) (string-offset-roman-chars 120003 word)) ("mathfrak"))
        (,(lambda (word) (string-offset-roman-chars 120055 word)) ("mathbb"))
        (,(lambda (word) (string-offset-roman-chars 120159 word)) ("mathsf"))
        (,(lambda (word) (string-offset-roman-chars 120367 word)) ("mathtt"))
        )
      TeX-fold-macro-spec-list
      '(
        ;; as the defaults
        ("[f]" ("footnote" "marginpar"))
        ("[c]" ("cite"))
        ("[l]" ("label"))
        ("[r]" ("ref" "pageref" "eqref"))
        ("[i]" ("index" "glossary"))
        ("..." ("dots"))
        ("{1}" ("emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt"
                "textbf" "textsc" "textup"))
        ;; tweaked defaults
        ("©" ("copyright"))
        ("®" ("textregistered"))
        ("™"  ("texttrademark"))
        ("[1]:||►" ("item"))
        ("❡❡ {1}" ("part" "part*"))
        ("❡ {1}" ("chapter" "chapter*"))
        ("§ {1}" ("section" "section*"))
        ("§§ {1}" ("subsection" "subsection*"))
        ("§§§ {1}" ("subsubsection" "subsubsection*"))
        ("¶ {1}" ("paragraph" "paragraph*"))
        ("¶¶ {1}" ("subparagraph" "subparagraph*"))
        ;; extra
        ("⬖ {1}" ("begin"))
        ("⬗ {1}" ("end"))
        ))

(defun string-offset-roman-chars (offset word)
  "Shift the codepoint of each character in WORD by OFFSET with an extra -6 shift if the letter is lowercase"
  (apply 'string
         (mapcar (lambda (c)
                   (string-offset-apply-roman-char-exceptions
                    (+ (if (>= c 97) (- c 6) c) offset)))
                 word)))

(defvar string-offset-roman-char-exceptions
  '(;; lowercase serif
    (119892 .  8462) ; ℎ
    ;; lowercase caligraphic
    (119994 . 8495) ; ℯ
    (119996 . 8458) ; ℊ
    (120004 . 8500) ; ℴ
    ;; caligraphic
    (119965 . 8492) ; ℬ
    (119968 . 8496) ; ℰ
    (119969 . 8497) ; ℱ
    (119971 . 8459) ; ℋ
    (119972 . 8464) ; ℐ
    (119975 . 8466) ; ℒ
    (119976 . 8499) ; ℳ
    (119981 . 8475) ; ℛ
    ;; fraktur
    (120070 . 8493) ; ℭ
    (120075 . 8460) ; ℌ
    (120076 . 8465) ; ℑ
    (120085 . 8476) ; ℜ
    (120092 . 8488) ; ℨ
    ;; blackboard
    (120122 . 8450) ; ℂ
    (120127 . 8461) ; ℍ
    (120133 . 8469) ; ℕ
    (120135 . 8473) ; ℙ
    (120136 . 8474) ; ℚ
    (120137 . 8477) ; ℝ
    (120145 . 8484) ; ℤ
    )
  "An alist of deceptive codepoints, and then where the glyph actually resides.")

(defun string-offset-apply-roman-char-exceptions (char)
  "Sometimes the codepoint doesn't contain the char you expect.
Such special cases should be remapped to another value, as given in `string-offset-roman-char-exceptions'."
  (if (assoc char string-offset-roman-char-exceptions)
      (cdr (assoc char string-offset-roman-char-exceptions))
    char))

(defun TeX-fold-parenthesize-as-necessary (tokens &optional suppress-left suppress-right)
  "Add ❪ ❫ parenthesis as if multiple LaTeX tokens appear to be present"
  (if (TeX-string-single-token-p tokens) tokens
    (concat (if suppress-left "" "❪")
            tokens
            (if suppress-right "" "❫"))))

(defun TeX-string-single-token-p (teststring)
  "Return t if TESTSTRING appears to be a single token, nil otherwise"
  (if (string-match-p "^\\\\?\\w+$" teststring) t nil))
(after! tex
  (map!
   :map LaTeX-mode-map
   :ei [C-return] #'LaTeX-insert-item)
  (setq TeX-electric-math '("\\(" . "")))
;; Making \( \) less visible
(defface unimportant-latex-face
  '((t :inherit font-lock-comment-face :weight extra-light))
  "Face used to make \\(\\), \\[\\] less visible."
  :group 'LaTeX-math)

(font-lock-add-keywords
 'latex-mode
 `(("\\\\[]()[]" 0 'unimportant-latex-face prepend))
 'end)

;; (font-lock-add-keywords
;;  'latex-mode
;;  '(("\\\\[[:word:]]+" 0 'font-lock-keyword-face prepend))
;;  'end)
(setq preview-LaTeX-command '("%`%l \"\\nonstopmode\\nofiles\
\\PassOptionsToPackage{" ("," . preview-required-option-list) "}{preview}\
\\AtBeginDocument{\\ifx\\ifPreview\\undefined"
preview-default-preamble "\\fi}\"%' \"\\detokenize{\" %t \"}\""))
(after! cdlatex
  (setq cdlatex-env-alist
        '(("bmatrix" "\\begin{bmatrix}\n?\n\\end{bmatrix}" nil)
          ("equation*" "\\begin{equation*}\n?\n\\end{equation*}" nil)))
  (setq ;; cdlatex-math-symbol-prefix ?\; ;; doesn't work at the moment :(
   cdlatex-math-symbol-alist
   '( ;; adding missing functions to 3rd level symbols
     (?_    ("\\downarrow"  ""           "\\inf"))
     (?2    ("^2"           "\\sqrt{?}"     ""     ))
     (?3    ("^3"           "\\sqrt[3]{?}"  ""     ))
     (?^    ("\\uparrow"    ""           "\\sup"))
     (?k    ("\\kappa"      ""           "\\ker"))
     (?m    ("\\mu"         ""           "\\lim"))
     (?c    (""             "\\circ"     "\\cos"))
     (?d    ("\\delta"      "\\partial"  "\\dim"))
     (?D    ("\\Delta"      "\\nabla"    "\\deg"))
     ;; no idea why \Phi isnt on 'F' in first place, \phi is on 'f'.
     (?F    ("\\Phi"))
     ;; now just convenience
     (?.    ("\\cdot" "\\dots"))
     (?:    ("\\vdots" "\\ddots"))
     (?*    ("\\times" "\\star" "\\ast")))
   cdlatex-math-modify-alist
   '( ;; my own stuff
     (?B    "\\mathbb"        nil          t    nil  nil)
     (?a    "\\abs"           nil          t    nil  nil))))
(after! tex
  (add-to-list 'TeX-view-program-list '("Evince" "evince %o"))
  (add-to-list 'TeX-view-program-selection '(output-pdf "Evince")))
(when EMACS28+
  (add-hook 'latex-mode-hook #'TeX-latex-mode))

(provide 'config-latex))

;;:------------------------
;;; !Pkg LAAS
;;:------------------------

;; This block defines `laas-tex-fold-maybe'.

(confpkg-with-record '("load: pkg-laas" config)
(use-package! laas
  :hook (LaTeX-mode . laas-mode)
  :config
  (defun laas-tex-fold-maybe ()
    (unless (equal "/" aas-transient-snippet-key)
      (+latex-fold-last-macro-a)))
  (add-hook 'aas-post-snippet-expand-hook #'laas-tex-fold-maybe))

(provide 'config--pkg-laas))

;;:------------------------
;;; !Pkg pdftotext
;;:------------------------

(confpkg-with-record '("load: pkg-pdftotext" config)
;; (package! pdftotext :recipe (:host github :repo "tecosaur/pdftotext"))
;; (use-package! pdftotext
;;   :init
;;   (unless (display-graphic-p)
;;     (add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdftotext-mode))
;;     (add-to-list 'magic-mode-alist '("%PDF" . pdftotext-mode)))
;;   :config
;;   (unless (display-graphic-p) (after! pdf-tools (pdftotext-install)))
;;   ;; For prettyness
;;   (add-hook 'pdftotext-mode-hook #'spell-fu-mode-disable)
;;   (add-hook 'pdftotext-mode-hook (lambda () (page-break-lines-mode 1)))
;;   ;; I have no idea why this is needed
;;   (map! :map pdftotext-mode-map
;;         "<mouse-4>" (cmd! (scroll-down mouse-wheel-scroll-amount-horizontal))
;;         "<mouse-5>" (cmd! (scroll-up mouse-wheel-scroll-amount-horizontal))))


(provide 'config--pkg-pdftotext))

;;:------------------------
;;; R lang
;;:------------------------

(confpkg-with-record '("load: r-lang" config)
(after! ess-r-mode
  (appendq! +ligatures-extra-symbols
            '(:assign "⟵"
              :multiply "×"))
  (set-ligatures! 'ess-r-mode
    ;; Functional
    :def "function"
    ;; Types
    :null "NULL"
    :true "TRUE"
    :false "FALSE"
    :int "int"
    :floar "float"
    :bool "bool"
    ;; Flow
    :not "!"
    :and "&&" :or "||"
    :for "for"
    :in "%in%"
    :return "return"
    ;; Other
    :assign "<-"
    :multiply "%*%"))

(provide 'config-r-lang))

;;:------------------------
;;; Julia
;;:------------------------

(confpkg-with-record '("hook: julia" set-hooks)
  (with-eval-after-load 'julia-mode
(confpkg-with-record '("load: julia" load-hooks)
(add-hook 'julia-mode-hook #'rainbow-delimiters-mode-enable)
(add-hook! 'julia-mode-hook
  (setq-local lsp-enable-folding t
              lsp-folding-range-limit 100))

(provide 'config-julia))))

;;:------------------------
;;; conf-data-toml
;;:------------------------

(confpkg-with-record '("load: conf-data-toml" config)
(use-package! conf-data-toml
  :magic ("\\`data_config_version = [0-9]" . conf-data-toml-mode))

(provide 'config-conf-data-toml))

;;:------------------------
;;; !Pkg graphviz-dot-mode
;;:------------------------

(confpkg-with-record '("load: pkg-graphviz-dot-mode" config)
(use-package! graphviz-dot-mode
  :commands graphviz-dot-mode
  :mode ("\\.dot\\'" . graphviz-dot-mode)
  :init
  (after! org
    (setcdr (assoc "dot" org-src-lang-modes)
            'graphviz-dot)))

(use-package! company-graphviz-dot
  :after graphviz-dot-mode)

(provide 'config--pkg-graphviz-dot-mode))

;;:------------------------
;;; Markdown
;;:------------------------

(confpkg-with-record '("load: markdown" config)
(add-hook! (gfm-mode markdown-mode) #'visual-line-mode #'turn-off-auto-fill)
(custom-set-faces!
  '(markdown-header-face-1 :height 1.25 :weight extra-bold :inherit markdown-header-face)
  '(markdown-header-face-2 :height 1.15 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-3 :height 1.08 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-4 :height 1.00 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-5 :height 0.90 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-6 :height 0.75 :weight extra-bold :inherit markdown-header-face))

(provide 'config-markdown))

;;:------------------------
;;; !Pkg Beancount
;;:------------------------

;; This block defines `beancount-bal'.

(confpkg-with-record '("load: pkg-beancount" config)
(use-package! beancount
  :mode ("\\.beancount\\'" . beancount-mode)
  :init
  (after! all-the-icons
    (add-to-list 'all-the-icons-icon-alist
                 '("\\.beancount\\'" all-the-icons-material "attach_money" :face all-the-icons-lblue))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(beancount-mode all-the-icons-material "attach_money" :face all-the-icons-lblue)))
  :config
  (setq beancount-electric-currency t)
  (defun beancount-bal ()
    "Run bean-report bal."
    (interactive)
    (let ((compilation-read-command nil))
      (beancount--run "bean-report"
                      (file-relative-name buffer-file-name) "bal")))
  (map! :map beancount-mode-map
        :n "TAB" #'beancount-align-to-previous-number
        :i "RET" (cmd! (newline-and-indent) (beancount-align-to-previous-number))))

(provide 'config--pkg-beancount))

;;:------------------------
;;; gimp-palette
;;:------------------------

;; This block defines `gimp-palette-update-line' and
;; `gimp-palette-update-region'.

(confpkg-with-record '("load: gimp-palette" config)
(define-derived-mode gimp-palette-mode fundamental-mode "GIMP Palette"
  "A major mode for GIMP Palette (.gpl) files that keeps RGB and Hex colors in sync."
  (when (require 'rainbow-mode)
    (rainbow-mode 1))
  (when (bound-and-true-p hl-line-mode)
    (hl-line-mode -1))
  (add-hook 'after-change-functions #'gimp-palette-update-region nil t))
(defun gimp-palette-update-region (beg end &optional _)
  "Update each line between BEG and END with `gimp-palette-update-line'.
If run interactively without a region set, the whole buffer is affected."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (let ((marker (prepare-change-group)))
    (unwind-protect
        (save-excursion
          (goto-char beg)
          (while (< (point) end)
            (gimp-palette-update-line)
            (forward-line 1)))
      (undo-amalgamate-change-group marker))))
(defun gimp-palette-update-line ()
  "Update the RGB and Hex colour codes on the current line.
Whichever `point' is currently on is taken as the source of truth."
  (interactive)
  (let ((column (current-column))
        (ipoint (point)))
    (beginning-of-line)
    (when (and (re-search-forward "\\=\\([0-9 ]*\\)\\(#[0-9A-Fa-f]\\{6\\}\\)" nil t)
               (<= column (length (match-string 0))))
      (cond
       ((>= column (length (match-string 1))) ; Point in #HEX
        (cl-destructuring-bind (r g b) (color-name-to-rgb (match-string 2))
          (replace-match
           (format "%3d %3d %3d "
                   (round (* 255 r))
                   (round (* 255 g))
                   (round (* 255 b)))
           nil t nil 1)))
       ((string-match-p "\\`[0-9]+ +[0-9]+ +[0-9]+\\'" (match-string 1)) ; Valid R G B
        (cl-destructuring-bind (r g b)
            (mapcar #'string-to-number
                    (save-match-data
                      (split-string (match-string 1) " +" t)))
          (replace-match
           (format "%3d %3d %3d " r g b)
           nil t nil 1)
          (replace-match
           (color-rgb-to-hex (/ r 255.0) (/ g 255.0) (/ b 255.0) 2)
           nil t nil 2)))))
    (goto-char ipoint)))
(add-to-list 'magic-mode-alist (cons "\\`GIMP Palette\n" #'gimp-palette-mode))

(provide 'config-gimp-palette))

(confpkg-finish-record 'config)

;;; config.el ends here