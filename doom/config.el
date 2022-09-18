;;; config.el -*- lexical-binding: t; -*-

;; [[file:config.org::*Personal Information][Personal Information:1]]
(setq user-full-name "VitalyR"
      user-mail-address "vitalyankh@gmail.com")
;; Personal Information:1 ends here

;; [[file:config.org::*Personal Information][Personal Information:2]]
(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry nil) ; default is 7200 (2h)
;; Personal Information:2 ends here

;; [[file:config.org::*Workaround][Workaround:1]]
(defadvice copilot--complete-post-command (around intercept activate)
  (condition-case err
      ad-do-it
    ;; Let the debugger run
    ((debug error) (signal (car err) (cdr err)))))
(setq straight-repository-branch "develop")
;; Workaround:1 ends here

;; [[file:config.org::*Workaround][Workaround:3]]
(use-package clang-format+
  :config
  (add-hook 'c-mode-common-hook #'clang-format+-mode)
  (setq clang-format+-context 'modification)
  (setq clang-format+-always-enable t)
  )

(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode    ; elisp's mechanisms are good enough
            cpp-mode  ; use clang-format+ for C/C++
            c++-mode
            c-mode
            org-msg-edit-mode)) ; doesn't need a formatter

;; fix org mode async export
(defadvice! fixed-+org--fix-async-export-a (fn &rest args)
  :override #'+org--fix-async-export-a
  (let ((old-async-init-file org-export-async-init-file)
        (org-export-async-init-file (make-temp-file "doom-org-async-export")))
    (with-temp-file org-export-async-init-file
      (prin1 `(progn (setq org-export-async-debug
                           ,(or org-export-async-debug
                                debug-on-error)
                           load-path ',load-path)
                     (unwind-protect
                         (let ((file ,old-async-init-file))
                           (if file
                               (load file nil t)
                             (load ,early-init-file nil t)
                             (require 'doom-start)))
                       (delete-file load-file-name)))
             (current-buffer)))
    (apply fn args)))
;; Workaround:3 ends here

;; [[file:config.org::*Workaround][Workaround:4]]
(after! projectile
  (add-to-list 'projectile-project-root-files "stack.yaml"))
;; Workaround:4 ends here

;; [[file:config.org::*Simple settings][Simple settings:1]]
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
;; x-stretch-cursor t                              ; Stretch cursor to the glyph width
)

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      auto-save-default t                         ; Nobody likes to lose work, I certainly don't
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      scroll-preserve-screen-position 'always     ; Don't have `point' jump around
      scroll-margin 2                             ; It's nice to maintain a little margin
      word-wrap-by-category t                     ; Different languages live together happily
      org-return-follows-link t)                  ; Organise it!

;;(display-time-mode 1)                             ; Enable time in the mode-line

;; This messes things up, disable for now
;;(unless (string-match-p "^Power N/A" (battery)) ; On laptops...
;;  (display-battery-mode 1))                     ; it's nice to know how much power you have

(global-subword-mode 1)                           ; Iterate through CamelCase words

(scroll-bar-mode)                                 ; I like scroll bars

;; Useset C-z which is bound to =suspend-frame= by default
(global-unset-key (kbd "C-z"))
;; Simple settings:1 ends here

;; [[file:config.org::*Frame sizing][Frame sizing:1]]
;;(add-to-list 'default-frame-alist '(height . 24))
;;(add-to-list 'default-frame-alist '(width . 80))
(push  '(alpha-background . 95) default-frame-alist)
(add-to-list 'initial-frame-alist '(fullscreen . maximized)
             )
;; Frame sizing:1 ends here

;; [[file:config.org::*Auto-customisations][Auto-customisations:1]]
(setq-default custom-file (expand-file-name ".custom.el" doom-user-dir))
(when (file-exists-p custom-file)
  (load custom-file))
;; Auto-customisations:1 ends here

;; [[file:config.org::*Mouse][Mouse:1]]
(setq mouse-yank-at-point nil)
;; Mouse:1 ends here

;; [[file:config.org::*Mouse][Mouse:2]]
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-large-scroll-height 60
      pixel-scroll-precision-interpolation-factor 30.0)
;; Mouse:2 ends here

;; [[file:config.org::*Buffer defaults][Buffer defaults:1]]
;; (setq-default major-mode 'org-mode)
;; Buffer defaults:1 ends here

;; [[file:config.org::*Drag text from emacs to other apps][Drag text from emacs to other apps:1]]
(setq
 mouse-drag-and-drop-region-cross-program t
 mouse-drag-and-drop-region t)
;; Drag text from emacs to other apps:1 ends here

;; [[file:config.org::*Font Face][Font Face:1]]
(setq doom-font (font-spec :family "JetBrains Mono" :weight 'extra-light :size 19)
      doom-big-font (font-spec :family "JetBrains Mono" :weight 'extra-light :size 36)
      doom-variable-pitch-font (font-spec :family "CMU Typewriter Text" :size 23)
      doom-serif-font (font-spec :family "CMU Typewriter Text" :weight 'light :size 23))
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
                             (set-fontset-font "fontset-default" 'kana "Noto Sans CJK JP Regular" nil 'prepend)
                             (set-fontset-font "fontset-default" 'hangul "Noto Sans CJK KR Regular" nil 'prepend)
                             (set-fontset-font "fontset-default" 'cjk-misc "Noto Sans CJK SC Regular" nil 'prepend)
                             ;; Cyrillic: Привет, Здравствуйте, Здраво, Здравейте
                             ;;(set-fontset-font "fontset-default" 'cyrillic "Noto Sans CJK SC Regular" nil 'prepend)
                             ))
;; Font Face:1 ends here

;; [[file:config.org::*Font Face][Font Face:3]]
nil
;; Font Face:3 ends here

;; [[file:config.org::*Theme and modeline][Theme and modeline:1]]
(setq doom-theme 'doom-one-light)
(use-package doom-themes
  :config
  ;;Global settings (defaults)
  (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
        doom-themes-enable-italic t ; if nil, italics is universally disabled
        doom-themes-treemacs-enable-variable-pitch nil)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  )
;; (remove-hook 'window-setup-hook #'doom-init-theme-h)
;; (add-hook 'after-init-hook #'doom-init-theme-h 'append)
;; (delq! t custom-theme-load-path)
;; Theme and modeline:1 ends here

;; [[file:config.org::*Theme and modeline][Theme and modeline:2]]
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))
;; Theme and modeline:2 ends here

;; [[file:config.org::*Theme and modeline][Theme and modeline:3]]
(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)
;; Theme and modeline:3 ends here

;; [[file:config.org::*Miscellaneous][Miscellaneous:1]]
(add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))
;; Miscellaneous:1 ends here

;; [[file:config.org::*Miscellaneous][Miscellaneous:2]]
(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")
;; Miscellaneous:2 ends here

;; [[file:config.org::*Dashboard quick actions][Dashboard quick actions:1]]
(defun +doom-dashboard-setup-modified-keymap ()
  (setq +doom-dashboard-mode-map (make-sparse-keymap))
  (map! :map +doom-dashboard-mode-map
        :desc "Find file" :ne "f" #'find-file
        :desc "Recent files" :ne "r" #'consult-recent-file
        :desc "Config dir" :ne "C" #'doom/open-private-config
        :desc "Open config.org" :ne "c" (cmd! (find-file (expand-file-name "config.org" doom-user-dir)))
        :desc "Open dotfile" :ne "." (cmd! (doom-project-find-file "~/.config/"))
        :desc "Notes (roam)" :ne "n" #'org-roam-node-find
        :desc "Switch buffer" :ne "b" #'+vertico/switch-workspace-buffer
        :desc "Switch buffers (all)" :ne "B" #'consult-buffer
        :desc "IBuffer" :ne "i" #'ibuffer
        :desc "Previous buffer" :ne "p" #'previous-buffer
        :desc "Set theme" :ne "t" #'consult-theme
        :desc "Quit" :ne "Q" #'save-buffers-kill-terminal
        :desc "Show keybindings" :ne "h" (cmd! (which-key-show-keymap '+doom-dashboard-mode-map))))

(add-transient-hook! #'+doom-dashboard-mode (+doom-dashboard-setup-modified-keymap))
(add-transient-hook! #'+doom-dashboard-mode :append (+doom-dashboard-setup-modified-keymap))
(add-hook! 'doom-init-ui-hook :append (+doom-dashboard-setup-modified-keymap))
;; Dashboard quick actions:1 ends here

;; [[file:config.org::*Dashboard quick actions][Dashboard quick actions:2]]
(map! :leader :desc "Dashboard" "d" #'+doom-dashboard/open)
;; Dashboard quick actions:2 ends here

;; [[file:config.org::*Mouse buttons][Mouse buttons:1]]
;;(map! :n [mouse-8] #'better-jumper-jump-backward
;;      :n [mouse-9] #'better-jumper-jump-forward)
;; Mouse buttons:1 ends here

;; [[file:config.org::*Window title][Window title:1]]
(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))
;; Window title:1 ends here

;; [[file:config.org::*Splash screen][Splash screen:1]]
(defvar fancy-splash-image-template
  (expand-file-name "misc/splash-images/emacs-e-template.svg" doom-user-dir)
  "Default template svg used for the splash image, with substitutions from ")

(defvar fancy-splash-sizes
  `((:height 300 :min-height 50 :padding (0 . 2))
    (:height 250 :min-height 42 :padding (2 . 4))
    (:height 200 :min-height 35 :padding (3 . 3))
    (:height 150 :min-height 28 :padding (3 . 3))
    (:height 100 :min-height 20 :padding (2 . 2))
    (:height 75  :min-height 15 :padding (2 . 1))
    (:height 50  :min-height 10 :padding (1 . 0))
    (:height 1   :min-height 0  :padding (0 . 0)))
  "list of plists with the following properties
  :height the height of the image
  :min-height minimum `frame-height' for image
  :padding `+doom-dashboard-banner-padding' (top . bottom) to apply
  :template non-default template file
  :file file to use instead of template")

(defvar fancy-splash-template-colours
  '(("$colour1" . keywords) ("$colour2" . type) ("$colour3" . base5) ("$colour4" . base8))
  "list of colour-replacement alists of the form (\"$placeholder\" . 'theme-colour) which applied the template")

(unless (file-exists-p (expand-file-name "theme-splashes" doom-cache-dir))
  (make-directory (expand-file-name "theme-splashes" doom-cache-dir) t))

(defun fancy-splash-filename (theme-name height)
  (expand-file-name (concat (file-name-as-directory "theme-splashes")
                            theme-name
                            "-" (number-to-string height) ".svg")
                    doom-cache-dir))

(defun fancy-splash-clear-cache ()
  "Delete all cached fancy splash images"
  (interactive)
  (delete-directory (expand-file-name "theme-splashes" doom-cache-dir) t)
  (message "Cache cleared!"))

(defun fancy-splash-generate-image (template height)
  "Read TEMPLATE and create an image if HEIGHT with colour substitutions as
   described by `fancy-splash-template-colours' for the current theme"
  (with-temp-buffer
    (insert-file-contents template)
    (re-search-forward "$height" nil t)
    (replace-match (number-to-string height) nil nil)
    (dolist (substitution fancy-splash-template-colours)
      (goto-char (point-min))
      (while (re-search-forward (car substitution) nil t)
        (replace-match (doom-color (cdr substitution)) nil nil)))
    (write-region nil nil
                  (fancy-splash-filename (symbol-name doom-theme) height) nil nil)))

(defun fancy-splash-generate-images ()
  "Perform `fancy-splash-generate-image' in bulk"
  (dolist (size fancy-splash-sizes)
    (unless (plist-get size :file)
      (fancy-splash-generate-image (or (plist-get size :template)
                                       fancy-splash-image-template)
                                   (plist-get size :height)))))

(defun ensure-theme-splash-images-exist (&optional height)
  (unless (file-exists-p (fancy-splash-filename
                          (symbol-name doom-theme)
                          (or height
                              (plist-get (car fancy-splash-sizes) :height))))
    (fancy-splash-generate-images)))

(defun get-appropriate-splash ()
  (let ((height (frame-height)))
    (cl-some (lambda (size) (when (>= height (plist-get size :min-height)) size))
             fancy-splash-sizes)))

(setq fancy-splash-last-size nil)
(setq fancy-splash-last-theme nil)
(defun set-appropriate-splash (&rest _)
  (let ((appropriate-image (get-appropriate-splash)))
    (unless (and (equal appropriate-image fancy-splash-last-size)
                 (equal doom-theme fancy-splash-last-theme)))
    (unless (plist-get appropriate-image :file)
      (ensure-theme-splash-images-exist (plist-get appropriate-image :height)))
    (setq fancy-splash-image
          (or (plist-get appropriate-image :file)
              (fancy-splash-filename (symbol-name doom-theme) (plist-get appropriate-image :height))))
    (setq +doom-dashboard-banner-padding (plist-get appropriate-image :padding))
    (setq fancy-splash-last-size appropriate-image)
    (setq fancy-splash-last-theme doom-theme)
    (+doom-dashboard-reload)))

(add-hook 'window-size-change-functions #'set-appropriate-splash)
(add-hook 'doom-load-theme-hook #'set-appropriate-splash)
;; Splash screen:1 ends here

;; [[file:config.org::*Splash screen][Splash screen:2]]
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

(defun splase-phrase-set-random-set ()
  "Set a new random splash phrase set."
  (interactive)
  (setq splash-phrase-set
        (nth (random (1- (length splash-phrase-sources)))
             (cl-set-difference (mapcar #'car splash-phrase-sources) (list splash-phrase-set))))
  (+doom-dashboard-reload t))

(defvar splase-phrase--cache nil)

(defun splash-phrase-get-from-file (file)
  "Fetch a random line from FILE."
  (let ((lines (or (cdr (assoc file splase-phrase--cache))
                   (cdar (push (cons file
                                     (with-temp-buffer
                                       (insert-file-contents (expand-file-name file splash-phrase-source-folder))
                                       (split-string (string-trim (buffer-string)) "\n")))
                               splase-phrase--cache)))))
    (nth (random (length lines)) lines)))

(defun splash-phrase (&optional set)
  "Construct a splash phrase from SET. See `splash-phrase-sources'."
  (mapconcat
   #'splash-phrase-get-from-file
   (cdr (assoc (or set splash-phrase-set) splash-phrase-sources))
   " "))

(defun doom-dashboard-phrase ()
  "Get a splash phrase, flow it over multiple lines as needed, and make fontify it."
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

(defadvice! doom-dashboard-widget-loaded-with-phrase ()
  :override #'doom-dashboard-widget-loaded
  (setq line-spacing 0.2)
  (insert
   "\n\n"
   (propertize
    (+doom-dashboard--center
     +doom-dashboard--width
     (doom-display-benchmark-h 'return))
    'face 'doom-dashboard-loaded)
   "\n"
   (doom-dashboard-phrase)
   "\n"))
;; Splash screen:2 ends here

;; [[file:config.org::*Splash screen][Splash screen:3]]
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1) (global-hl-line-mode nil))
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)
;; Splash screen:3 ends here

;; [[file:config.org::*Splash screen][Splash screen:4]]
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
;; Splash screen:4 ends here

;; [[file:config.org::daemon initialisation][daemon initialisation]]
(defun greedily-do-daemon-setup ()
  (require 'org)
  (when (require 'mu4e nil t)
    (setq mu4e-confirm-quit t)
    (setq +mu4e-lock-greedy t)
    (setq +mu4e-lock-relaxed t)
    (+mu4e-lock-add-watcher)
    (when (+mu4e-lock-available t)
      (mu4e~start)))
  (when (require 'elfeed nil t)
    (run-at-time nil (* 8 60 60) #'elfeed-update)))

(when (daemonp)
  (add-hook 'emacs-startup-hook #'greedily-do-daemon-setup)
  (add-hook! 'server-after-make-frame-hook
    (unless (string-match-p "\\*draft\\|\\*stdin\\|emacs-everywhere" (buffer-name))
      (switch-to-buffer +doom-dashboard-name))))
;; daemon initialisation ends here

;; [[file:config.org::*Prompt to run setup script][Prompt to run setup script:2]]
nil
;; Prompt to run setup script:2 ends here

;; [[file:config.org::*Which-key][Which-key:1]]
(setq which-key-idle-delay 0.5) ;; I need the help, I really do
;; Which-key:1 ends here

;; [[file:config.org::*Which-key][Which-key:2]]
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))
;; Which-key:2 ends here

;; [[file:config.org::*Very large files][Very large files:2]]
;; (use-package! vlf-setup
;;   :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)
;; Very large files:2 ends here

;; [[file:config.org::*Dirvish][Dirvish:1]]
(setq dirvish-hide-details t
      dirvish-side-follow-buffer-file t
      )
;; Dirvish:1 ends here

;; [[file:config.org::*Straight][Straight:1]]
(use-package straight
  :config
  (add-to-list 'straight-built-in-pseudo-packages 'org)
  )
;; Straight:1 ends here

;; [[file:config.org::*Eros][Eros:1]]
(setq eros-eval-result-prefix "⟹ ") ; default =>
;; Eros:1 ends here

;; [[file:config.org::*Meow][Meow:2]]
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
  ;;(define-key doom-leader-workspaces/windows-map (kbd "t") 'treemacs-select-window)
  (global-set-key (kbd "M-j") 'kmacro-start-macro-or-insert-counter)
  (global-set-key (kbd "M-k") 'kmacro-end-or-call-macro)
  ;; for doom emacs
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
         :desc "New empty buffer"            "N"   #'evil-buffer-new
         :desc "Kill other buffers"          "O"   #'doom/kill-other-buffers
         :desc "Previous buffer"             "p"   #'previous-buffer
         :desc "Revert buffer"               "r"   #'revert-buffer
         :desc "Save buffer"                 "s"   #'basic-save-buffer
         :desc "Save all buffers"            "S"   #'evil-write-all
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
   ;;'("<escape>" . mode-line-other-buffer)
   )
  ;; Let =C-[= be the =ESC= of =evil= in =meow=:
  (defun meow-insert-define-key (&rest keybindings)
    "Define key for insert state.

Usage:
  (meow-insert-define-key
   '(\"C-<space>\" . meow-insert-exit))"
    (mapcar (lambda (key-ref)
              (define-key meow-insert-state-keymap
                (kbd (car key-ref))
                (meow--parse-def (cdr key-ref))))
            keybindings))
  (meow-insert-define-key
   '("\C-[" . meow-insert-exit))

  (setq meow-expand-exclude-mode-list nil)
  (setq meow-expand-hint-remove-delay 1024)
  (define-key input-decode-map (kbd "C-[") [control-bracketleft])
  (define-key meow-insert-state-keymap [control-bracketleft] 'meow-insert-exit)
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
;; Meow:2 ends here

;; [[file:config.org::*Copilot][Copilot:2]]
(customize-set-variable 'copilot-enable-predicates '((lambda () (eq (meow--current-state) 'insert))))
(defun my-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (company-indent-or-complete-common nil)))
;; complete by copilot first, then company-mode
(use-package! copilot
  :after company
  :hook (prog-mode . copilot-mode)
  :hook (text-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map company-active-map
         ("<tab>" . 'my-tab)
         ("TAB" . 'my-tab)
         :map company-mode-map
         ("<tab>" . 'my-tab)
         ("TAB" . 'my-tab)))
;; (use-package! copilot
;;   :after corfu
;;   :hook (prog-mode . copilot-mode)
;;   :hook (text-mode . copilot-mode)
;;   :bind (("C-TAB" . 'copilot-accept-completion-by-word)
;;          ("C-<tab>" . 'copilot-accept-completion-by-word)
;;          :map corfu-map
;;          ("<tab>" . 'my-tab)
;;          ("TAB" . 'my-tab)))
;; Copilot:2 ends here

;; [[file:config.org::*Annotate][Annotate:2]]
;; (use-package annotate
;;   :after org
;;   :hook
;;   (((text-mode prog-mode) . annotate-mode))
;;   :config
;;   (setq annotate-file (convert-standard-filename
;;                        (file-name-concat org-directory "annotations")
;;                        )))
;; Annotate:2 ends here

;; [[file:config.org::*Consult][Consult:1]]
(after! consult
  (set-face-attribute 'consult-file nil :inherit 'consult-buffer)
  (setf (plist-get (alist-get 'perl consult-async-split-styles-alist) :initial) ";"))
;; Consult:1 ends here

;; [[file:config.org::*Commit message templates][Commit message templates:1]]
(defvar +magit-project-commit-templates-alist nil
  "Alist of toplevel dirs and template strings/functions.")
(after! magit
  (defun +magit-fill-in-commit-template ()
    "Insert template from `+magit-fill-in-commit-template' if applicable."
    (when-let ((template (and (save-excursion (goto-char (point-min)) (string-match-p "\\`\\s-*$" (thing-at-point 'line)))
                              (cdr (assoc (file-name-base (directory-file-name (magit-toplevel)))
                                          +magit-project-commit-templates-alist)))))
      (goto-char (point-min))
      (insert (if (stringp template) template (funcall template)))
      (goto-char (point-min))
      (end-of-line)))
  (add-hook 'git-commit-setup-hook #'+magit-fill-in-commit-template 90))
;; Commit message templates:1 ends here

;; [[file:config.org::*Commit message templates][Commit message templates:2]]
(after! magit
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

  (add-to-list '+magit-project-commit-templates-alist (cons "org-mode" #'+org-commit-message-template)))
;; Commit message templates:2 ends here

;; [[file:config.org::*\\\[\\\]agit delta][\[\]agit delta:3]]
(after! magit
  (magit-delta-mode +1))
;; \[\]agit delta:3 ends here

;; [[file:config.org::*Smerge][Smerge:1]]
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
;; Smerge:1 ends here

;; [[file:config.org::*Company][Company:1]]
(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  ;;(add-hook 'evil-normal-state-entry-hook #'company-abort) ;; make aborting less annoying.
  )
;; Company:1 ends here

;; [[file:config.org::*Company][Company:3]]
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)
;; Company:3 ends here

;; [[file:config.org::*Plain Text][Plain Text:1]]
(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    ;;company-ispell
    company-files
    company-yasnippet))
;; Plain Text:1 ends here

;; [[file:config.org::*ESS][ESS:1]]
(set-company-backend! 'ess-r-mode '(company-R-args company-R-objects company-dabbrev-code :separate))
;; ESS:1 ends here

;; [[file:config.org::*Projectile][Projectile:1]]
(setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))
;; Projectile:1 ends here

;; [[file:config.org::*Configuration][Configuration:1]]
(setq ispell-dictionary "en-custom")
;; Configuration:1 ends here

;; [[file:config.org::*Configuration][Configuration:2]]
(setq ispell-personal-dictionary (expand-file-name ".ispell_personal" doom-user-dir))
;; Configuration:2 ends here

;; [[file:config.org::*Prompt recognition][Prompt recognition:1]]
(after! tramp
  (setenv "SHELL" "/bin/bash")
  (setq tramp-shell-prompt-pattern "\\(?:^\\|
\\)[^]#$%>\n]*#?[]#$%>] *\\(\\[[0-9;]*[a-zA-Z] *\\)*")) ;; default + 
;; Prompt recognition:1 ends here

;; [[file:config.org::*Guix][Guix:1]]
(after! tramp
  (appendq! tramp-remote-path
            '("~/.guix-profile/bin" "~/.guix-profile/sbin"
              "/run/current-system/profile/bin"
              "/run/current-system/profile/sbin")))
;; Guix:1 ends here

;; [[file:config.org::*Auto activating snippets][Auto activating snippets:2]]
(use-package! aas
  :commands aas-mode
 )
;; Auto activating snippets:2 ends here

;; [[file:config.org::*YASnippet][YASnippet:1]]
(setq yas-triggers-in-field t)
;; YASnippet:1 ends here

;; [[file:config.org::*String inflection][String inflection:2]]
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
  )
;; String inflection:2 ends here

;; [[file:config.org::*Smart parentheses][Smart parentheses:1]]
(sp-local-pair
 '(org-mode)
 "<<" ">>"
 :actions '(insert))
;; Smart parentheses:1 ends here

;; [[file:config.org::*Info colours][Info colours:2]]
(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)
;; Info colours:2 ends here

;; [[file:config.org::*Theme magic][Theme magic:3]]
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
;; Theme magic:3 ends here

;; [[file:config.org::*Theme magic][Theme magic:4]]
(run-with-idle-timer 0.1 nil (lambda () (add-hook 'doom-load-theme-hook 'theme-magic-from-emacs)))
;; Theme magic:4 ends here

;; [[file:config.org::*Emojify][Emojify:1]]
(setq emojify-emoji-set "twemoji-v2")
;; Emojify:1 ends here

;; [[file:config.org::*Emojify][Emojify:2]]
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
;; Emojify:2 ends here

;; [[file:config.org::*Keycast][Keycast:2]]
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
                  :height 1.1)))
;; Keycast:2 ends here

;; [[file:config.org::*Screencast][Screencast:2]]
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
;; Screencast:2 ends here

;; [[file:config.org::*Mixed pitch][Mixed pitch:1]]
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
;; Mixed pitch:1 ends here

;; [[file:config.org::*Mixed pitch][Mixed pitch:2]]
(autoload #'mixed-pitch-serif-mode "mixed-pitch"
  "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch." t)

(after! mixed-pitch
  (defface variable-pitch-serif
    '((t (:family "serif")))
    "A variable-pitch face with serifs."
    :group 'basic-faces)
  (setq mixed-pitch-set-height t)
  (setq variable-pitch-serif-font (font-spec :family "Linux Libertine" :size 23))
  (set-face-attribute 'variable-pitch-serif nil :font variable-pitch-serif-font)
  (defun mixed-pitch-serif-mode (&optional arg)
    "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch."
    (interactive)
    (let ((mixed-pitch-face 'variable-pitch-serif))
      (mixed-pitch-mode (or arg 'toggle)))))
;; Mixed pitch:2 ends here

;; [[file:config.org::*Mixed pitch][Mixed pitch:3]]
;; (set-char-table-range composition-function-table ?f '(["\\(?:ff?[fijlt]\\)" 0 font-shape-gstring]))
;; (set-char-table-range composition-function-table ?T '(["\\(?:Th\\)" 0 font-shape-gstring]))
;; Mixed pitch:3 ends here

;; [[file:config.org::*Marginalia][Marginalia:1]]
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
;; Marginalia:1 ends here

;; [[file:config.org::*Centaur Tabs][Centaur Tabs:1]]
(after! centaur-tabs
  (centaur-tabs-mode -1)
  (setq centaur-tabs-height 36
        centaur-tabs-set-icons t
        centaur-tabs-modified-marker "o"
        centaur-tabs-close-button "×"
        centaur-tabs-set-bar 'above
        centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-change-fonts "LXGW WenKai" 160)
  (global-set-key (kbd "C-<left>")  'centaur-tabs-backward)
  (global-set-key (kbd "C-<right>") 'centaur-tabs-forward)
  )
;; (setq x-underline-at-descent-line t)
;; Centaur Tabs:1 ends here

;; [[file:config.org::*All the icons][All the icons:1]]
(after! all-the-icons
  (setcdr (assoc "m" all-the-icons-extension-icon-alist)
          (cdr (assoc "matlab" all-the-icons-extension-icon-alist))))
;; All the icons:1 ends here

;; [[file:config.org::*Prettier page breaks][Prettier page breaks:2]]
(use-package! page-break-lines
  :commands page-break-lines-mode
  :init
  (autoload 'turn-on-page-break-lines-mode "page-break-lines")
  :config
  (setq page-break-lines-max-width fill-column)
  (map! :prefix "g"
        :desc "Prev page break" :nv "[" #'backward-page
        :desc "Next page break" :nv "]" #'forward-page))
;; Prettier page breaks:2 ends here

;; [[file:config.org::*Writeroom][Writeroom:1]]
(setq +zen-text-scale 0.8)
;; Writeroom:1 ends here

;; [[file:config.org::*Writeroom][Writeroom:2]]
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
            'org-modern-hide-stars
            )
  (add-hook 'writeroom-mode-enable-hook #'+zen-prose-org-h)
  (add-hook 'writeroom-mode-disable-hook #'+zen-nonprose-org-h)
  )
;; Writeroom:2 ends here

;; [[file:config.org::*Treemacs][Treemacs:1]]
(after! (treemacs dired)
  (defvar treemacs-file-ignore-extensions '()
    "File extension which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-globs '()
    "Globs which will are transformed to `treemacs-file-ignore-regexps' which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-regexps '()
    "RegExps to be tested to ignore files, generated from `treeemacs-file-ignore-globs'")
  (defun treemacs-file-ignore-generate-regexps ()

    (setq treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp treemacs-file-ignore-globs)))
  (if (equal treemacs-file-ignore-globs '()) nil (treemacs-file-ignore-generate-regexps))
  (defun treemacs-ignore-filter (file full-path)
    "Ignore files specified by `treemacs-file-ignore-extensions', and `treemacs-file-ignore-regexps'"
    (or (member (file-name-extension file) treemacs-file-ignore-extensions)
        (let ((ignore-file nil))
          (dolist (regexp treemacs-file-ignore-regexps ignore-file)
            (setq ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-filter))
;; Treemacs:1 ends here

;; [[file:config.org::*Treemacs][Treemacs:2]]
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
        ;; agda
        "agdai"
        ;; ocaml
        ))
(setq treemacs-file-ignore-globs
      '(;; LaTeX
        "*/_minted-*"
        ;; AucTeX
        "*/.auctex-auto"
        "*/_region_.log"
        "*/_region_.tex"))
;; Treemacs:2 ends here

;; [[file:config.org::*Treemacs][Treemacs:3]]
(after! (treemacs meow)
  ;; No EVIL, then I add this
  (define-key treemacs-mode-map (kbd "/") 'meow-visit)
)
;; Treemacs:3 ends here

;; [[file:config.org::*Treemacs][Treemacs:4]]
(use-package treemacs
  :config
  (setq
   treemacs-file-event-delay 1000
   treemacs-follow-after-init t
   treemacs-file-follow-delay 0.1
   treemacs-indentation 1
   treemacs-recenter-after-file-follow t
   treemacs-recenter-after-tag-follow t
   treemacs-text-scale -1.0
   treemacs-follow-mode t
   treemacs-width 25
   ;;treemacs-display-in-side-window nil
   )
  :bind
  (:map global-map
   ("C-x t 1"   . treemacs-delete-other-windows)
   ("C-x t t"   . treemacs)
   ("C-x t d"   . treemacs-select-directory)
   ("C-x t B"   . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag))
  )
;; Treemacs:4 ends here

;; [[file:config.org::*xkcd][xkcd:2]]
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
;; xkcd:2 ends here

;; [[file:config.org::*xkcd][xkcd:3]]
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
;; xkcd:3 ends here

;; [[file:config.org::*Selectric][Selectric:2]]
(use-package! selectic-mode
  :commands selectic-mode)
;; Selectric:2 ends here

;; [[file:config.org::*Spray][Spray:2]]
(use-package! spray
  :commands spray-mode
  :config
  (setq spray-wpm 600
        spray-height 800)
  (defun spray-mode-hide-cursor ()
    "Hide or unhide the cursor as is appropriate."
    ;;(if spray-mode
    ;; (setq-local spray--last-evil-cursor-state evil-normal-state-cursor
    ;;             evil-normal-state-cursor '(nil))
    ;;(setq-local evil-normal-state-cursor spray--last-evil-cursor-state))
    )
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
;; Spray:2 ends here

;; [[file:config.org::*Elcord][Elcord:2]]
(use-package! elcord
  :commands elcord-mode
  :config
  (setq elcord-use-major-mode-as-main-icon t))
;; Elcord:2 ends here

;; [[file:config.org::*Flycheck][Flycheck:1]]
(defun init-flycheck-hook ()
  (remove-hook 'post-command-hook 'flycheck-maybe-display-error-at-point-soon 'local)
  (remove-hook 'focus-in-hook 'flycheck-maybe-display-error-at-point-soon 'local)
  (remove-hook 'post-command-hook 'flycheck-display-error-at-point-soon 'local)
  (remove-hook 'focus-in-hook 'flycheck-display-error-at-point-soon 'local)
  )

(add-hook 'flycheck-mode-hook 'init-flycheck-hook)
;; Flycheck:1 ends here

;; [[file:config.org::*Systemd][Systemd:2]]
(use-package! systemd
  :defer t)
;; Systemd:2 ends here

;; [[file:config.org::*File Templates][File Templates:1]]
(set-file-template! "\\.tex$" :trigger "__" :mode 'latex-mode)
(set-file-template! "\\.org$" :trigger "__" :mode 'org-mode)
(set-file-template! "/LICEN[CS]E$" :trigger '+file-templates/insert-license)
;; File Templates:1 ends here

;; [[file:config.org::*LSP][LSP:2]]
(use-package lsp-mode
  :custom
  (lsp-enable-file-watchers nil)
  (lsp-ui-doc-position "Bottom")
  (lsp-keep-workspace-alive nil)
  (lsp-ui-doc-enable t)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-segments '(symbols))
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-semantic-tokens-enable t)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-show-with-cursor nil)
  )
;; LSP:2 ends here

;; [[file:config.org::*ffi-navigator][ffi-navigator:2]]
(use-package lsp
  :custom
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("python3" "-m" "ffi_navigator.langserver"))
    :major-modes '(python-mode c++-mode)
    :server-id 'ffi-navigator
    :add-on? t))
  )
;; ffi-navigator:2 ends here

;; [[file:config.org::*Agda][Agda:1]]
(setq auto-mode-alist
      (append
       '(
         ("\\.agda\\'" . agda2-mode)
         ("\\.lagda.md\\'" . agda2-mode)
         )
       auto-mode-alist))
;; (add-to-list 'auto-mode-alist '( ("\\.agda\\'" . agda2-mode)
;;                                  ("\\.lagda.md\\'" . agda2-mode)))
;; Agda:1 ends here

;; [[file:config.org::*Grammarly][Grammarly:2]]
;; (use-package lsp-grammarly
;;   :hook (text-mode . (lambda ()
;;                        (require 'lsp-grammarly)
;;                        (lsp)))
;; )  ; or lsp-deferred
;; Grammarly:2 ends here

;; [[file:config.org::*Meson][Meson:2]]
(add-hook 'meson-mode-hook 'company-mode)
(setq auto-mode-alist
      (append
       '(
         ("\\meson.buil\\'" . meson-mode)
         )
       auto-mode-alist))
;; Meson:2 ends here

;; [[file:config.org::*OCaml][OCaml:1]]
(add-hook 'before-save-hook 'ocamlformat-before-save)
;; OCaml:1 ends here

;; [[file:config.org::*Rust][Rust:1]]
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
;; Rust:1 ends here

;; [[file:config.org::*Plaintext][Plaintext:1]]
(after! text-mode
  (add-hook! 'text-mode-hook
             ;; Disable hl-line-mode for all modes inherited from text mode
             (hl-line-mode -1)
             (unless (derived-mode-p 'org-mode)
               ;; Apply ANSI color codes
               (with-silent-modifications
                 (ansi-color-apply-on-region (point-min) (point-max) t)))))
;; Plaintext:1 ends here

(after! org
  (setq org-directory "~/org"                       ; let's put files here
        org-journal-dir org-directory               ; let's keep things simple
        org-use-property-inheritance t              ; it's convenient to have properties inherited
        org-log-done 'time                          ; having the time a item is done sounds convenient
        org-list-allow-alphabetical t               ; have a. A. a) A) list bullets
        org-export-in-background t                  ; run export processes in external emacs process
        org-catch-invisible-edits 'smart            ; try not to accidently do weird stuff in invisible regions
        org-export-with-sub-superscripts '{}        ; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}
        org-image-actual-width '(0.9)
        org-footnote-auto-adjust t)                 ; let org orgnize footnotes autometely
  (setq org-babel-default-header-args
        '((:session . "none")
          (:results . "replace")
          (:exports . "code")
          (:cache . "no")
          (:noweb . "no")
          (:hlines . "no")
          (:tangle . "no")
          (:comments . "link")))
  ;;(remove-hook 'text-mode-hook #'visual-line-mode)
  ;;(add-hook 'text-mode-hook #'auto-fill-mode)
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
  ;;(setq cdlatex-takeover-subsuperscript nil)
  (defadvice! org-edit-latex-emv-after-insert ()
    :after #'org-cdlatex-environment-indent
    (org-edit-latex-environment))
  ;;(add-hook 'org-mode-hook 'turn-on-flyspell)
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
  (after! lsp-clangd
    (setq lsp-clients-clangd-args
          '("-j=12"
            "--background-index"
            "--clang-tidy"
            "--completion-style=detailed"
            "--header-insertion-decorators=0"))
    (set-lsp-priority! 'clangd 2))
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
  (after! org-agenda
    (org-super-agenda-mode))
  
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
                  ;;(setq-local evil-normal-state-cursor (list nil))
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
                    ("Personal journal" :keys "j"
                     :icon ("checklist" :set "octicon" :color "yellow")
                     :file +org-capture-journal-file
                     :type entry
                     :prepend t
                     :target (file+olp+datatree +org-capture-journal-file)
                     :type entry
                     :template ("* %U"
                                "%i %a"
                                "%?"
                                ))
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
                                 #'org-capture-reinitialise-hook)))))
    )
  (setf (alist-get 'height +org-capture-frame-parameters) 15)
  ;; (alist-get 'name +org-capture-frame-parameters) "❖ Capture") ;; ATM hardcoded in other places, so changing breaks stuff
  (setq +org-capture-fn
        (lambda ()
          (interactive)
          (set-window-parameter nil 'mode-line-format 'none)
          (org-capture)))
  (defadvice! doom-modeline--buffer-file-name-roam-aware-a (orig-fun)
    :around #'doom-modeline-buffer-file-name ; takes no args
    (if (s-contains-p org-roam-directory (or buffer-file-name ""))
        (replace-regexp-in-string
         "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
         "🢔(\\1-\\2-\\3) "
         (subst-char-in-string ?_ ?  buffer-file-name))
      (funcall orig-fun)))
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
  (setq org-id-method 'ts)
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
      (cond
       ((org-export-derived-backend-p backend 'html)
        (format "<img class='invertible' src='%s' title=\"%s\" alt='%s'>" img (subst-char-in-string ?\" ?" alt) title))
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
    :around #'org-superstar-mode
    (ignore-errors (apply orig-fn args)))
  (defconst flycheck-org-lint-form
    (flycheck-prepare-emacs-lisp-form
      (require 'org)
      (require 'org-attach)
      (let ((source (car command-line-args-left))
            (process-default-directory default-directory))
        (with-temp-buffer
          (insert-file-contents source 'visit)
          (setq buffer-file-name source)
          (setq default-directory process-default-directory)
          (delay-mode-hooks (org-mode))
          (setq delayed-mode-hooks nil)
          (dolist (err (org-lint))
            (let ((inf (cl-second err)))
              (princ (elt inf 0))
              (princ ": ")
              (princ (elt inf 2))
              (terpri)))))))
  
  (defconst flycheck-org-lint-variables
    '(org-directory
      org-id-locations
      org-id-locations-file
      org-attach-id-dir
      org-attach-use-inheritance
      org-attach-id-to-path-function-list
      org-link-parameters)
    "Variables inherited by the org-lint subprocess.")
  
  (defun flycheck-org-lint-variables-form ()
    (require 'org-attach)  ; Needed to make variables available
    `(progn
       ,@(seq-map (lambda (opt) `(setq-default ,opt ',(symbol-value opt)))
                  (seq-filter #'boundp flycheck-org-lint-variables))))
  
  (flycheck-define-checker org-lint
    "Org buffer checker using `org-lint'."
    :command ("emacs" (eval flycheck-emacs-args)
              "--eval" (eval (concat "(add-to-list 'load-path \""
                                     (file-name-directory (locate-library "org"))
                                     "\")"))
              "--eval" (eval (flycheck-sexp-to-string
                              (flycheck-org-lint-variables-form)))
              "--eval" (eval (flycheck-sexp-to-string
                              (flycheck-org-lint-customisations-form)))
              "--eval" (eval flycheck-org-lint-form)
              "--" source)
    :error-patterns
    ((error line-start line ": " (message) line-end))
    :modes org-mode)
  (add-to-list 'flycheck-checkers 'org-lint)
  (defun flycheck-org-lint-customisations-form ()
    `(progn
       (require 'ox)
       (cl-pushnew '(:latex-cover-page nil "coverpage" nil)
                   (org-export-backend-options (org-export-get-backend 'latex)))
       (cl-pushnew '(:latex-font-set nil "fontset" nil)
  
                   (org-export-backend-options (org-export-get-backend 'latex)))))
  (add-hook 'org-mode-hook #'+org-pretty-mode)
  (setq org-pretty-entities-include-sub-superscripts nil)
  ;; disable =solaire-mode= for org-mode
  (add-hook 'org-mode-hook (lambda () (solaire-mode -1)))
  (custom-set-faces!
    '(org-document-title :height 1.3)
    '(org-quote :font "Linux Libertine" :height 1.05)
    '(org-table :font "Sarasa Mono SC" :height 1.05)
    '(outline-1 :height 1.25)
    '(outline-2 :height 1.15)
    '(outline-3 :height 1.12)
    '(outline-4 :height 1.09)
    '(outline-5 :height 1.06)
    '(outline-6 :height 1.03)
    '(outline-8 :height 1.01)
    '(outline-9 )
    )
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
  (setq org-inline-src-prettify-results '("⟨" . "⟩"))
  (setq doom-themes-org-fontify-special-tags nil)
  (package! seperate-inline :recipe
   (:host github :repo "ingtshan/separate-inline.el" :files ("*.el")))
  (use-package seperate-inline
    :hook ((org-mode-hook . separate-inline-mode)
           (org-mode-hook . (lambda ()
                              (add-hook 'separate-inline-mode-hook
                                        'separate-inline-use-default-rules-for-org-local
                                        nil 'make-it-local))))
    )
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
            `(:list_property "∷"
              :em_dash       "-"
              :ellipses      "…"
              :arrow_right   "→"
              :arrow_left    "←"
              :arrow_lr      "↔"
              :properties    "⚙"
              :end           "∎"
              :priority_a   ,(propertize "⚑" 'face 'all-the-icons-red)
              :priority_b   ,(propertize "⬆" 'face 'all-the-icons-orange)
              :priority_c   ,(propertize "■" 'face 'all-the-icons-yellow)
              :priority_d   ,(propertize "⬇" 'face 'all-the-icons-green)
              :priority_e   ,(propertize "❓" 'face 'all-the-icons-blue)))
  
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
  (package! org-pretty-tags)
  (use-package org-pretty-tags
    :config
    (setq org-pretty-tags-surrogate-strings
          `(("uni"        . ,(all-the-icons-faicon   "graduation-cap" :face 'all-the-icons-purple  :v-adjust 0.01))
            ("ucc"        . ,(all-the-icons-material "computer"       :face 'all-the-icons-silver  :v-adjust 0.01))
            ("assignment" . ,(all-the-icons-material "library_books"  :face 'all-the-icons-orange  :v-adjust 0.01))
            ("test"       . ,(all-the-icons-material "timer"          :face 'all-the-icons-red     :v-adjust 0.01))
            ("lecture"    . ,(all-the-icons-fileicon "keynote"        :face 'all-the-icons-orange  :v-adjust 0.01))
            ("email"      . ,(all-the-icons-faicon   "envelope"       :face 'all-the-icons-blue    :v-adjust 0.01))
            ("read"       . ,(all-the-icons-octicon  "book"           :face 'all-the-icons-lblue   :v-adjust 0.01))
            ("article"    . ,(all-the-icons-octicon  "file-text"      :face 'all-the-icons-yellow  :v-adjust 0.01))
            ("web"        . ,(all-the-icons-faicon   "globe"          :face 'all-the-icons-green   :v-adjust 0.01))
            ("info"       . ,(all-the-icons-faicon   "info-circle"    :face 'all-the-icons-blue    :v-adjust 0.01))
            ("issue"      . ,(all-the-icons-faicon   "bug"            :face 'all-the-icons-red     :v-adjust 0.01))
            ("someday"    . ,(all-the-icons-faicon   "calendar-o"     :face 'all-the-icons-cyan    :v-adjust 0.01))
            ("idea"       . ,(all-the-icons-octicon  "light-bulb"     :face 'all-the-icons-yellow  :v-adjust 0.01))
  
     ("emacs"      . ,(all-the-icons-fileicon "emacs"          :face 'all-the-icons-lpurple :v-adjust 0.01))))
    (org-pretty-tags-global-mode))
  ;;(setq org-highlight-latex-and-related '(native latex entities))
  (require 'org-src)
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))
  (package! org-fragtog)
  (add-hook 'org-mode-hook 'org-fragtog-mode)
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-preview-latex-process-alist
        '((dvipng :programs
                  ("latex" "dvipng")
                  :description "dvi > png"
                  :message "you need to install the programs: latex and dvipng."
                  :image-input-type "dvi"
                  :image-output-type "png"
                  :image-size-adjust
                  (0.35 . 0.35)
                  :latex-compiler
                  ("latex -interaction nonstopmode -output-directory %o %f")
                  :image-converter
                  ("dvipng -D %D -T tight -o %O %f")
                  :transparent-image-converter
                  ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
          (dvisvgm :programs
                   ("latex" "dvisvgm")
                   :description "dvi > svg"
                   :message "you need to install the programs: latex and dvisvgm."
                   :image-input-type "dvi"
                   :image-output-type "svg"
                   :image-size-adjust
                   (0.8 . 0.8)
                   :latex-compiler
                   ("latex -interaction nonstopmode -output-format=dvi -output-directory %o %f")
                   :image-converter
                   ("dvisvgm %f -n -b min -c %S -o %O"))
          (imagemagick :programs
                       ("latex" "convert")
                       :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                       (1.0 . 1.0)
                       :latex-compiler
                       ("pdflatex -interaction nonstopmode -output-directory %o %f")
                       :image-converter
                       ("convert -density %D -trim -antialias %f -quality 100 %O")))
        )
  (setq org-format-latex-header "\\documentclass[utf8]{article}
  \\usepackage[usenames]{xcolor}
  \\usepackage[T1]{fontenc}
  
  % Chinese support, don't add this when not necessary
  %\\usepackage{ctex}
  
  \\usepackage{booktabs}
  \\pagestyle{empty}             % do not remove
  % The settings below are copied from fullpage.sty
  \\setlength{\\textwidth}{\\paperwidth}
  \\addtolength{\\textwidth}{-3cm}
  \\setlength{\\oddsidemargin}{1.5cm}
  \\addtolength{\\oddsidemargin}{-2.54cm}
  \\setlength{\\evensidemargin}{\\oddsidemargin}
  \\setlength{\\textheight}{\\paperheight}
  \\addtolength{\\textheight}{-\\headheight}
  \\addtolength{\\textheight}{-\\headsep}
  \\addtolength{\\textheight}{-\\footskip}
  \\addtolength{\\textheight}{-3cm}
  \\setlength{\\topmargin}{1.5cm}
  \\addtolength{\\topmargin}{-2.54cm}
  \\usepackage{amsmath}
  \\usepackage{amssymb}
  ")
  ;; (setq org-format-latex-options
  ;;      (plist-put org-format-latex-options :background "Transparent"))
  ;; (with-eval-after-load 'font-latex
  ;;   (set-face-attribute 'font-latex-math-face nil :background (face-attribute 'default :background))
  ;;   )
  ;; (defun set-latex-background-same-with-default ()
  ;;   "Set inline latex color correctly"
  ;;   (interactive)
  ;;   (set-face-attribute 'font-latex-sedate-face nil :inherit 'fixed-pitch)
  ;;   (set-face-attribute 'font-latex-math-face nil :background (face-attribute 'default :background))
  ;;   ;(set-face-attribute 'org-block nil :background (face-attribute 'default :background))
  ;;   )
  (defun scimax-org-latex-fragment-justify (justification)
    "Justify the latex fragment at point with JUSTIFICATION.
  JUSTIFICATION is a symbol for 'left, 'center or 'right."
    (interactive
     (list (intern-soft
            (completing-read "Justification (left): " '(left center right)
                             nil t nil nil 'left))))
    (let* ((ov (ov-at))
           (beg (ov-beg ov))
           (end (ov-end ov))
           (shift (- beg (line-beginning-position)))
           (img (overlay-get ov 'display))
           (img (and (and img (consp img) (eq (car img) 'image)
                          (image-type-available-p (plist-get (cdr img) :type)))
                     img))
           space-left offset)
      (when (and img
                 ;; This means the equation is at the start of the line
                 (= beg (line-beginning-position))
                 (or
                  (string= "" (s-trim (buffer-substring end (line-end-position))))
                  (eq 'latex-environment (car (org-element-context)))))
        (setq space-left (- (window-max-chars-per-line) (car (image-size img)))
              offset (floor (cond
                             ((eq justification 'center)
                              (- (/ space-left 2) shift))
                             ((eq justification 'right)
                              (- space-left shift))
                             (t
                              0))))
        (when (>= offset 0)
          (overlay-put ov 'before-string (make-string offset ?\ ))))))
  
  (defun scimax-org-latex-fragment-justify-advice (beg end image imagetype)
    "After advice function to justify fragments."
    (scimax-org-latex-fragment-justify (or (plist-get org-format-latex-options :justify) 'left)))
  
  
  (defun scimax-toggle-latex-fragment-justification ()
    "Toggle if LaTeX fragment justification options can be used."
    (interactive)
    (if (not (get 'scimax-org-latex-fragment-justify-advice 'enabled))
        (progn
          (advice-add 'org--format-latex-make-overlay :after 'scimax-org-latex-fragment-justify-advice)
          (put 'scimax-org-latex-fragment-justify-advice 'enabled t)
          (message "Latex fragment justification enabled"))
      (advice-remove 'org--format-latex-make-overlay 'scimax-org-latex-fragment-justify-advice)
      (put 'scimax-org-latex-fragment-justify-advice 'enabled nil)
      (message "Latex fragment justification disabled")))
  ;; Numbered equations all have (1) as the number for fragments with vanilla
  ;; org-mode. This code injects the correct numbers into the previews so they
  ;; look good.
  (defun scimax-org-renumber-environment (orig-func &rest args)
    "A function to inject numbers in LaTeX fragment previews."
    (let ((results '())
          (counter -1)
          (numberp))
      (setq results (cl-loop for (begin . env) in
                             (org-element-map (org-element-parse-buffer) 'latex-environment
                               (lambda (env)
                                 (cons
                                  (org-element-property :begin env)
                                  (org-element-property :value env))))
                             collect
                             (cond
                              ((and (string-match "\\\\begin{equation}" env)
                                    (not (string-match "\\\\tag{" env)))
                               (cl-incf counter)
                               (cons begin counter))
                              ((string-match "\\\\begin{align}" env)
                               (prog2
                                   (cl-incf counter)
                                   (cons begin counter)
                                 (with-temp-buffer
                                   (insert env)
                                   (goto-char (point-min))
                                   ;; \\ is used for a new line. Each one leads to a number
                                   (cl-incf counter (count-matches "\\\\$"))
                                   ;; unless there are nonumbers.
                                   (goto-char (point-min))
                                   (cl-decf counter (count-matches "\\nonumber")))))
                              (t
                               (cons begin nil)))))
  
      (when (setq numberp (cdr (assoc (point) results)))
        (setf (car args)
              (concat
               (format "\\setcounter{equation}{%s}\n" numberp)
               (car args)))))
  
    (apply orig-func args))
  
  
  (defun scimax-toggle-latex-equation-numbering ()
    "Toggle whether LaTeX fragments are numbered."
    (interactive)
    (if (not (get 'scimax-org-renumber-environment 'enabled))
        (progn
          (advice-add 'org-create-formula-image :around #'scimax-org-renumber-environment)
          (put 'scimax-org-renumber-environment 'enabled t)
          (message "Latex numbering enabled"))
      (advice-remove 'org-create-formula-image #'scimax-org-renumber-environment)
      (put 'scimax-org-renumber-environment 'enabled nil)
      (message "Latex numbering disabled.")))
  
  (advice-add 'org-create-formula-image :around #'scimax-org-renumber-environment)
  (put 'scimax-org-renumber-environment 'enabled t)
  (after! org-plot
    (defvar +org-plot-term-size '(1050 . 650)
      "The size of the GNUPlot terminal, in the form (WIDTH . HEIGHT).")
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
              (doom-color 'violet)))
  
    (defun +org-plot-gnuplot-term-properties (_type)
      (format "background rgb '%s' size %s,%s"
              (doom-color 'bg) (car +org-plot-term-size) (cdr +org-plot-term-size)))
  
    (setq org-plot/gnuplot-script-preamble #'+org-plot-generate-theme)
    (setq org-plot/gnuplot-term-extra #'+org-plot-gnuplot-term-properties))(doom-color 'teal)
  (setq org-export-headline-levels 5) ; I like nesting
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  (setq org-export-creator-string
        (format "Emacs %s (Org mode %s-%s)" emacs-version (org-release) (org-git-version)))
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
  (after! ox-html
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
         ;;  contents.
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
  )
  ;; org-latex-compilers = ("pdflatex" "xelatex" "lualatex"), which are the possible values for %latex
  (setq org-latex-pdf-process '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))
  (setq org-latex-compiler "xelatex")
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
           (hanging-secnum-preamble "
  \\renewcommand\\sectionformat{\\llap{\\thesection\\autodot\\enskip}}
  \\renewcommand\\subsectionformat{\\llap{\\thesubsection\\autodot\\enskip}}
  \\renewcommand\\subsubsectionformat{\\llap{\\thesubsubsection\\autodot\\enskip}}
  ")
           (big-chap-preamble "
  \\RedeclareSectionCommand[afterindent=false, beforeskip=0pt, afterskip=0pt, innerskip=0pt]{chapter}
  \\setkomafont{chapter}{\\normalfont\\Huge}
  \\renewcommand*{\\chapterheadstartvskip}{\\vspace*{0\\baselineskip}}
  \\renewcommand*{\\chapterheadendvskip}{\\vspace*{0\\baselineskip}}
  \\renewcommand*{\\chapterformat}{%
    \\fontsize{60}{30}\\selectfont\\rlap{\\hspace{6pt}\\thechapter}}
  \\renewcommand*\\chapterlinesformat[3]{%
    \\parbox[b]{\\dimexpr\\textwidth-0.5em\\relax}{%
      \\raggedleft{{\\large\\scshape\\bfseries\\chapapp}\\vspace{-0.5ex}\\par\\Huge#3}}%
      \\hfill\\makebox[0pt][l]{#2}}
  "))
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
      ;; (add-to-list 'org-latex-classes
      ;;              `("bmc-article" "\\documentclass[article,code,maths]{bmc}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
      ;;                ,@article-sections))
      ;; (add-to-list 'org-latex-classes
      ;;              `("bmc" "\\documentclass[code,maths]{bmc}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
      ;;                ,@book-sections))
      ))
  
  (setq org-latex-tables-booktabs t
        org-latex-hyperref-template "
  \\providecolor{url}{HTML}{0077bb}
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
    citecolor=cite\n}
  \\urlstyle{same}
  "
        org-latex-reference-command "\\cref{%s}")
  (defvar org-latex-embed-files-preamble "
  %\\usepackage[main,include]{embedall}
  %\\IfFileExists{./\\jobname.org}{\\embedfile[desc=The original file]{\\jobname.org}}{}
  "
    "Preamble that embeds files within the pdf.")
  
  (defvar org-latex-caption-preamble "
  \\usepackage{subcaption}
  \\usepackage[hypcap=true]{caption}
  \\setkomafont{caption}{\\sffamily\\small}
  \\setkomafont{captionlabel}{\\upshape\\bfseries}
  \\captionsetup{justification=raggedright,singlelinecheck=true}
  \\usepackage{capt-of} % required by Org
  "
    "Preamble that improves captions.")
  
  (defvar org-latex-checkbox-preamble "
  \\newcommand{\\checkboxUnchecked}{$\\square$}
  \\newcommand{\\checkboxTransitive}{\\rlap{\\raisebox{-0.1ex}{\\hspace{0.35ex}\\Large\\textbf -}}$\\square$}
  \\newcommand{\\checkboxChecked}{\\rlap{\\raisebox{0.2ex}{\\hspace{0.35ex}\\scriptsize \\ding{52}}}$\\square$}
  "
    "Preamble that improves checkboxes.")
  
  (defvar org-latex-box-preamble "
  \\ExplSyntaxOn
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
  \\ExplSyntaxOff
  "
    "Preamble that provides a macro for custom boxes.")
  (defun org-latex-embed-extra-files ()
    "Return a string that uses embedfile to embed all tangled files."
    (mapconcat
     (lambda (file-desc)
       (format "\\IfFileExists{%1$s}{\\embedfile[desc=%2$s]{%1$s}}{}"
               (thread-last (car file-desc)
                 (replace-regexp-in-string "\\\\" "\\\\\\\\")
                 (replace-regexp-in-string "~" "\\\\string~"))
               (cdr file-desc)))
     (append
      (mapcar (lambda (f-block)
                (let ((file-lang (cons (or (car f-block) (caddr (cadr f-block))) (caadr f-block))))
                  (cons (car file-lang) (format "Tangled %s file" (cdr file-lang)))))
              (org-babel-tangle-collect-blocks)) ; all files being tangled to
      (let (extra-files)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*#\\+embed:" nil t)
            (let* ((file-desc (split-string (org-element-property :value (org-element-at-point)) " :desc\\(?:ription\\)? ")))
              (push (cons (car file-desc) (or (cdr file-desc) "Extra file")) extra-files))))
        (nreverse extra-files)))
     "\n"))
  (defvar org-latex-embed-files t
    "Embed the source .org, .tex, and any tangled files.")
  (defvar org-latex-use-microtype t
    "Use the microtype pakage.")
  (defvar org-latex-italic-quotes t
    "Make \"quote\" environments italic.")
  (defvar org-latex-par-sep t
    "Vertically seperate paragraphs, and remove indentation.")
  
  (defvar org-latex-conditional-features
    '(("\\[\\[\\(?:file\\|https?\\):\\(?:[^]]\\|\\\\\\]\\)+?\\.\\(?:eps\\|pdf\\|png\\|jpeg\\|jpg\\|jbig2\\)\\]\\]\\|\\\\includegraphics[\\[{]" . image)
      ("\\[\\[\\(?:file\\|https?\\):\\(?:[^]]+?\\|\\\\\\]\\)\\.svg\\]\\]\\|\\\\includesvg[\\[{]" . svg)
      ("\\\\(\\|\\\\\\[\\|\\\\begin{\\(?:math\\|displaymath\\|equation\\|align\\|flalign\\|multiline\\|gather\\)[a-z]*\\*?}" . maths)
      ("^[ \t]*|" . table)
      ("cref:\\|\\cref{\\|\\[\\[[^\\]+\n?[^\\]\\]\\]" . cleveref)
      ("[;\\\\]?\\b[A-Z][A-Z]+s?[^A-Za-z]" . acronym)
      ("\\+[^ ].*[^ ]\\+\\|_[^ ].*[^ ]_\\|\\\\uu?line\\|\\\\uwave\\|\\\\sout\\|\\\\xout\\|\\\\dashuline\\|\\dotuline\\|\\markoverwith" . underline)
      (":float wrap" . float-wrap)
      (":float sideways" . rotate)
      ("^[ \t]*#\\+caption:\\|\\\\caption" . caption)
      ("\\[\\[xkcd:" . (image caption))
      (org-latex-use-microtype . microtype)
      ((and org-latex-italic-quotes "^[ \t]*#\\+begin_quote\\|\\\\begin{quote}") . italic-quotes)
      (org-latex-par-sep . par-sep)
      (org-latex-embed-files . embed-files)
      ((and org-latex-embed-files "^[ \t]*#\\+embed\\|^[ \t]*#\\+begin_src\\|^[ \t]*#\\+BEGIN_SRC") . embed-tangled)
      ("^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\|[A-Za-z]+[.)]\\) \\[[ -X]\\]" . checkbox)
      ("^[ \t]*#\\+begin_warning\\|\\\\begin{warning}" . box-warning)
      ("^[ \t]*#\\+begin_info\\|\\\\begin{info}"       . box-info)
      ("^[ \t]*#\\+begin_notes\\|\\\\begin{notes}"     . box-notes)
      ("^[ \t]*#\\+begin_success\\|\\\\begin{success}" . box-success)
      ("^[ \t]*#\\+begin_error\\|\\\\begin{error}"     . box-error))
    "Org feature tests and associated LaTeX feature flags.
  
  Alist where the car is a test for the presense of the feature,
  and the cdr is either a single feature symbol or list of feature symbols.
  
  When a string, it is used as a regex search in the buffer.
  The feature is registered as present when there is a match.
  
  The car can also be a
  - symbol, the value of which is fetched
  - function, which is called with info as an argument
  - list, which is `eval'uated
  
  If the symbol, function, or list produces a string: that is used as a regex
  search in the buffer. Otherwise any non-nil return value will indicate the
  existance of the feature.")
  (defvar org-latex-feature-implementations
    '((image         :snippet "\\usepackage{graphicx}" :order 2)
      (svg           :snippet "\\usepackage[inkscapelatex=false]{svg}" :order 2)
      ;; replace bmc with amsmath here
      (maths         :snippet "\\usepackage{amsmath}" :order 0.2)
      (table         :snippet "\\usepackage{longtable}\n\\usepackage{booktabs}" :order 2)
      (cleveref      :snippet "\\usepackage[capitalize]{cleveref}" :order 1) ; after bmc-maths
      (underline     :snippet "\\usepackage[normalem]{ulem}" :order 0.5)
      (float-wrap    :snippet "\\usepackage{wrapfig}" :order 2)
      (rotate        :snippet "\\usepackage{rotating}" :order 2)
      (caption       :snippet org-latex-caption-preamble :order 2.1)
      (microtype     :snippet "\\usepackage[activate={true,nocompatibility},final,tracking=true,kerning=true,spacing=true,factor=2000]{microtype}\n" :order 0.1)
      (embed-files   :snippet org-latex-embed-files-preamble :order -2)
      (embed-tangled :requires embed-files :snippet (concat (org-latex-embed-extra-files) "\n") :order -1)
      (acronym       :snippet "\\newcommand{\\acr}[1]{\\protect\\textls*[110]{\\scshape #1}}\n\\newcommand{\\acrs}{\\protect\\scalebox{.91}[.84]{\\hspace{0.15ex}s}}" :order 0.4)
      (italic-quotes :snippet "\\renewcommand{\\quote}{\\list{}{\\rightmargin\\leftmargin}\\item\\relax\\em}\n" :order 0.5)
      (par-sep       :snippet "\\setlength{\\parskip}{\\baselineskip}\n\\setlength{\\parindent}{0pt}\n" :order 0.5)
      (.pifont       :snippet "\\usepackage{pifont}")
      (.xcoffins     :snippet "\\usepackage{xcoffins}")
      (checkbox      :requires .pifont :order 3
                     :snippet (concat (unless (memq 'maths features)
                                        "\\usepackage{amssymb} % provides \\square")
                                      org-latex-checkbox-preamble))
      (.fancy-box    :requires (.pifont .xcoffins) :snippet org-latex-box-preamble :order 3.9)
      (box-warning   :requires .fancy-box :snippet "\\defsimplebox{warning}{e66100}{Warning}" :order 4)
      (box-info      :requires .fancy-box :snippet "\\defsimplebox{info}{3584e4}{Information}" :order 4)
      (box-notes     :requires .fancy-box :snippet "\\defsimplebox{notes}{26a269}{Notes}" :order 4)
      (box-success   :requires .fancy-box :snippet "\\defsimplebox{success}{26a269}{\\vspace{-\\baselineskip}}" :order 4)
      (box-error     :requires .fancy-box :snippet "\\defsimplebox{error}{c01c28}{Important}" :order 4))
    "LaTeX features and details required to implement them.
  
  List where the car is the feature symbol, and the rest forms a plist with the
  following keys:
  - :snippet, which may be either
    - a string which should be included in the preamble
    - a symbol, the value of which is included in the preamble
    - a function, which is evaluated with the list of feature flags as its
      single argument. The result of which is included in the preamble
    - a list, which is passed to `eval', with a list of feature flags available
      as \"features\"
  
  - :requires, a feature or list of features that must be available
  - :when, a feature or list of features that when all available should cause this
      to be automatically enabled.
  - :prevents, a feature or list of features that should be masked
  - :order, for when ordering is important. Lower values appear first.
      The default is 0.
  - :eager, when non-nil the feature will be eagerly loaded, i.e. without being detected.")
  (defun org-latex-detect-features (&optional buffer info)
    "List features from `org-latex-conditional-features' detected in BUFFER."
    (let ((case-fold-search nil))
      (with-current-buffer (or buffer (current-buffer))
        (delete-dups
         (mapcan (lambda (construct-feature)
                   (when (let ((out (pcase (car construct-feature)
                                      ((pred stringp) (car construct-feature))
                                      ((pred functionp) (funcall (car construct-feature) info))
                                      ((pred listp) (eval (car construct-feature)))
                                      ((pred symbolp) (symbol-value (car construct-feature)))
                                      (_ (user-error "org-latex-conditional-features key %s unable to be used" (car construct-feature))))))
                           (if (stringp out)
                               (save-excursion
                                 (goto-char (point-min))
                                 (re-search-forward out nil t))
                             out))
                     (if (listp (cdr construct-feature)) (cdr construct-feature) (list (cdr construct-feature)))))
                 org-latex-conditional-features)))))
  (defun org-latex-expand-features (features)
    "For each feature in FEATURES process :requires, :when, and :prevents keywords and sort according to :order."
    (dolist (feature features)
      (unless (assoc feature org-latex-feature-implementations)
        (message "Feature %s not provided in org-latex-feature-implementations, ignoring." feature)
        (setq features (remove feature features))))
    (setq current features)
    (while current
      (when-let ((requirements (plist-get (cdr (assq (car current) org-latex-feature-implementations)) :requires)))
        (setcdr current (if (listp requirements)
                            (append requirements (cdr current))
                          (cons requirements (cdr current)))))
      (setq current (cdr current)))
    (dolist (potential-feature
             (append features (delq nil (mapcar (lambda (feat)
                                                  (when (plist-get (cdr feat) :eager)
                                                    (car feat)))
                                                org-latex-feature-implementations))))
      (when-let ((prerequisites (plist-get (cdr (assoc potential-feature org-latex-feature-implementations)) :when)))
        (setf features (if (if (listp prerequisites)
                               (cl-every (lambda (preq) (memq preq features)) prerequisites)
                             (memq prerequisites features))
                           (append (list potential-feature) features)
                         (delq potential-feature features)))))
    (dolist (feature features)
      (when-let ((prevents (plist-get (cdr (assoc feature org-latex-feature-implementations)) :prevents)))
        (setf features (cl-set-difference features (if (listp prevents) prevents (list prevents))))))
    (sort (delete-dups features)
          (lambda (feat1 feat2)
            (if (< (or (plist-get (cdr (assoc feat1 org-latex-feature-implementations)) :order) 1)
                   (or (plist-get (cdr (assoc feat2 org-latex-feature-implementations)) :order) 1))
                t nil))))
  (defun org-latex-generate-features-preamble (features)
    "Generate the LaTeX preamble content required to provide FEATURES.
  This is done according to `org-latex-feature-implementations'"
    (let ((expanded-features (org-latex-expand-features features)))
      (concat
       (format "\n%% features: %s\n" expanded-features)
       (mapconcat (lambda (feature)
                    (when-let ((snippet (plist-get (cdr (assoc feature org-latex-feature-implementations)) :snippet)))
                      (concat
                       (pcase snippet
                         ((pred stringp) snippet)
                         ((pred functionp) (funcall snippet features))
                         ((pred listp) (eval `(let ((features ',features)) (,@snippet))))
                         ((pred symbolp) (symbol-value snippet))
                         (_ (user-error "org-latex-feature-implementations :snippet value %s unable to be used" snippet)))
                       "\n")))
                  expanded-features
                  "")
       "% end features\n")))
  (defvar info--tmp nil)
  
  (defadvice! org-latex-save-info (info &optional t_ s_)
    :before #'org-latex-make-preamble
    (setq info--tmp info))
  
  (defadvice! org-splice-latex-header-and-generated-preamble-a (orig-fn tpl def-pkg pkg snippets-p &optional extra)
    "Dynamically insert preamble content based on `org-latex-conditional-preambles'."
    :around #'org-splice-latex-header
    (let ((header (funcall orig-fn tpl def-pkg pkg snippets-p extra)))
      (if snippets-p header
        (concat header
                (org-latex-generate-features-preamble (org-latex-detect-features nil info--tmp))
                "\n"))))
  (setq org-latex-default-packages-alist
        '(("AUTO" "inputenc" t ("pdflatex"))
          ("T1" "fontenc" t ("pdflatex"))
          ("" "xcolor" nil) ; Generally useful
          ("" "hyperref" nil)))
  (setq org-export-async-debug t)
  (defvar org-latex-default-fontset 'noto
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
  (add-to-list 'org-latex-conditional-features '(org-latex-default-fontset . custom-font) t)
  (add-to-list 'org-latex-feature-implementations '(custom-font :snippet (org-latex-fontset :serif :sans :mono) :order 0) t)
  (add-to-list 'org-latex-feature-implementations '(.custom-maths-font :eager t :when (custom-font maths) :snippet (org-latex-fontset :maths) :order 0.3) t)
  (defvar org-latex-fontsets
    '((cm nil) ; computer modern
      (## nil) ; no font set
      (cmu-typewriter-text
       :serif "\\usepackage[osf]{Alegreya}"
       :sans "\\usepackage{AlegreyaSans}"
       :mono "\\usepackage[scale=0.88]{sourcecodepro}"
       :maths "\\usepackage[varbb]{newpxmath}") ;FIXME
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
  (add-to-list 'org-latex-conditional-features '((string= (car (org-latex-fontset-entry)) "alegreya") . alegreya-typeface))
  (add-to-list 'org-latex-feature-implementations '(alegreya-typeface) t)
  (add-to-list 'org-latex-feature-implementations'(.alegreya-tabular-figures :eager t :when (alegreya-typeface table) :order 0.5 :snippet "
  \\makeatletter
  % tabular lining figures in tables
  \\renewcommand{\\tabular}{\\AlegreyaTLF\\let\\@halignto\\@empty\\@tabular}
  \\makeatother\n") t)
  (add-to-list 'org-latex-conditional-features '("LaTeX" . latex-symbol))
  (add-to-list 'org-latex-feature-implementations '(latex-symbol :when alegreya-typeface :order 0.5 :snippet "
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
  \\makeatother\n") t)
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
  (defvar org-latex-cover-page-maketitle "
  \\usepackage{tikz}
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
    \\let\\oldtoday\\today
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
  \\makeatother
  "
    "LaTeX snippet for the preamble that sets \\maketitle to produce a cover page.")
  
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
  
  (add-to-list 'org-latex-feature-implementations '(cover-page :snippet org-latex-cover-page-maketitle :order 9) t)
  (add-to-list 'org-latex-conditional-features '((org-latex-cover-page-p) . cover-page) t)
  (defvar org-latex-condense-lists t
    "Reduce the space between list items.")
  (defvar org-latex-condensed-lists "
  \\newcommand{\\setuplistspacing}{\\setlength{\\itemsep}{-0.5ex}\\setlength{\\parskip}{1.5ex}\\setlength{\\parsep}{0pt}}
  \\let\\olditem\\itemize\\renewcommand{\\itemize}{\\olditem\\setuplistspacing}
  \\let\\oldenum\\enumerate\\renewcommand{\\enumerate}{\\oldenum\\setuplistspacing}
  \\let\\olddesc\\description\\renewcommand{\\description}{\\olddesc\\setuplistspacing}
  ")
  
  (add-to-list 'org-latex-conditional-features '((and org-latex-condense-lists "^[ \t]*[-+]\\|^[ \t]*[1Aa][.)] ") . condensed-lists) t)
  (add-to-list 'org-latex-feature-implementations '(condensed-lists :snippet org-latex-condensed-lists :order 0.7) t)
  (setq org-latex-listings 'engraved) ; NOTE non-standard value
  (defvar-local org-export-has-code-p nil)
  
  (defadvice! org-export-expect-no-code (&rest _)
    :before #'org-export-as
    (setq org-export-has-code-p nil))
  
  (defadvice! org-export-register-code (&rest _)
    :after #'org-latex-src-block
    :after #'org-latex-inline-src-block-engraved
    (setq org-export-has-code-p t))
  
  (setq org-latex-engraved-code-preamble "
  \\usepackage{fvextra}
  \\fvset{
    commandchars=\\\\\\{\\},
    highlightcolor=white!95!black!80!blue,
    breaklines=true,
    breaksymbol=\\color{white!60!black}\\tiny\\ensuremath{\\hookrightarrow}}
  \\renewcommand\\theFancyVerbLine{\\footnotesize\\color{black!40!white}\\arabic{FancyVerbLine}}
  
  \\providecolor{codebackground}{HTML}{f7f7f7}
  \\providecolor{codeborder}{HTML}{f0f0f0}
  \\providecolor{EFD}{HTML}{28292e}
  
  % TODO have code boxes keep line vertical alignment
  \\usepackage[breakable,xparse]{tcolorbox}
  \\DeclareTColorBox[]{Code}{o}%
  {colback=codebackground, colframe=codeborder,
    fontupper=\\footnotesize\\setlength{\\fboxsep}{0pt},
    colupper=EFD,
    IfNoValueTF={#1}%
    {boxsep=2pt, arc=2.5pt, outer arc=2.5pt,
      boxrule=0.5pt, left=2pt}%
    {boxsep=2.5pt, arc=0pt, outer arc=0pt,
      boxrule=0pt, leftrule=1.5pt, left=0.5pt},
    right=2pt, top=1pt, bottom=0.5pt,
    breakable}
  ")
  
  (add-to-list 'org-latex-feature-implementations
               '(.no-protrusion-in-code :snippet "\\ifcsname Code\\endcsname\n  \\let\\oldcode\\Code\\renewcommand{\\Code}{\\microtypesetup{protrusion=false}\\oldcode}\n\\fi"
                                        :when microtype
                                        :when (microtype engraved-code-setup)
                                        :eager t
                                        :order 98.5) t)
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
    (when (and org-export-has-code-p
               (memq 'julia-code (org-latex-detect-features))
               (save-excursion
                 (goto-char (point-min))
                 (re-search-forward "[^\x00-\x7F\u200b]" nil t)))
      (setf info (plist-put
                  (if (member #'+org-latex-replace-non-ascii-chars (plist-get info :filter-final-output))
                      (plist-put info :filter-final-output
                                 (delq #'+org-latex-replace-non-ascii-chars (plist-get info :filter-final-output)))
                    info)
                  :latex-compiler "lualatex"))))
  (setq org-latex-julia-mono-fontspec "
  \\usepackage{fontspec}
  \\newfontfamily\\JuliaMono{JuliaMono-Regular.ttf}[Path=/usr/share/fonts/truetype/, Extension=.ttf]
  \\newfontface\\JuliaMonoRegular{JuliaMono-Regular}
  \\setmonofont{JuliaMonoRegular}[Contextuals=Alternate, Scale=MatchLowercase]
  ")
  
  (add-to-list 'org-latex-feature-implementations '(julia-code :snippet org-latex-julia-mono-fontspec :order 0) t)
  (add-to-list 'org-latex-conditional-features '((and org-export-has-code-p "^[ \t]*#\\+begin_src julia\\|^[ \t]*#\\+BEGIN_SRC julia\\|src_julia") . julia-code) t)
  
  (add-to-list 'org-latex-feature-implementations '(.microtype-lualatex :eager t :when (microtype julia-code) :prevents microtype :order 0.1 :snippet "\\usepackage[activate={true,nocompatibility},final,tracking=true,factor=2000]{microtype}\n"))
  (add-to-list 'org-latex-feature-implementations '(.custom-font-no-mono :eager t :prevents custom-font :order 0 :snippet (org-latex-fontset :serif :sans)) t)
  ;; (defun emojify-emoji-in-buffer-p ()
  ;;   "Determine if any emojis are present in the current buffer, using `emojify-mode'."
  ;;   (unless emojify-mode
  ;;     (emojify-mode 1)
  ;;     (emojify-display-emojis-in-region (point-min) (point-max)))
  ;;   (let (emoji-found end)
  ;;     (save-excursion
  ;;       (goto-char (point-min))
  ;;       (while (not (or emoji-found end))
  ;;         (if-let ((pos (re-search-forward "[^[:ascii:]]" nil t)))
  ;;             (when (get-text-property (1- pos) 'emojified)
  ;;               (setq emoji-found t))
  ;;           (setq end t))))
  ;;     emoji-found))
  ;; (defun org-latex-emoji-setup ()
  ;;   (format "\\newcommand\\emoji[1]{\\raisebox{-0.3ex}{\\includegraphics[height=1.8ex]{%s/#1}}}" (emojify-image-dir)))
  
  ;; (add-to-list 'org-latex-conditional-features '((emojify-emoji-in-buffer-p) . emoji) t)
  ;; (add-to-list 'org-latex-feature-implementations '(emoji :requires image :snippet (org-latex-emoji-setup) :order 3 ))
  ;; (defun emojify-latexify-emoji-in-buffer ()
  ;;   (unless emojify-mode
  ;;     (emojify-mode 1)
  ;;     (emojify-display-emojis-in-region (point-min) (point-max)))
  ;;   (let (end)
  ;;     (save-excursion
  ;;       (goto-char (point-min))
  ;;       (while (not end)
  ;;         (if-let ((pos (re-search-forward "[^[:ascii:]]\\{1,2\\}" nil t)))
  ;;             (when-let ((char (get-text-property (1- pos) 'emojify-text))
  ;;                        (emoji (emojify-get-emoji char)))
  ;;               (replace-match (format "\\\\emoji{%s}" (file-name-sans-extension (ht-get emoji "image")))))
  ;;           (setq end t))))))
  ;; (defun +org-latex-convert-emojis (text backend _info)
  ;;   (when (org-export-derived-backend-p backend 'latex)
  ;;     (with-temp-buffer
  ;;       (insert text)
  ;;       (when (emojify-emoji-in-buffer-p)
  ;;         (emojify-latexify-emoji-in-buffer)
  ;;         (buffer-string)))))
  
  ;; (add-to-list 'org-export-plain-text-functions #'+org-latex-convert-emojis)
  ;; (defun org-latex-emoji-install-vector-graphics ()
  ;;   "Dowload, convert, and install vector emojis for use with LaTeX."
  ;;   (interactive)
  ;;   (let ((dir (org-latex-emoji-install-vector-graphics--download)))
  ;;     (org-latex-emoji-install-vector-graphics--convert dir)
  ;;     (org-latex-emoji-install-vector-graphics--install dir))
  ;;   (message "Vector emojis installed."))
  
  ;; (defun org-latex-emoji-install-vector-graphics--download ()
  ;;   (message "Locating latest emojis...")
  ;;   (let* ((twemoji-url (substring (shell-command-to-string "echo \"https://github.com$(curl -sL https://github.com/twitter/twemoji/releases/latest | grep '.zip\"' | cut -d '\"' -f 2)\"") 0 -1))
  ;;          (twemoji-version (replace-regexp-in-string "^.*tags/v\\(.*\\)\\.zip" "\\1" twemoji-url))
  ;;          (twemoji-dest-folder (make-temp-file "twemoji-" t)))
  ;;     (message "Downloading Twemoji v%s" twemoji-version)
  ;;     (let ((default-directory twemoji-dest-folder))
  ;;       (call-process "curl" nil nil nil "-L" twemoji-url "--output" "twemoji.zip")
  ;;       (message "Unzipping")
  ;;       (call-process "unzip" nil nil nil "twemoji.zip")
  ;;       (concat twemoji-dest-folder "/twemoji-" twemoji-version "/assets/svg"))))
  
  ;; (defun org-latex-emoji-install-vector-graphics--convert (dir)
  ;;   (let ((default-directory dir))
  ;;     (if (executable-find "cairosvg") ; cairo's PDFs are ~10% smaller
  ;;         (let* ((images (directory-files dir nil ".*.svg"))
  ;;                (num-images (length images))
  ;;                (index 0)
  ;;                (max-threads (1- (string-to-number (shell-command-to-string "nproc"))))
  ;;                (threads 0))
  ;;           (while (< index num-images)
  ;;             (setf threads (1+ threads))
  ;;             (message "Converting emoji %d/%d (%s)" (1+ index) num-images (nth index images))
  ;;             (make-process :name "cairosvg"
  ;;                           :command (list "cairosvg" (nth index images) "-o" (concat (file-name-sans-extension (nth index images)) ".pdf"))
  ;;                           :sentinel (lambda (proc msg)
  ;;                                       (when (memq (process-status proc) '(exit signal))
  ;;                                         (setf threads (1- threads)))))
  ;;             (setq index (1+ index))
  ;;             (while (> threads max-threads)
  ;;               (sleep-for 0.01)))
  ;;           (while (> threads 0)
  ;;             (sleep-for 0.01))
  ;;           (message "Finished conversion!")))
  ;;     (shell-command "inkscape --batch-process --export-type='pdf' *.svg")))
  
  ;; (defun org-latex-emoji-install-vector-graphics--install (dir)
  ;;   (message "Installing vector emojis into emoji directory")
  ;;   (let ((images (directory-files dir t ".*.pdf"))
  ;;         (emoji-dir (concat (emojify-image-dir) "/")))
  ;;     (mapcar
  ;;      (lambda (image)
  ;;        (rename-file image emoji-dir t))
  ;;      images)))
  
  (defvar +org-pdflatex-inputenc-encoded-chars
    "[[:ascii:]\u00A0-\u01F0\u0218-\u021BȲȳȷˆˇ˜˘˙˛˝\u0400-\u04FFḂḃẞ\u200B\u200C\u2010-\u201E†‡•…‰‱‹›※‽⁄⁎⁒₡₤₦₩₫€₱℃№℗℞℠™Ω℧℮←↑→↓〈〉␢␣◦◯♪⟨⟩Ḡḡ\uFB00-\uFB06\u2500-\u259F]")
  (defun +org-latex-replace-non-ascii-chars (text backend info)
    "Replace non-ascii chars with \\char\"XYZ forms."
    (when (and (org-export-derived-backend-p backend 'latex)
               (string= (plist-get info :latex-compiler) "pdflatex"))
      (let (case-replace)
        (replace-regexp-in-string "[^[:ascii:]]"
                                  (lambda (nonascii)
                                    (if (string-match-p +org-pdflatex-inputenc-encoded-chars nonascii) nonascii
                                      (or (cdr (assoc nonascii +org-latex-non-ascii-char-substitutions)) "¿")))
                                  text))))
  
  (add-to-list 'org-export-filter-final-output-functions #'+org-latex-replace-non-ascii-chars t)
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
  (setq org-beamer-theme "[progressbar=foot]metropolis")
  (defun org-beamer-p (info)
    (eq 'beamer (and (plist-get info :back-end) (org-export-backend-name (plist-get info :back-end)))))
  
  (add-to-list 'org-latex-conditional-features '(org-beamer-p . beamer) t)
  (add-to-list 'org-latex-feature-implementations '(beamer :requires .missing-koma :prevents (italic-quotes condensed-lists)) t)
  (add-to-list 'org-latex-feature-implementations '(.missing-koma :snippet "\\usepackage{scrextend}" :order 2) t)
  (setq org-beamer-frame-level 2)
  (setq org-re-reveal-theme "white"
        org-re-reveal-transition "slide"
        org-re-reveal-plugins '(markdown notes math search zoom))
  (setq org-ascii-charset 'utf-8)
  (defadvice! org-md-plain-text-unicode-a (orig-fn text info)
    "Locally rebind `org-html-special-string-regexps'"
    :around #'org-md-plain-text
    (let ((org-html-special-string-regexps
           '(("\\\\-" . "-")
             ("---\\([^-]\\|$\\)" . "-\\1")
             ("--\\([^-]\\|$\\)" . "-\\1")
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
  (add-to-list '+org-babel-mode-alist '(jags . ess-jags))
)

(setq org-id-method 'ts)

(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star ["◉" "✜" "✸" "✿" "✤" "○" "◆" "▶"]
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((43 . "➤")
                          (45 . "-")
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
        org-modern-progress nil
        org-modern-priority nil
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
          ("import" . "⇤")
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
          ("RESULTS" . "🠶")))
  (custom-set-faces! '(org-modern-statistics :inherit org-checkbox-statistics-todo)))

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(use-package valign
  :init
  (require 'valign)
  ;; :hook
  ;; (org-mode . valign-mode)
  ;; (markdown-mode . valign-mode)
  :config
  (setq valign-fancy-bar 1)
  )

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

(use-package! ob-http
  :commands org-babel-execute:http)

(use-package! org-transclusion
  :commands org-transclusion-mode
  :init
  (map! :after org :map org-mode-map
        "M-n" #'org-transclusion-mode))

(use-package! org-chef
  :commands (org-chef-insert-recipe org-chef-get-recipe-from-url))

(use-package! org-glossary
  :hook (org-mode . org-glossary-mode))

;; (evil-define-command evil-buffer-org-new (count file)
;;   "Creates a new ORG buffer replacing the current window, optionally
;;    editing a certain FILE"
;;   :repeat nil
;;   (interactive "P<f>")
;;   (if file
;;       (evil-edit file)
;;     (let ((buffer (generate-new-buffer "*new org*")))
;;       (set-window-buffer nil buffer)
;;       (with-current-buffer buffer
;;         (org-mode)))))
;; (map! :leader
;;       (:prefix "b"
;;        :desc "New empty ORG buffer" "o" #'evil-buffer-org-new))

(use-package! citar
  :when (modulep! :completion vertico)
  :config
  (setq citar-bibliography
        (let ((libfile-search-names '("library.bib" "Library.bib" "library.json" "Library.json"))
              (libfile-dir "~/Zotero")
              paths)
          (dolist (libfile libfile-search-names)
            (when (and (not paths)
                       (file-exists-p (expand-file-name libfile libfile-dir)))
              (setq paths (list (expand-file-name libfile libfile-dir)))))
          paths)))

(use-package! citeproc
  :defer t)

;;; Org-Cite configuration

(map! :after org
      :map org-mode-map
      :localleader
      :desc "Insert citation" "@" #'org-cite-insert)

(use-package! oc
  :after org citar
  :config
  (require 'ox)
  (setq org-cite-global-bibliography
        (let ((paths (or citar-bibliography
                         (bound-and-true-p bibtex-completion-bibliography))))
          ;; Always return bibliography paths as list for org-cite.
          (if (stringp paths) (list paths) paths)))
  ;; setup export processor; default csl/citeproc-el, with biblatex for latex
  (setq org-cite-export-processors
        '((t csl))))

;; Org-cite processors
(use-package! oc-biblatex
  :after oc)

(use-package! oc-csl
  :after oc
  :config
  (setq org-cite-csl-styles-dir "~/Zotero/styles"))

(use-package! oc-natbib
  :after oc)

;;;; Third-party

(use-package! citar-org
  :no-require
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (org-support-shift-select t)
  (when (modulep! :lang org +roam2)
    ;; Include property drawer metadata for 'org-roam' v2.
    (citar-org-note-include '(org-id org-roam-ref)))
  ;; Personal extras
  (setq citar-symbols
        `((file ,(all-the-icons-faicon "file-o" :v-adjust -0.1) . " ")
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-silver :v-adjust -0.3) . " ")
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-dsilver :v-adjust 0.01) . " "))))

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

(after! lsp-clangd
  (setq lsp-clients-clangd-args
        '("-j=12"
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion-decorators=0"))
  (set-lsp-priority! 'clangd 2))

(use-package! org-super-agenda
  :commands org-super-agenda-mode)

(use-package! doct
  :commands doct)

(setq org-directory "~/org"
      org-roam-directory org-directory)

(use-package org-roam
  :defer t
  :config
  (setq org-roam-directory org-directory)
  )

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :commands org-roam-ui-open
  :hook (org-roam . org-roam-ui-mode)
  :config
  (require 'org-roam) ; in case autoloaded
  (defun org-roam-ui-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (unless org-roam-ui-mode (org-roam-ui-mode 1))
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-ui-port))))

(use-package! xenops
  ;;:hook (org-mode . xenops-mode)
  :config
  ;;test, compare it with org-fragtog and pdfaltex
  ;;(add-hook 'latex-mode-hook #'xenops-mode)
  ;;(add-hook 'LaTeX-mode-hook #'xenops-mode)
  ;;(add-hook 'org-mode-hook #'xenops-mode)
  )
(setq xenops-reveal-on-entry t
      xenops-image-directory (expand-file-name "xenops/image" user-emacs-directory)
      xenops-math-latex-process 'pdflatex
      )
(setq xenops-math-latex-process-alist
      '((dvipng :programs
                ("latex" "dvipng")
                :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
                (1.0 . 1.0)
                :latex-compiler
                ("latex -interaction nonstopmode -shell-escape -output-format dvi -output-directory %o %f")
                :image-converter
                ("dvipng -D %D -T tight -o %O %f"))
        (latex :programs
                  ("latex" "dvisvgm")
                  :description "dvi > svg"
                  :message "you need to install the programs: latex and dvisvgm."
                  :image-input-type "dvi"
                  :image-output-type "svg"
                  :image-size-adjust
                  (1.2 . 1.2)
                  :latex-compiler
                  ("latex -interaction nonstopmode -output-format=dvi -output-directory %o %f")
                  :image-converter
                  ("dvisvgm %f -n -b min -c %S -o %O"))
        (lualatex :programs ("lualatex" "dvisvgm")
                  :description "dvi > svg"
                  :use-xcolor t
                  :message
                  "you need to install the programs: lualatex and dvisvgm. add --jiton option to use luajittex"
                  :image-input-type "dvi"
                  :image-output-type "svg"
                  :image-size-adjust (1.0 . 1.0)
                  :latex-compiler                  ("lualatex --interaction=nonstopmode --shell-escape --output-format=dvi --output-directory=%o %f")
                  :image-converter
                  ("dvisvgm %f -n -b min -c %S -o %O"))
        (tectonic :programs
                  ("latex" "dvisvgm")
                  :description "xdv > svg"
                  :message "you need to install the programs: tectonic and dvisvgm."
                  :image-input-type "xdv"
                  :image-output-type "svg"
                  :image-size-adjust (1.0 . 1.0)
                  :latex-compiler
                  ("tectonic -X compile %f -Z shell-escape --outfmt xdv --outdir %o")
                  :image-converter
                  ("dvisvgm %f -n -b min -c %S -o %O"))
        (xelatex :programs ("xelatex" "dvisvgm")
                 :description "xdv > svg"
                 :message "you need to install the programs: xelatex and dvisvgm."
                 :image-input-type "xdv"
                 :image-output-type "svg"
                 :image-size-adjust (0.75 . 0.75)
                 :latex-compiler
                 ("xelatex -no-pdf -interaction nonstopmode -shell-escape -output-directory %o %f")
                 :image-converter
                 ("dvisvgm %f -n -b min -c %S -o %O"))
        (imagemagick :programs
                     ("latex" "convert")
                     :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                     (1.0 . 1.0)
                     :latex-compiler
                     ("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f")
                     :image-converter
                     ("convert -density %D -trim -antialias %f -quality 100 %O"))))

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
            (cdr unicode) latex)))))

(use-package! ox-gfm
  :after ox)

;; [[file:config.org::*Compilation][Compilation:1]]
(setq TeX-save-query nil
      TeX-show-compilation t
      TeX-command-extra-options "-shell-escape")
(after! latex
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t)))
;; Compilation:1 ends here

;; [[file:config.org::*Compilation][Compilation:2]]
(setq +latex-viewers '(pdf-tools evince zathura okular skim sumatrapdf))
(use-package pdf-tools
  :config
  (setq-default pdf-view-display-size 'fit-width)
  )
;; Compilation:2 ends here

;; [[file:config.org::*Template][Template:2]]
(setq tec/yas-latex-template-preamble "
\\usepackage[pdfa,unicode=true,hidelinks]{hyperref}

\\usepackage[dvipsnames,svgnames,table,hyperref]{xcolor}
\\renewcommand{\\UrlFont}{\\ttfamily\\small}

\\usepackage[a-2b]{pdfx} % why not be archival

\\usepackage[T1]{fontenc}
\\usepackage[osf]{newpxtext}  % Palatino
\\usepackage{gillius}
\\usepackage[scale=0.9]{sourcecodepro}
% use amsmath instead
%\\usepackage[varbb]{newpxmath}
\\usepackage{mathtools}
\\usepackage{amssymb}
\\usepackage{amsmath}

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
;; Template:2 ends here

;; [[file:config.org::*Deliminators][Deliminators:1]]
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
;; Deliminators:1 ends here

;; [[file:config.org::*Editor visuals][Editor visuals:1]]
;; (after! latex
;;   (setcar (assoc "⋆" LaTeX-fold-math-spec-list) "★")) ;; make \star bigger

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
        ("'{1}'" ("text"))
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
;; Editor visuals:1 ends here

;; [[file:config.org::*Editor visuals][Editor visuals:2]]
(after! tex
  (map!
   :map LaTeX-mode-map
   :ei [C-return] #'LaTeX-insert-item)
  (setq TeX-electric-math '("\\(" . "")))
;; Editor visuals:2 ends here

;; [[file:config.org::*Editor visuals][Editor visuals:3]]
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
;; Editor visuals:3 ends here

;; [[file:config.org::*Editor visuals][Editor visuals:4]]
(setq preview-LaTeX-command '("%`%l \"\\nonstopmode\\nofiles\
\\PassOptionsToPackage{" ("," . preview-required-option-list) "}{preview}\
\\AtBeginDocument{\\ifx\\ifPreview\\undefined"
preview-default-preamble "\\fi}\"%' \"\\detokenize{\" %t \"}\""))
;; Editor visuals:4 ends here

;; [[file:config.org::*CDLaTeX][CDLaTeX:1]]
(after! cdlatex
  (setq cdlatex-env-alist
        '(("bmatrix" "\\begin{bmatrix}\n?\n\\end{bmatrix}" nil)
          ("equation*" "\\begin{equation*}\n?\n\\end{equation*}" nil)))
  (setq ;; cdlatex-math-symbol-prefix ?\; ;; doesn't work at the moment :(
   cdlatex-simplify-sub-super-scripts nil
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
;; CDLaTeX:1 ends here

;; [[file:config.org::*LAAS][LAAS:2]]
(use-package! laas
  :hook
  ((LaTeX-mode . laas-mode)
   (latex-mode . laas-mode)
   (org-mode . laas-mode))
  :config
  (setq laas-enable-auto-space nil)
  (aas-set-snippets 'laas-mode
    ;; set condition!
    :cond #'texmathp ; expand only while in math
    "supp" "\\supp"
    "On" "O(n)"
    "O1" "O(1)"
    "Olog" "O(\\log n)"
    "Olon" "O(n \\log n)"
    ;; bind to functions!
    "Sum" (lambda () (interactive)
            (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
    "Span" (lambda () (interactive)
             (yas-expand-snippet "\\Span($1)$0"))
    ;; add accent snippets
    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))
    "tan" "\\tan"
    ;; inner product
    "i*" (lambda () (interactive)
           (yas-expand-snippet "\\langle $1\\rangle$0"))
    "sr" "^2"
    ;; 这是 laas 中定义的用于包裹式 latex 代码的函数，实现 \bm{a}
    :cond #'laas-object-on-left-condition
    ",." (lambda () (interactive) (laas-wrap-previous-object "bm"))
    ".," (lambda () (interactive) (laas-wrap-previous-object "bm"))
))
;; LAAS:2 ends here

;; [[file:config.org::*SyncTeX][SyncTeX:1]]
(after! tex
  (add-to-list 'TeX-view-program-list '("Evince" "evince %o"))
  (add-to-list 'TeX-view-program-selection '(output-pdf "Evince")))
;; SyncTeX:1 ends here

;; [[file:config.org::*Fixes][Fixes:1]]
(when EMACS28+
  (add-hook 'latex-mode-hook #'TeX-latex-mode))
;; Fixes:1 ends here

;; [[file:config.org::*Python][Python:1]]
(after! lsp-python-ms
  (set-lsp-priority! 'mspyls 1))
;; Python:1 ends here

;; [[file:config.org::*Haskell][Haskell:1]]
(after! lsp-haskell
  (setq lsp-haskell-formatting-provider "stylish-haskell"
        haskell-interactive-popup-errors nil
        ))
;; Haskell:1 ends here

;; [[file:config.org::*MuPDF][MuPDF:2]]
;; (use-package paper
;;   :mode ("\\.pdf\\'"  . paper-mode)
;;   :mode ("\\.epub\\'"  . paper-mode)
;;   :mode ("\\.cbz\\'"  . paper-mode)
;;   :config
;;   ;;(require 'evil-collection-paper)
;;   ;;(evil-collection-paper-setup)
;;   )
;; MuPDF:2 ends here

;; [[file:config.org::*Terminal viewing][Terminal viewing:2]]

;; Terminal viewing:2 ends here

;; [[file:config.org::*Terminal viewing][Terminal viewing:3]]
;;(use-package! pdftotext
;;  :init
;;  (unless (display-graphic-p)
;;    (add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdftotext-mode))
;;    (add-to-list 'magic-mode-alist '("%PDF" . pdftotext-mode)))
;;  :config
;;  (unless (display-graphic-p) (after! pdf-tools (pdftotext-install)))
;;  ;; For prettyness
;;  (add-hook 'pdftotext-mode-hook #'spell-fu-mode-disable)
;;  (add-hook 'pdftotext-mode-hook (lambda () (page-break-lines-mode 1)))
;;  ;; I have no idea why this is needed
;;  (map! :map pdftotext-mode-map
;;        "<mouse-4>" (cmd! (scroll-down mouse-wheel-scroll-amount-horizontal))
;;        "<mouse-5>" (cmd! (scroll-up mouse-wheel-scroll-amount-horizontal))))
;; Terminal viewing:3 ends here

;; [[file:config.org::*Editor Visuals][Editor Visuals:1]]
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
;; Editor Visuals:1 ends here

;; [[file:config.org::*Julia][Julia:1]]
(after! julia-mode
  (add-hook 'julia-mode-hook #'rainbow-delimiters-mode-enable)
  (add-hook! 'julia-mode-hook
    (setq-local lsp-enable-folding t
                lsp-folding-range-limit 100)))
;; Julia:1 ends here

;; [[file:config.org::*Graphviz][Graphviz:2]]
(use-package! graphviz-dot-mode
  :commands graphviz-dot-mode
  :mode ("\\.dot\\'" . graphviz-dot-mode)
  :init
  (after! org
    (setcdr (assoc "dot" org-src-lang-modes)
            'graphviz-dot)))

(use-package! company-graphviz-dot
  :after graphviz-dot-mode)
;; Graphviz:2 ends here

;; [[file:config.org::*Markdown][Markdown:1]]
(add-hook! (gfm-mode markdown-mode) #'visual-line-mode #'turn-off-auto-fill)
;; Markdown:1 ends here

;; [[file:config.org::*Markdown][Markdown:2]]
(custom-set-faces!
  '(markdown-header-face-1 :height 1.25 :inherit markdown-header-face)
  '(markdown-header-face-2 :height 1.15 :inherit markdown-header-face)
  '(markdown-header-face-3 :height 1.08 :inherit markdown-header-face)
  '(markdown-header-face-4 :height 1.00 :inherit markdown-header-face)
  '(markdown-header-face-5 :height 0.90 :inherit markdown-header-face)
  '(markdown-header-face-6 :height 0.75 :inherit markdown-header-face))
;; Markdown:2 ends here

;; [[file:config.org::*Beancount][Beancount:2]]
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
;; Beancount:2 ends here

;; [[file:config.org::*wakatime][wakatime:2]]
(use-package wakatime-mode
  :config
  (global-wakatime-mode)
  )
;; wakatime:2 ends here

;; [[file:config.org::*Input Method][Input Method:2]]
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
;; Input Method:2 ends here

;; [[file:config.org::*Defaults][Defaults:1]]
(setq calc-angle-mode 'rad  ; radians are rad
      calc-symbolic-mode t) ; keeps expressions like \sqrt{2} irrational for as long as possible
;; Defaults:1 ends here

;; [[file:config.org::*CalcTeX][CalcTeX:2]]
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
;; CalcTeX:2 ends here

;; [[file:config.org::*Embedded calc][Embedded calc:1]]
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
;; Embedded calc:1 ends here

;; [[file:config.org::*Embedded calc][Embedded calc:2]]
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
;; Embedded calc:2 ends here

;; [[file:config.org::*IRC][IRC:3]]
(after! circe
  (setq-default circe-use-tls t)
  (setq circe-notifications-alert-icon "/usr/share/icons/breeze/actions/24/network-connect.svg"
        lui-logging-directory "~/.emacs.d/.local/etc/irc"
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
      ("blush"                         . ":)")
      ("innocent"                      . "😇")
      ("slight_smile"                  . ":)")
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
;; IRC:3 ends here

;; [[file:config.org::org-emph-to-irc][org-emph-to-irc]]
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
;; org-emph-to-irc ends here

;; [[file:config.org::*Keybindings][Keybindings:1]]
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
;; Keybindings:1 ends here

;; [[file:config.org::*Usability enhancements][Usability enhancements:1]]
;; (after! elfeed-search
;;   (set-evil-initial-state! 'elfeed-search-mode 'normal))
;; (after! elfeed-show-mode
;;   (set-evil-initial-state! 'elfeed-show-mode   'normal))

(after! evil-snipe
  (push 'elfeed-show-mode   evil-snipe-disabled-modes)
  (push 'elfeed-search-mode evil-snipe-disabled-modes))
;; Usability enhancements:1 ends here

;; [[file:config.org::*Functionality enhancements][Functionality enhancements:1]]
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
;; Functionality enhancements:1 ends here

;; [[file:config.org::*Rebuild mail index while using mu4e][Rebuild mail index while using mu4e:1]]
(after! mu4e
  (defvar mu4e-reindex-request-file "/tmp/mu_reindex_now"
    "Location of the reindex request, signaled by existance")
  (defvar mu4e-reindex-request-min-seperation 5.0
    "Don't refresh again until this many second have elapsed.
Prevents a series of redisplays from being called (when set to an appropriate value)")

  (defvar mu4e-reindex-request--file-watcher nil)
  (defvar mu4e-reindex-request--file-just-deleted nil)
  (defvar mu4e-reindex-request--last-time 0)

  (defun mu4e-reindex-request--add-watcher ()
    (setq mu4e-reindex-request--file-just-deleted nil)
    (setq mu4e-reindex-request--file-watcher
          (file-notify-add-watch mu4e-reindex-request-file
                                 '(change)
                                 #'mu4e-file-reindex-request)))

  (defadvice! mu4e-stop-watching-for-reindex-request ()
    :after #'mu4e~proc-kill
    (if mu4e-reindex-request--file-watcher
        (file-notify-rm-watch mu4e-reindex-request--file-watcher)))

  (defadvice! mu4e-watch-for-reindex-request ()
    :after #'mu4e~proc-start
    (mu4e-stop-watching-for-reindex-request)
    (when (file-exists-p mu4e-reindex-request-file)
      (delete-file mu4e-reindex-request-file))
    (mu4e-reindex-request--add-watcher))

  (defun mu4e-file-reindex-request (event)
    "Act based on the existance of `mu4e-reindex-request-file'"
    (if mu4e-reindex-request--file-just-deleted
        (mu4e-reindex-request--add-watcher)
      (when (equal (nth 1 event) 'created)
        (delete-file mu4e-reindex-request-file)
        (setq mu4e-reindex-request--file-just-deleted t)
        (mu4e-reindex-maybe t))))

  (defun mu4e-reindex-maybe (&optional new-request)
    "Run `mu4e~proc-index' if it's been more than
`mu4e-reindex-request-min-seperation'seconds since the last request,"
    (let ((time-since-last-request (- (float-time)
                                      mu4e-reindex-request--last-time)))
      (when new-request
        (setq mu4e-reindex-request--last-time (float-time)))
      (if (> time-since-last-request mu4e-reindex-request-min-seperation)
          (mu4e~proc-index nil t)
        (when new-request
          (run-at-time (* 1.1 mu4e-reindex-request-min-seperation) nil
                       #'mu4e-reindex-maybe))))))
;; Rebuild mail index while using mu4e:1 ends here

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(after! mu4e
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
  (setq mu4e-headers-fields
        '((:flags . 6)
          (:account-stripe . 2)
          (:from-or-to . 25)
          (:folder . 10)
          (:recipnum . 2)
          (:subject . 80)
          (:human-date . 8))
        +mu4e-min-header-frame-width 142
        mu4e-headers-date-format "%d/%m/%y"
        mu4e-headers-time-format "⧖ %H:%M"
        mu4e-headers-results-limit 1000
        mu4e-index-cleanup t)
  
  (add-to-list 'mu4e-bookmarks
               '(:name "Yesterday's messages" :query "date:2d..1d" :key ?y) t)
  
  (defvar +mu4e-header--folder-colors nil)
  (appendq! mu4e-header-info-custom
            '((:folder .
               (:name "Folder" :shortname "Folder" :help "Lowest level folder" :function
                (lambda (msg)
                  (+mu4e-colorize-str
                   (replace-regexp-in-string "\\`.*/" "" (mu4e-message-field msg :maildir))
                   '+mu4e-header--folder-colors))))))
  (setq mu4e-alert-icon "/usr/share/icons/Papirus/64x64/apps/evolution.svg")
  (setq sendmail-program "/usr/bin/msmtp"
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from"); , "--read-recipients")
        message-send-mail-function #'message-send-mail-with-sendmail)
  (defun mu4e-compose-from-mailto (mailto-string &optional quit-frame-after)
    (require 'mu4e)
    (unless mu4e~server-props (mu4e t) (sleep-for 0.1))
    (let* ((mailto (message-parse-mailto-url mailto-string))
           (to (cdr (assoc "To" mailto)))
           (subject (or (cdr (assoc "Subject" mailto)) ""))
           (body (cdr (assoc "Body" mailto)))
           (headers (-filter (lambda (spec) (not (-contains-p '("To" "Subject" "Body") (car spec)))) mailto)))
      (when-let ((mu4e-main (get-buffer mu4e-main-buffer-name)))
        (switch-to-buffer mu4e-main))
      (mu4e~compose-mail to subject headers)
      (when body
        (goto-char (point-min))
        (if (eq major-mode 'org-msg-edit-mode)
            (org-msg-goto-body)
          (mu4e-compose-goto-bottom))
        (insert body))
      (goto-char (point-min))
      (cond ((null to) (search-forward "To: "))
            ((string= "" subject) (search-forward "Subject: "))
            (t (if (eq major-mode 'org-msg-edit-mode)
                   (org-msg-goto-body)
                 (mu4e-compose-goto-bottom))))
      (font-lock-ensure)
  ;;TODO: replace this with meow's config
      ;; (when evil-normal-state-minor-mode
      ;;   (evil-append 1))
      (when quit-frame-after
        (add-hook 'kill-buffer-hook
                  `(lambda ()
                     (when (eq (selected-frame) ,(selected-frame))
                       (delete-frame)))))))
  (defvar mu4e-from-name "VitalyR"
    "Name used in \"From:\" template.")
  (defadvice! mu4e~draft-from-construct-renamed (orig-fn)
    "Wrap `mu4e~draft-from-construct-renamed' to change the name."
    :around #'mu4e~draft-from-construct
    (let ((user-full-name mu4e-from-name))
      (funcall orig-fn)))
  (setq message-signature mu4e-from-name)
  (defun +mu4e-get-woof-header ()
    (pcase (read-char
            (format "\
  %s
    %s Declare %s Applied %s Aborted
  %s
    %s Confirm %s Fixed
  %s
    %s Request %s Resolved
  
  %s remove X-Woof header"
                    (propertize "Patch" 'face 'outline-3)
                    (propertize "p" 'face '(bold consult-key))
                    (propertize "a" 'face '(bold consult-key))
                    (propertize "c" 'face '(bold consult-key))
                    (propertize "Bug" 'face 'outline-3)
                    (propertize "b" 'face '(bold consult-key))
                    (propertize "f" 'face '(bold consult-key))
                    (propertize "Help" 'face 'outline-3)
                    (propertize "h" 'face '(bold consult-key))
                    (propertize "r" 'face '(bold consult-key))
                    (propertize "x" 'face '(bold error))))
      (?p "X-Woof-Patch: confirmed")
      (?a "X-Woof-Patch: applied")
      (?c "X-Woof-Patch: cancelled")
      (?b "X-Woof-Bug: confirmed")
      (?f "X-Woof-Bug: fixed")
      (?h "X-Woof-Help: confirmed")
      (?r "X-Woof-Help: cancelled")
      (?x 'delete)))
  (defun +mu4e-insert-woof-header ()
    "Insert an X-Woof header into the current message."
    (interactive)
    (when-let ((header (+mu4e-get-woof-header)))
      (save-excursion
        (goto-char (point-min))
        (search-forward "--text follows this line--")
        (unless (eq header 'delete)
          (beginning-of-line)
          (insert header "\n")
          (forward-line -1))
        (when (re-search-backward "^X-Woof-" nil t)
          (kill-whole-line)))))
  
  (map! :map mu4e-compose-mode-map
        :localleader
        :desc "Insert X-Woof Header" "w" #'+mu4e-insert-woof-header)
  
  (map! :map org-msg-edit-mode-map
        :after org-msg
        :localleader
        :desc "Insert X-Woof Header" "w" #'+mu4e-insert-woof-header)
  (after! mu4e
    (defvar +org-ml-target-dir "~/.emacs.d/.local/straight/repos/org-mode/")
    (defvar +org-ml-max-age 600
      "Maximum permissible age in seconds.")
    (defvar +org-ml--cache-timestamp 0)
    (defvar +org-ml--cache nil)
  
    (define-minor-mode +org-ml-patchy-mood-mode
      "Apply patches to Org in bulk."
      :global t
      (let ((action (cons "apply patch to org" #'+org-ml-apply-patch)))
        (if +org-ml-patchy-mood-mode
            (add-to-list 'mu4e-view-actions action)
          (setq mu4e-view-actions (delete action mu4e-view-actions)))))
  
    (defun +org-ml-apply-patch (msg)
      "Apply the patch in the current message to Org."
      (interactive)
      (unless msg (setq msg (mu4e-message-at-point)))
      (with-current-buffer (get-buffer-create "*Shell: Org apply patches*")
        (erase-buffer)
        (let* ((default-directory +org-ml-target-dir)
               (exit-code (call-process "git" nil t nil "am" (mu4e-message-field msg :path))))
          (magit-refresh)
          (when (not (= 0 exit-code))
            (+popup/buffer)))))
  
    (defun +org-ml-current-patches ()
      "Get the currently open patches, as a list of alists.
  Entries of the form (subject . id)."
      (delq nil
            (mapcar
             (lambda (entry)
               (unless (plist-get entry :fixed)
                 (cons
                  (format "%-8s  %s"
                          (propertize
                           (replace-regexp-in-string "T.*" ""
                                                     (plist-get entry :date))
                           'face 'font-lock-doc-face)
                          (propertize
                           (replace-regexp-in-string "\\[PATCH\\] ?" ""
                                                     (plist-get entry :summary))
                           'face 'font-lock-keyword-face))
                  (plist-get entry :id))))
             (with-current-buffer (url-retrieve-synchronously "https://updates.orgmode.org/data/patches")
               (goto-char url-http-end-of-headers)
               (json-parse-buffer :object-type 'plist)))))
  
    (defun +org-ml-select-patch-thread ()
      "Find and apply a proposed Org patch."
      (interactive)
      (let* ((current-workspace (+workspace-current))
             (patches (progn
                        (when (or (not +org-ml--cache)
                                  (> (- (float-time) +org-ml--cache-timestamp)
                                     +org-ml-max-age))
                          (setq +org-ml--cache (+org-ml-current-patches)
                                +org-ml--cache-timestamp (float-time)))
                        +org-ml--cache))
             (msg-id (cdr (assoc (completing-read
                                  "Thread: " (mapcar #'car patches))
                                 patches))))
        (+workspace-switch +mu4e-workspace-name)
        (mu4e-view-message-with-message-id msg-id)
        (unless +org-ml-patchy-mood-mode
          (add-to-list 'mu4e-view-actions
                       (cons "apply patch to org" #'+org-ml-transient-mu4e-action)))))
  
    (defun +org-ml-transient-mu4e-action (msg)
      (setq mu4e-view-actions
            (delete (cons "apply patch to org" #'+org-ml-transient-mu4e-action)
                    mu4e-view-actions))
      (+workspace/other)
      (magit-status +org-ml-target-dir)
      (+org-ml-apply-patch msg)))
  (after! mu4e
    (defun +mu4e-ml-message-link (msg)
      "Copy the link to MSG on the mailing list archives."
      (let* ((list-addr (or (mu4e-message-field msg :mailing-list)
                            (cdar (mu4e-message-field-raw msg :list-post))
                            (thread-last (append (mu4e-message-field msg :to)
                                                 (mu4e-message-field msg :cc))
                              (mapcar #'last)
                              (mapcar #'cdr)
                              (mapcar (lambda (addr)
                                        (when (string-match-p "emacs.*@gnu\\.org$" addr)
                                          (replace-regexp-in-string "@" "." addr))))
                              (delq nil)
                              (car))))
             (msg-url
              (cond
               ((string= "emacs-orgmode.gnu.org" list-addr)
                (format "https://list.orgmode.org/%s" (mu4e-message-field msg :message-id)))
               (t (user-error "Mailing list %s not supported" list-addr)))))
        (message "Link %s copied to clipboard" (gui-select-text msg-url))
        msg-url))
  
    (add-to-list 'mu4e-view-actions (cons "link to message ML" #'+mu4e-ml-message-link) t))
  (defun +browse-url-orgmode-ml (url &optional _)
    "Open an orgmode list url using notmuch."
    (let ((id (and (or (string-match "^https?://orgmode\\.org/list/\\([^/]+\\)" url)
                       (string-match "^https?://list\\.orgmode\\.org/\\([^/]+\\)" url))
                   (match-string 1 url))))
      (mu4e-view-message-with-message-id id)))
  
  (add-to-list 'browse-url-handlers (cons "^https?://orgmode\\.org/list" #'+browse-url-orgmode-ml))
  (add-to-list 'browse-url-handlers (cons "^https?://list\\.orgmode\\.org/" #'+browse-url-orgmode-ml))
)

;; [[file:config.org::*Org Msg][Org Msg:1]]
(setq +org-msg-accent-color "#1a5fb4"
      org-msg-greeting-fmt "\nHi%s,\n\n"
      org-msg-signature "\n\n#+begin_signature\nAll the best,\\\\\n@@html:<b>@@Timothy@@html:</b>@@\n#+end_signature")
(map! :map org-msg-edit-mode-map
      :after org-msg
      :n "G" #'org-msg-goto-body)
;; Org Msg:1 ends here
