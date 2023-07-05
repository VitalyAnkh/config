;;; init.el -*- lexical-binding: t; -*-
(setq native-comp-deferred-compilation nil)
;; (after! (doom-packages straight)
;;   (setq straight--native-comp-available t))
(doom! :completion vertico
       :config (default +bindings))
