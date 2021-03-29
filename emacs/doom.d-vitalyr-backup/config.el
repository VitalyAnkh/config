(add-hook! org-mode :append
           'visual-line-mode
           'variable-pitch-mode)

(add-hook! org-mode :append #'org-appear-mode)
