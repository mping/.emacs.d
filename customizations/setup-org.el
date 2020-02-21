;; https://orgmode.org/worg/org-tutorials/org4beginners.html

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))