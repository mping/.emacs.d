;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.

;; ensures dired sorts dot files and dirs first
(setenv "LC_COLLATE" "C") 

;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)


;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(ido-mode t)

;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
(setq ido-enable-flex-matching t)

;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point nil)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files, even if they're not
;; open now
(setq ido-use-virtual-buffers t)

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
;; (ido-ubiquitous-mode 1)

;; dired sorting
;(setq dired-use-ls-dired  nil)
;(setq insert-directory-program "/usr/local/bin/gls")
;(setq dired-listing-switches "-aBhl  --group-directories-first")
;;; cross platform unified ls
(require 'ls-lisp)
(setq dired-listing-switches "-alhG")
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-ignore-case t)
(setq ls-lisp-use-string-collate nil)
;; customise the appearance of the listing
(setq ls-lisp-verbosity '(links uid))
(setq ls-lisp-format-time-list '("%b %e %H:%M" "%b %e  %Y"))
(setq ls-lisp-use-localized-time-format t)
(setq ls-lisp-dirs-first t)



;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; quits emacs
(global-set-key (kbd "M-q") 'kill-emacs)


;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; projectile everywhere!
(projectile-global-mode)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-project-search-path '("~/Devel/workspace"))

;; neotree for project exploration
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; Every time when the neotree window is opened, let it find current file and jump to node.
(setq neo-smart-open t)
;;When running ‘projectile-switch-project’ (C-c p p), ‘neotree’ will change root automatically. 
(setq projectile-switch-project-action 'neotree-projectile-action)


;;;; sidebar.el
;; git submodule add https://github.com/sebastiencs/sidebar.el.git 
;; TODO move icons-in-terminal to a submodule too
(add-to-list 'load-path "~/.local/share/icons-in-terminal/") ;; If it's not already done
(add-to-list 'load-path "~/.emacs.d/sidebar.el")
(require 'sidebar)
(global-set-key (kbd "C-x C-a") 'sidebar-open)
(global-set-key (kbd "C-x C-f") 'sidebar-buffers-open)

;; https://www.emacswiki.org/emacs/WindMove
;; use shift+arrow keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
