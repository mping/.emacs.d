;;Tabbar
(require 'tabbar)

(global-set-key [C-prior] #'tabbar-backward-tab)
(global-set-key [C-next] #'tabbar-forward-tab)

;; gray is #93A1A1
;; green is #A6E22E
;; text is #07363D

;; Tabbar settings
(set-face-attribute
 'tabbar-default nil :background "gray20" :foreground "gray20" :box '(:line-width 1 :color "gray20" :style nil))

(set-face-attribute
 'tabbar-unselected nil :background "gray30" :foreground "#93A1A1" :box '(:line-width 5 :color "#93A1A1" :style nil))

(set-face-attribute
 'tabbar-highlight nil :background "gray75" :foreground "#A6E22E" :box '(:line-width 5 :color "#A6E22E" :style nil))

(set-face-attribute
 'tabbar-selected nil :background "#07363D" :foreground "#A6E22E" :underline nil :box '(:line-width 5 :color "#A6E22E" :style nil))

(set-face-attribute
 'tabbar-button nil :box '(:line-width 1 :color "gray20" :style nil))

(set-face-attribute
 'tabbar-separator nil :background "gray20" :height 0.6)

;; Change padding of the tabs
;; we also need to set separator to avoid overlapping tabs by highlighted tabs
(custom-set-variables
 '(tabbar-separator (quote (0.5))))

;; adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format " [%s]  " (tabbar-tab-tabset tab))
                  (format " %s " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

(tabbar-mode 1)
