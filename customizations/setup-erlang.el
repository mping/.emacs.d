;;;;
;; setup edts

;; if edts doesnt start, go to ~/.emacs.d/elpa/edts-xxxx/ 
;; > make

;; if that throws an error because of webmachine, edit lib/webmachine/rebar.config
;; > remove "{erl_opts, [warnings_as_errors]}."

;; if that throws an error because of lager, edit lib/edts/rebar.config
;; > put lager_transform in front "{lager_transform, parse_transform}"
;;;;
(add-hook 'after-init-hook 'setup-edts)
(defun setup-edts ()
  ;; (require 'edts-start)
  ;;(add-to-list 'auto-mode-alist '("\\.erl.*$" . erlang-mode))
  )