;;;;
;; setup edts
;; I used kerl to install erlang

;; if edts doesnt start, go to ~/.emacs.d/elpa/edts-xxxx/ 
;; > make

;; if that throws an error because of webmachine, edit lib/webmachine/rebar.config
;; > remove "{erl_opts, [warnings_as_errors]}."

;; if that throws an error because of lager, edit lib/edts/rebar.config
;; > put lager_transform in front "{lager_transform, parse_transform}"
;;;;


(setq load-path (cons "/Users/mping/Devel/erlang/19.1/lib/tools-2.8.6/emacs" load-path))
(require 'erlang-start)

(setq erlang-root-dir "/Users/mping/Devel/erlang/19.1/")
(setq exec-path (cons "/Users/mping/Devel/erlang/19.1/bin" exec-path))
(setq erlang-man-root-dir "/Users/mping/Devel/erlang/19.1/man")

(add-hook 'after-init-hook 'setup-edts)
(defun setup-edts ()
  (require 'edts-start)
  ;;(add-to-list 'auto-mode-alist '("\\.erl.*$" . erlang-mode))
  )