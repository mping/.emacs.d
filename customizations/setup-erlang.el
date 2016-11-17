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


;; disable edts while we try distel
;;(add-hook 'after-init-hook 'setup-edts)
(add-hook 'after-init-hook 'setup-flycheck)
(add-hook 'after-init-hook 'setup-distel)
(add-hook 'after-init-hook 'setup-autocomplete)


;;;;
;; EDTS
;;;;

(defun setup-edts ()
  (require 'edts-start)
  ;;(add-to-list 'auto-mode-alist '("\\.erl.*$" . erlang-mode))
  )


;;;;
;; flycheck + distel + company
;; to start properly: C-c C-z, move to erl buffer, then C-c C-d n, then kill (say 'y')
;;;;

;; flycheck from elpa
(defun setup-flycheck ()

	;; flycheck for syntax validation
	(require 'flycheck)
	(flycheck-define-checker erlang-otp
	                         "An Erlang syntax checker using the Erlang interpreter."
	                         :command ("erlc" "-o" temporary-directory "-Wall"
	                                   "-I" "../include" "-I" "../../include"
	                                   "-I" "../../../include" source)
	                         :error-patterns
	                         ((warning line-start (file-name) ":" line ": Warning:" (message) line-end)
	                          (error line-start (file-name) ":" line ": " (message) line-end))
	                         :modes (erlang-mode))

	(add-hook 'erlang-mode-hook
  	(lambda () (flycheck-select-checker 'erlang-otp) (flycheck-mode)))

	(require 'flycheck-tip)
	;;(define-key erlang-mode (kbd "C-c C-n") 'flycheck-tip-cycle)
	(setq flycheck-display-errors-function 'verbose)
	(with-eval-after-load 'flycheck (flycheck-pos-tip-mode)))

;; distel: > git clone https://github.com/massemanet/distel ~/.emacs/distel
(defun setup-distel ()
	;; distel
	;; prevent annoying hang-on-compile
	(defvar inferior-erlang-prompt-timeout t)
	;; default node name to emacs@localhost
	(setq inferior-erlang-machine-options '("-sname" "emacs"))
	;; tell distel to default to that node
	(setq erl-nodename-cache
	      (make-symbol
	       (concat
	        "emacs@"
	        ;; Mac OS X uses "name.local" instead of "name", this should work
	        ;; pretty much anywhere without having to muck with NetInfo
	        ;; ... but I only tested it on Mac OS X.
	                (car (split-string (shell-command-to-string "hostname"))))))

	(push "~/.emacs.d/distel/elisp/" load-path)
	(require 'distel)
	(distel-setup))


;; autocomplete: > git clone https://github.com/sebastiw/company-distel ~/.emacs/company-distel
(defun setup-autocomplete ()
	(push "~/.emacs.d/company-distel/" load-path)
	(require 'company-distel)
	(with-eval-after-load 'company
		(add-to-list 'company-backends 'company-distel))

	;;(add-hook 'erlang-mode-hook 'auto-complete-mode)
	(add-hook 'erlang-mode-hook (lambda () (setq company-backends '(company-distel)))))







