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


;; use the adequate path
(setq load-path (cons "/Users/mping/Devel/erlang/19.1/lib/tools-2.8.6/emacs" load-path))
(require 'erlang-start)

(setq erlang-root-dir "/home/mping/devel/erlang/18.3/")
(setq exec-path (cons "/home/mping/devel/erlang/18.3/bin" exec-path))
(setq erlang-man-root-dir "/home/mping/devel/erlang/18.3/man")


(push "~/.emacs.d/distel/elisp/" load-path)
(push "~/.emacs.d/company-distel/" load-path)

(require 'flycheck)
(require 'distel)
(require 'company-distel)

;; disable edts while we try distel
;; (add-hook 'after-init-hook 'setup-edts)

(add-hook 'after-init-hook 'setup-flycheck)
(add-hook 'after-init-hook 'setup-distel)
(add-hook 'after-init-hook 'setup-autocomplete)

;;;;
;; EDTS
;;;;

(defun setup-edts ()
  (require 'edts-start))


;;;;
;; flycheck + distel + company
;; to start properly: C-c C-z, move to erl buffer, then C-c C-d n, then kill (say 'y')
;;;;

;; flycheck from elpa
(defun setup-flycheck ()

	;; flycheck for syntax validation

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

;; distel: > git clone https://github.com/massemanet/distel ~/.emacs.d/distel
;; http://web.archive.org/web/20130827210416/http://bc.tech.coop/blog/070528.html
(defun setup-distel ()
	;; distel
	;; prevent annoying hang-on-compile
	(defvar inferior-erlang-prompt-timeout t)
	;; default node name to emacs@localhost
	(setq inferior-erlang-machine-options '("-sname" "emacs"))
	(setq erlang-compile-extra-opts '(debug_info)) ;; so that when C-c C-k debug info is set up, no need do c("mod", [debug_info])
	;; tell distel to default to that node
	(setq erl-nodename-cache
	      (make-symbol
	       (concat
	        "emacs@"
	        ;; Mac OS X uses "name.local" instead of "name", this should work
	        ;; pretty much anywhere without having to muck with NetInfo
	        ;; ... but I only tested it on Mac OS X.
                (car (split-string (shell-command-to-string "hostname"))))))

	(distel-setup))


;; autocomplete: > git clone https://github.com/sebastiw/company-distel ~/.emacs.d/company-distel
(defun setup-autocomplete ()
	(with-eval-after-load 'company
		(add-to-list 'company-backends 'company-distel))
	;;(add-hook 'erlang-mode-hook 'auto-complete-mode)
	(add-hook 'erlang-mode-hook (lambda () (setq company-backends '(company-distel)))))







