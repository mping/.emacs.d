
;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("elpa" . "http://elpa.gnu.org/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; (add-to-list 'package-archives
;;            '("tromey" . "http://tromey.com/elpa/") t)

;;(add-to-list 'package-archives
;;             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;;                          ("melpa" . "http://melpa-stable.milkbox.net/packages/")))


;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)


(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit
    parinfer

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; complete anything mode, in-buffer
    company

    ;; custom env vars
    load-env-vars

    ;;autocompletion in emacs itself
    ;; https://oremacs.com/swiper/#installation
    ivy
    swiper
    counsel

    ;; themes
    doom-themes
    kaolin-themes

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    projectile
    ag
    neotree
    all-the-icons-dired
    all-the-icons ;; install fonts from https://github.com/domtronn/all-the-icons.el

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit

    undo-tree

    ;; sidebar stuff
    dash
    dash-functional
    s
    ov
    projectile
    frame-local ;;required from icons-in-terminal

    ;; org mode
    org

    ;;clj-kondo flycheck
    flycheck
    flycheck-clj-kondo

    ;; git integration
    magit
    ))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; required by icons-in-terminal
(load "font-lock-plus.el")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")
;;(load "theme.el") ;; this is for solarized only

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")


;; For editing lisps
(load "elisp-editing.el")

;; Language-specific
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-markdown.el")
(load "setup-org.el")

;; ivy configuration
(load "setup-ivy.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#383e3f" "#e84c58" "#1D6B69" "#E36B3F" "#3B84CC" "#a9779c" "#6facb3" "#C8CCC3"])
 '(ansi-term-color-vector
   [unspecified "#2d2a2e" "#ff6188" "#a9dc76" "#ffd866" "#78dce8" "#ab9df2" "#a1efe4" "#fcfcfa"])
 '(awesome-tray-mode-line-active-color "#0031a9")
 '(awesome-tray-mode-line-inactive-color "#d7d7d7")
 '(clojure-align-forms-automatically t)
 '(coffee-tab-width 2)
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "56911bd75304fdb19619c9cb4c7b0511214d93f18e566e5b954416756a20cc80" "3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" "a83f05e5e2f2538376ea2bfdf9e3cd8b7f7593b16299238c1134c1529503fa88" "10845272b6fa47a6cdfc49816748bdb1dc1cb9be647801c25c054a8e6a27ef72" "6b423e7cd5d1e705b0c92356273899057d26e125f83d7f007615ae088195682f" "45a8b89e995faa5c69aa79920acff5d7cb14978fbf140cdd53621b09d782edcf" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "bb28b083fe1c61848c10c049be076afc572ea9bee6e1f8dc2631c5ee4f7388c8" "c1c459af570241993823db87096bc775506c378aa02c9c6cd9ccaa8247056b96" "6a0d7f41968908e25b2f56fa7b4d188e3fc9a158c39ef680b349dccffc42d1c8" "ad16a1bf1fd86bfbedae4b32c269b19f8d20d416bd52a87cd50e355bf13c2f23" "d83e34e28680f2ed99fe50fea79f441ca3fddd90167a72b796455e791c90dc49" "5846b39f2171d620c45ee31409350c1ccaddebd3f88ac19894ae15db9ef23035" "fc7fd2530b82a722ceb5b211f9e732d15ad41d5306c011253a0ba43aaf93dccc" "7a1190ad27c73888f8d16142457f59026b01fa654f353c17f997d83565c0fc65" "df21cdadd3f0648e3106338649d9fea510121807c907e2fd15565dde6409d6e9" "eae831de756bb480240479794e85f1da0789c6f2f7746e5cc999370bbc8d9c8a" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "3de3f36a398d2c8a4796360bfce1fa515292e9f76b655bb9a377289a6a80a132" "a85e40c7d2df4a5e993742929dfd903899b66a667547f740872797198778d7b5" "8be07a2c1b3a7300860c7a65c0ad148be6d127671be04d3d2120f1ac541ac103" "d2bd16a8bcf295dce0b70e1d2b5c17bb34cb28224a86ee770d56e6c22a565013" "bc4c89a7b91cfbd3e28b2a8e9e6750079a985237b960384f158515d32c7f0490" "1ed5c8b7478d505a358f578c00b58b430dde379b856fbcb60ed8d345fc95594e" "0ad7f1c71fd0289f7549f0454c9b12005eddf9b76b7ead32a24d9cb1d16cbcbd" "7f791f743870983b9bb90c8285e1e0ba1bf1ea6e9c9a02c60335899ba20f3c94" "d5f8099d98174116cba9912fe2a0c3196a7cd405d12fa6b9375c55fc510988b5" "e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "58c3313b4811ed8b30239b1d04816f74d438bcb72626d68181f294b115b7220d" "229c5cf9c9bd4012be621d271320036c69a14758f70e60385e87880b46d60780" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "264b639ee1d01cd81f6ab49a63b6354d902c7f7ed17ecf6e8c2bd5eb6d8ca09c" "7c0495f3973b9f79251205995ccccca41262b41a86553f81efe71c0dc3a50f43" "31e9b1ab4e6ccb742b3b5395287760a0adbfc8a7b86c2eda4555c8080a9338d9" "c614d2423075491e6b7f38a4b7ea1c68f31764b9b815e35c9741e9490119efc0" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "52588047a0fe3727e3cd8a90e76d7f078c9bd62c0b246324e557dfa5112e0d0c" "c342ef444e7aca36f4b39a8e2848c4ba793d51c58fdb520b8ed887766ed6d40b" "8ca8fbaeaeff06ac803d7c42de1430b9765d22a439efc45b5ac572c2d9d09b16" "615123f602c56139c8170c153208406bf467804785007cdc11ba73d18c3a248b" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "2679db166117d5b26b22a8f12a940f5ac415d76b004de03fcd34483505705f62" "1d904ba8343822dff21ffae28a348975eafeb0734034ed5fa33d78bf2519e7cb" "17a58e509bbb8318abf3558c4b7b44273b4f1b555c5e91d00d4785b7b59d6d28" "9089d25e2a77e6044b4a97a2b9fe0c82351a19fdd3e68a885f40f86bbe3b3900" "c499bf4e774b34e784ef5a104347b81c56220416d56d5fd3fd85df8704260aad" "fc0fe24e7f3d48ac9cf1f87b8657c6d7a5dd203d5dabd2f12f549026b4c67446" "2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "7f1263c969f04a8e58f9441f4ba4d7fb1302243355cb9faecb55aec878a06ee9" "643b4d181b6afa4306d65db76889d8b987e095ae8f262a4c71dd5669d39c9b09" "1de8de5dddd3c5138e134696180868490c4fc86daf9780895d8fc728449805f3" "7e5d400035eea68343be6830f3de7b8ce5e75f7ac7b8337b5df492d023ee8483" "8ce796252a78d1a69e008c39d7b84a9545022b64609caac98dc7980d76ae34e3" "9ef81da35ce99a4c7155db7d46e4f8c20a51860d6879cf082e3ed1c5222c17d3" "6231254e74298a1cf8a5fee7ca64352943de4b495e615c449e9bb27e2ccae709" "6bacece4cf10ea7dd5eae5bfc1019888f0cb62059ff905f37b33eec145a6a430" "845103fcb9b091b0958171653a4413ccfad35552bc39697d448941bcbe5a660d" "24132f7b6699c6e0118d742351b74141bac3c4388937e40db9207554eaae78c9" "1d50bd38eed63d8de5fcfce37c4bb2f660a02d3dff9cbfd807a309db671ff1af" "7c4cfa4eb784539d6e09ecc118428cd8125d6aa3053d8e8413f31a7293d43169" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "361f5a2bc2a7d7387b442b2570b0ef35198442b38c2812bf3c70e1e091771d1a" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "f2b56244ecc6f4b952b2bcb1d7e517f1f4272876a8c873b378f5cf68e904bd59" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "9b01a258b57067426cc3c8155330b0381ae0d8dd41d5345b5eddac69f40d409b" "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" "f9cae16fd084c64bf0a9de797ef9caedc9ff4d463dd0288c30a3f89ecf36ca7e" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "fa3bdd59ea708164e7821574822ab82a3c51e262d419df941f26d64d015c90ee" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "51956e440cec75ba7e4cff6c79f4f8c884a50b220e78e5e05145386f5b381f7b" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "285efd6352377e0e3b68c71ab12c43d2b72072f64d436584f9159a58c4ff545a" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" "1526aeed166165811eefd9a6f9176061ec3d121ba39500af2048073bea80911e" "392395ee6e6844aec5a76ca4f5c820b97119ddc5290f4e0f58b38c9748181e8d" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "a63355b90843b228925ce8b96f88c587087c3ee4f428838716505fd01cf741c8" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "2da65cb7074c176ca0a33f06bcc83ef692c9175e41b6370f5e94eb5811d6ee3a" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" default))
 '(ensime-sem-high-faces
   '((var :foreground "#000000" :underline
          (:style wave :color "yellow"))
     (val :foreground "#000000")
     (varField :foreground "#600e7a" :slant italic)
     (valField :foreground "#600e7a" :slant italic)
     (functionCall :foreground "#000000" :slant italic)
     (implicitConversion :underline
                         (:color "#c0c0c0"))
     (implicitParams :underline
                     (:color "#c0c0c0"))
     (operator :foreground "#000080")
     (param :foreground "#000000")
     (class :foreground "#20999d")
     (trait :foreground "#20999d" :slant italic)
     (object :foreground "#5974ab" :slant italic)
     (package :foreground "#000000")
     (deprecated :strike-through "#000000")))
 '(fci-rule-color "#6272a4")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-theme-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '(("#3E3D31" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#3E3D31" . 100)))
 '(hl-paren-background-colors '("#2492db" "#95a5a6" nil))
 '(hl-paren-colors '("#ecf0f1" "#ecf0f1" "#c0392b"))
 '(hl-todo-keyword-faces
   '(("TODO" . "#ba29eb")
     ("NEXT" . "#ba29eb")
     ("THEM" . "#09b8be")
     ("PROG" . "#00bfa5")
     ("OKAY" . "#0082c9")
     ("DONT" . "#ff3d00")
     ("FAIL" . "#b0151a")
     ("DONE" . "#22a54e")
     ("NOTE" . "#ffb627")
     ("KLUDGE" . "#fb6107")
     ("HACK" . "#4d10a5")
     ("TEMP" . "#7a7b75")
     ("FIXME" . "#ba29eb")
     ("XXX" . "#0d47a1")
     ("XXXX" . "#0d47a1")
     ("\\?\\?\\?+" . "#0d47a1")))
 '(ibuffer-deletion-face 'modus-theme-mark-del)
 '(ibuffer-filter-group-name-face 'modus-theme-mark-symbol)
 '(ibuffer-marked-face 'modus-theme-mark-sel)
 '(ibuffer-title-face 'modus-theme-pseudo-header)
 '(jdee-db-active-breakpoint-face-colors (cons "#1E2029" "#bd93f9"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1E2029" "#50fa7b"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1E2029" "#565761"))
 '(magit-diff-use-overlays nil)
 '(objed-cursor-color "#ff5555")
 '(package-selected-packages
   '(monokai-pro-theme yaml-mode spacemacs-theme humanoid-themes modus-operandi-theme base16-theme kaolin-themes doom-themes intellij-theme all-the-icons-dired cider cider-eval-sexp-fu projectile-codesearch undo-tree tango-plus-theme tagedit smex rainbow-delimiters projectile parinfer paredit neotree monokai-theme magit flatui-theme exec-path-from-shell company clojure-mode-extra-font-locking cider-decompile all-the-icons ag))
 '(pdf-view-midnight-colors (cons "#f8f8f2" "#282a36"))
 '(pos-tip-background-color "#DFE1DC")
 '(pos-tip-foreground-color "#4b5254")
 '(rustic-ansi-faces
   ["#282a36" "#ff5555" "#50fa7b" "#f1fa8c" "#61bfff" "#ff79c6" "#8be9fd" "#f8f8f2"])
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(vc-annotate-background "#282a36")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (list
    (cons 20 "#50fa7b")
    (cons 40 "#85fa80")
    (cons 60 "#bbf986")
    (cons 80 "#f1fa8c")
    (cons 100 "#f5e381")
    (cons 120 "#face76")
    (cons 140 "#ffb86c")
    (cons 160 "#ffa38a")
    (cons 180 "#ff8ea8")
    (cons 200 "#ff79c6")
    (cons 220 "#ff6da0")
    (cons 240 "#ff617a")
    (cons 260 "#ff5555")
    (cons 280 "#d45558")
    (cons 300 "#aa565a")
    (cons 320 "#80565d")
    (cons 340 "#6272a4")
    (cons 360 "#6272a4")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3E3D31" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(xterm-color-names
   ["#000000" "#a60000" "#005e00" "#813e00" "#0031a9" "#721045" "#00538b" "#f0f0f0"])
 '(xterm-color-names-bright
   ["#505050" "#972500" "#315b00" "#70480f" "#2544bb" "#8f0075" "#30517f" "#ffffff"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
