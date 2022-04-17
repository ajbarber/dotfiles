;; load emacs 24's package system. Add MELPA repository.

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
   ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "https://melpa.org/packages/")
   ;;'("melpa" . "http://melpa.milkbox.net/packages/")
   t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(beacon-color "#c82829")
 '(column-number-mode t)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   '("dd98976d2e8c2f92e52951b2c6472c840a2117db3dad3a9cfa7332596238ab9f" "fd1dd4d022ece05400c7bd1efc2ae5cca5cd64a53f3670da49d0c8f0ef41f4e3" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" "c9f102cf31165896631747fd20a0ca0b9c64ecae019ce5c2786713a5b7d6315e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "f2755fc8f0b4269cc45032715b8e11ea2d768aae47b8bb2a256ca1c8fdeb3628" "9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "2cfc1cab46c0f5bae8017d3603ea1197be4f4fff8b9750d026d19f0b9e606fae" "3448e3f5d01b39ce75962328a5310438e4a19e76e4b691c21c8e04ca318a5f62" "66881e95c0eda61d34aa7f08ebacf03319d37fe202d68ecf6a1dbfd49d664bc3" default))
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(eglot-confirm-server-initiated-edits nil)
 '(emms-mode-line-icon-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };"))
 '(fci-rule-color "#f6f0e1")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(frame-background-mode 'light)
 '(gnus-logo-colors '("#0d7b72" "#adadad") t)
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
F\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };") t)
 '(haskell-compile-command "ghc -dynamic  -Wall -ferror-spans -fforce-recomp -c %s")
 '(ivy-mode t)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(package-selected-packages
   '(haskell-mode lsp-haskell lsp-ivy lsp-mode cfn-mode vs-light-theme tommyh-theme fix-word auctex string-inflection pos-tip elpy psc-ide kaolin-themes zenburn-theme spacegray-theme purescript-mode yaml-mode evil wgrep god-mode flycheck-pycheckers blacken auto-complete popup-complete py-autopep8 flycheck-pyflakes flymake-diagnostic-at-point eslint-fix eslintd-fix js-doc tide projectile helm-ag ag all-the-icons leuven-theme solarized-theme alect-themes color-theme-solarized github-modern-theme haml-mode helm-css-scss magit liso-theme forest-blue-theme neotree company-ycmd ycmd helm rjsx-mode company-tern tern web-mode))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(popup-complete-enabled-modes '(reason-mode))
 '(purescript-mode-hook '(turn-on-eldoc-mode turn-on-purescript-indentation))
 '(recentf-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#f6f0e1")
 '(vc-annotate-color-map
   '((20 . "#e43838")
     (40 . "#f71010")
     (60 . "#ab9c3a")
     (80 . "#9ca30b")
     (100 . "#ef8300")
     (120 . "#958323")
     (140 . "#1c9e28")
     (160 . "#3cb368")
     (180 . "#028902")
     (200 . "#008b45")
     (220 . "#077707")
     (240 . "#259ea2")
     (260 . "#358d8d")
     (280 . "#0eaeae")
     (300 . "#2c53ca")
     (320 . "#1111ff")
     (340 . "#2020cc")
     (360 . "#a020f0")))
 '(vc-annotate-very-old-color "#a020f0"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 128 :width normal)))))

;; paths

;; remove paths for now as they seem to interfere with eachother

 ;; (setenv "PATH"
 ;;   (concat
 ;;    "/home/adam/usage_tracking/usage_tracking/bin" ":"
 ;;    "/home/adam/malm/node_modules/.bin" ":"
 ;;    "/home/adam/malm-maps/node_modules/.bin" ":"
 ;;    "/home/adam/zoidberg/node_modules/.bin" ":"
 ;;    (getenv "PATH")
 ;;   )
 ;; )

(global-linum-mode 1)

;; whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;save all
(defun save-all () (interactive) (save-some-buffers t))
(global-set-key (kbd "C-x s") 'save-all)

;; temp files

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

;;(auto-save-visited-mode 1)
;; completion

;; matching parentheses
(show-paren-mode 1)

;;completion
;;(global-set-key (kbd "TAB") 'completion-at-point)
;;
(global-syntax-subword-mode 1)

;; ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
;;(setq enable-recursive-minibuffers t)

(defun ivy-with-thing-at-point (cmd)
      (let ((ivy-initial-inputs-alist
             (list
              (cons cmd (thing-at-point 'symbol)))))
        (funcall cmd)))

(defun counsel-ag-thing-at-point ()
      (interactive)
      (ivy-with-thing-at-point 'counsel-ag))

;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-x o") 'other-window)
(global-set-key (kbd "C-z") 'counsel-ag-thing-at-point)
(global-set-key (kbd "C-c b") 'counsel-projectile-switch-to-buffer)
(global-set-key (kbd "<f2>") 'counsel-projectile-find-file)
(global-set-key (kbd "<f3>") 'magit-status)
(global-set-key (kbd "<f4>") 'counsel-switch-buffer-other-window)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'flymake-show-diagnostics-buffer)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "<f1>") 'counsel-projectile-switch-project)
(global-set-key (kbd "<f5> f") 'counsel-describef-function)
(global-set-key (kbd "<f5> v") 'counsel-describe-variable)
(global-set-key (kbd "<f5> l") 'counsel-find-library)
(global-set-key (kbd "<f5> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f5> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "<escape> k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-x f") 'counsel-find-file)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "M-u") 'fix-word-capitalize)
(global-set-key (kbd "C-u") 'fix-word-upcase)
(define-key key-translation-map (kbd "<C-escape>") (kbd "C-g"))
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(tool-bar-mode -1)

(global-set-key (kbd "<escape> <up>") 'windmove-up)
(global-set-key (kbd "<escape> <down>") 'windmove-down)
(global-set-key (kbd "<escape> <left>") 'windmove-left)
(global-set-key (kbd "<escape> <right>") 'windmove-right)

;; plugins
(add-to-list 'load-path "~/.emacs.d/plugins")
(load "plugins.el")

;; flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;flycheck popup
(with-eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;;customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

(flycheck-add-mode 'javascript-eslint 'web-mode)

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

;;use eslint with web-mode for jsx files

;;disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
 (append flycheck-disabled-checkers
        '(json-jsonlist)))

;; webmode
(require 'web-mode)

;; Use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))

;; Force 'content-type' as 'jsx' for .js and .jsx files
;; See here: http://cha1tanya.com/2015/06/20/configuring-web-mode-with-jsx.html
(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))

(setq-default indent-tabs-mode nil)

;; css mode
(add-hook 'css-mode-hook
          (lambda()
            (setq css-indent-offset 2)
            (setq indent-tabs-mode nil)))

;; eslint-fix
(require 'eslint-fix)
(eval-after-load 'web-mode
	   '(add-hook 'web-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))


;; Adjust indents for web-mode
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
)
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; autocompletion
(require 'company)

(setq company-tooltip-align-annotations t)

(add-to-list 'company-backends 'company-css)
(add-hook 'scss-mode-hook (lambda()
                            (company-mode)))

;; projectile
(require 'projectile)
(projectile-mode)

;; turn off autosave
;; (setq make-backup-files nil)


;; makes a really nice minibuffer menu for merging
(use-package smerge-mode
  :after hydra
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))

;; show cursor position within line
(column-number-mode 1)

;;----------------------------------------------------------------------------
;; Reason setup
;;----------------------------------------------------------------------------

;;(rassq-delete-all 'reason-mode-indent-line auto-mode-alist)

(defun shell-cmd (cmd)
  "Returns the stdout output of a shell command or nil if the command returned
   an error"
  (car (ignore-errors (apply 'process-lines (split-string cmd)))))

(let* ((refmt-bin (shell-cmd "which prettier")))
  ;; Add merlin.el to the emacs load path and tell emacs where to find ocamlmerlin

(setq refmt-bin 'npm)

(when refmt-bin
   (setq refmt-command refmt-bin)))

(require 'reason-mode)

(add-hook 'reason-mode-hook (lambda ()
                              (add-hook 'before-save-hook #'refmt-before-save)
                              ))

;; (require 'eglot)

;; (add-hook 'reason-mode-hook 'eglot-ensure)
;; (add-hook 'reason-mode-hook 'company-mode)

;; trailing spaces
;; (add-hook 'before-save-hook
;;           'delete-trailing-whitespace)

;;eshell

(add-hook 'eshell-mode-hook (lambda () (text-scale-decrease 1)))

(put 'dired-find-alternate-file 'disabled nil)


;; python

(add-hook 'python-mode-hook 'my/python-mode-hook)
;; (add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook
            (lambda()
              (company-mode)))


;;purescript

;; (require 'psc-ide)

;; (add-to-list 'eglot-server-programs
;;              '(reason-mode . ("/usr/bin/reason-language-server")))

;; (add-to-list 'eglot-server-programs
;;              '(purescript-mode  . ("/home/adam/.nvm/versions/node/v14.15.1/bin/purescript-language-server" "--stdio"
;;                                    "--config {\"purescript.buildCommand\": \"npx spago build --purs-args --json-errors\"}" )))

;; (add-hook 'purescript-mode-hook 'eglot-ensure)

;; optimisation for lsp
(setq gc-cons-threshold 100000000)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(add-hook 'purescript-mode-hook
          (lambda ()
            (flycheck-mode)
            (lsp)
            (turn-on-purescript-indentation)))

;; LaTeX

(defun my-LaTeX-mode()
  (add-to-list 'TeX-view-program-list '("firefox" "firefox %o"))
  (setq TeX-view-program-selection '((output-pdf "firefox")))
  ; Other mode specific config
  )
(add-hook 'LaTeX-mode-hook 'my-LaTeX-mode)


;; (setq psc-ide-use-npm-bin t)

;; python

;; (add-hook 'purescript-mode-hook 'eglot-ensure)
;; (add-hook 'purescript-mode-hook
;;              (lambda()
;;                (company-mode)))


;; trailing blanks
;; (setq-default show-trailing-whitespace 't)
;; (setq-default indicate-empty-lines 't)


;; rsi hacks
;; (require 'god-mode)
;; (god-mode)
;; (global-set-key (kbd "<escape>") #'god-local-mode)



;;autosave

;(setq backup-directory-alist `(("." . "~/.saves")))

;; cloudformation

;; Set up a mode for JSON based templates

(define-derived-mode cfn-json-mode js-mode
    "CFN-JSON"
    "Simple mode to edit CloudFormation template in JSON format."
    (setq js-indent-level 2))

(add-to-list 'magic-mode-alist
             '("\\({\n *\\)? *[\"']AWSTemplateFormatVersion" . cfn-json-mode))

;; Set up a mode for YAML based templates if yaml-mode is installed
;; Get yaml-mode here https://github.com/yoshiki/yaml-mode
(when (featurep 'yaml-mode)

  (define-derived-mode cfn-yaml-mode yaml-mode
    "CFN-YAML"
    "Simple mode to edit CloudFormation template in YAML format.")

  (add-to-list 'magic-mode-alist
               '("\\(---\n\\)?AWSTemplateFormatVersion:" . cfn-yaml-mode)))

;; Set up cfn-lint integration if flycheck is installed
;; Get flycheck here https://www.flycheck.org/
(when (featurep 'flycheck)
  (flycheck-define-checker cfn-lint
    "AWS CloudFormation linter using cfn-lint.

Install cfn-lint first: pip install cfn-lint

See `https://github.com/aws-cloudformation/cfn-python-lint'."

    :command ("cfn-lint" "-f" "parseable" source)
    :error-patterns ((warning line-start (file-name) ":" line ":" column
                              ":" (one-or-more digit) ":" (one-or-more digit) ":"
                              (id "W" (one-or-more digit)) ":" (message) line-end)
                     (error line-start (file-name) ":" line ":" column
                            ":" (one-or-more digit) ":" (one-or-more digit) ":"
                            (id "E" (one-or-more digit)) ":" (message) line-end))
    :modes (cfn-json-mode cfn-yaml-mode))

  (add-to-list 'flycheck-checkers 'cfn-lint)
  (add-hook 'cfn-json-mode-hook 'flycheck-mode)
  (add-hook 'cfn-yaml-mode-hook 'flycheck-mode))


;;haskell

(require 'lsp)
(require 'lsp-haskell)
;; Hooks so haskell and literate haskell major modes trigger LSP setup
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)
