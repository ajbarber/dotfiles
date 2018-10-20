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
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-map (ansi-color-make-color-map) t)
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("fd1dd4d022ece05400c7bd1efc2ae5cca5cd64a53f3670da49d0c8f0ef41f4e3" "2df493c5c7f329eef362290abdcd42a45abad98ffe33f639ecc55af084224e8b" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "13d20048c12826c7ea636fbe513d6f24c0d43709a761052adbca052708798ce3" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "2cfc1cab46c0f5bae8017d3603ea1197be4f4fff8b9750d026d19f0b9e606fae" "3448e3f5d01b39ce75962328a5310438e4a19e76e4b691c21c8e04ca318a5f62" "66881e95c0eda61d34aa7f08ebacf03319d37fe202d68ecf6a1dbfd49d664bc3" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
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
\"#######..#\" };")))
 '(fci-rule-color "#f1c40f")
 '(gnus-logo-colors (quote ("#0d7b72" "#adadad")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"##################\",
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
\"###########.######\" };")) t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(hl-sexp-background-color "#efebe9")
 '(ibuffer-deletion-face (quote diredp-deletion-file-name))
 '(ibuffer-marked-face (quote diredp-flag-mark))
 '(ivy-mode t)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (lua-mode flatland-theme dakrone-theme zenburn-theme tommyh-theme soft-morning-theme paper-theme iodine-theme hydandata-light-theme soft-charcoal-theme monokai-theme espresso-theme moe-theme flatui-theme hemisu-theme smex wgrep counsel-projectile multiple-cursors expand-region real-auto-save vdiff flycheck-inline flycheck-color-mode-line react-snippets eslint-fix eslintd-fix js-doc flycheck-popup-tip tide projectile ag all-the-icons leuven-theme solarized-theme alect-themes color-theme-solarized github-modern-theme haml-mode magit liso-theme forest-blue-theme neotree company-ycmd ycmd rjsx-mode company-tern tern web-mode)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(projectile-mode t nil (projectile))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(vc-annotate-background "#ecf0f1")
 '(vc-annotate-color-map
   (quote
    ((30 . "#e74c3c")
     (60 . "#c0392b")
     (90 . "#e67e22")
     (120 . "#d35400")
     (150 . "#f1c40f")
     (180 . "#d98c10")
     (210 . "#2ecc71")
     (240 . "#27ae60")
     (270 . "#1abc9c")
     (300 . "#16a085")
     (330 . "#2492db")
     (360 . "#0a74b9"))))
 '(vc-annotate-very-old-color "#0a74b9")
 '(weechat-color-list
   (quote
    (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "SRC" :slant normal :weight normal :height 90 :width normal))))
 '(ediff-even-diff-A ((((class color) (min-colors 89)) (:background "#c6c6c6"))))
 '(ediff-even-diff-B ((((class color) (min-colors 89)) (:background "#c6c6c6"))))
 '(ediff-odd-diff-A ((((class color) (min-colors 89)) (:background "#ffafaf" :bold t))))
 '(ediff-odd-diff-B ((((class color) (min-colors 89)) (:background "#ffafaf" :bold t)))))

;; misc settings i like
(delete-selection-mode 1)
(setq ediff-split-window-function 'split-window-horizontally)
;;(setq helm-ag-insert-at-point 'symbol)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; plugins

(add-to-list 'load-path "~/.emacs.d/plugins")
(load "plugins.el")

;; expand region

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; mark multiple

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; global line numbers
(global-linum-mode t)


;; switch to minibuffer

(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))
(global-set-key (kbd "C-`") 'switch-to-minibuffer-window)

;;ivy

(defun ivy-with-thing-at-point (cmd)
  (let ((ivy-initial-inputs-alist
         (list
          (cons cmd (thing-at-point 'symbol)))))
    (funcall cmd)))

;; Example 1
(defun counsel-ag-thing-at-point ()
  (interactive)
  (ivy-with-thing-at-point 'counsel-ag))

;; Example 2
(defun swiper-thing-at-point ()
  (interactive)
  (ivy-with-thing-at-point 'swiper))

(ivy-mode 1)
;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
(setq ivy-use-virtual-buffers t)
;; number of result lines to display
(setq ivy-height 10)
;; does not count candidates
(setq ivy-count-format "")
;; no regexp by default
(setq ivy-initial-inputs-alist nil)
;; configure regexp engine.
(setq ivy-re-builders-alist
      ;; allow input not in order
      '((t   . ivy--regex-ignore-order)))

;; keys

;;(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "<f1>") 'ibuffer)
(global-set-key (kbd "<f2>") 'counsel-projectile)
(global-set-key (kbd "<f3>") 'counsel-projectile-switch-project)
(global-set-key (kbd "<f4>") 'counsel-recentf)

(global-set-key (kbd "C-s") 'swiper)
(setq ivy-display-style 'fancy)

(global-set-key (kbd "C-x .") 'counsel-ag-thing-at-point)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "<C-tab>") 'counsel-ag)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "<f6>") 'list-flycheck-errors)
(global-set-key (kbd "<f5>") 'tide-fix)
(global-set-key (kbd "<f7>") 'tide-organize-imports)
(global-set-key (kbd "C-x g") 'magit-status)

;;(setq enable-recursive-minibuffers t)
;; flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;flycheck popup
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))
;; (with-eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))


;; disable jshint since we prefer eslint checking 
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;;customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

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

(setq js-indent-level 2)
(setq sgml-basic-offset 0)
(setq js2-basic-offset 2)
(setq-local javascript-indent-level 2) 

;; Use rjsx-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))

;; Force 'content-type' as 'jsx' for .js and .jsx files
;; See here: http://cha1tanya.com/2015/06/20/configuring-web-mode-with-jsx.html
 (setq web-mode-content-types-alist
       '(("jsx" . "\\.js[x]?\\'")))

(setq-default indent-tabs-mode nil)
(setq web-mode-enable-auto-quoting nil)

;; css mode
(add-hook 'css-mode-hook
          (lambda()
            (setq css-indent-offset 2)
            (setq indent-tabs-mode nil)))

;; haml mode
(require 'haml-mode)

;; Adjust indents for web-mode
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
;;  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
;;  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
)
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; autocompletion
(require 'company)
;;(require 'company-etags)

;;(add-to-list 'company-backends 'company-etags)
;;(add-hook 'web-mode-hook (lambda ()
;;                          (tern-mode)
;;                          (company-mode)))

(setq company-tooltip-align-annotations t)

(add-to-list 'company-backends 'company-css)
(add-hook 'scss-mode-hook (lambda()
                            (company-mode)))

;; neotree
(add-to-list 'load-path "/home/adam/zoidberg")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;;(setq neo-autorefresh t)

(add-hook 'web-mode-hook 'eslintd-fix-mode)

(require 'tide)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  ;;(setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "js" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(setq-default flycheck-disabled-checkers
 (append flycheck-disabled-checkers
        '(tsx-tide)))

;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

;; insert quotes

(defun double-quote ()
  (interactive)
  (if (use-region-p)
      (save-excursion
        (let ((beginning (region-beginning))
              (end (+ (region-end) 1)))
          (goto-char beginning)
          (insert "'")
          (goto-char end)
          (insert "'")))
    (insert "''")
    (backward-char)))

(global-set-key (kbd "M-\'") 'double-quote)

(put 'dired-find-alternate-file 'disabled nil)
