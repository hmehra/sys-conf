(use-package which-key
             :ensure t
             :config
             (which-key-mode))

(use-package try
  :ensure t)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default))

(defun generate-tags ()
  "Generate Gtags and protocol documentation."
  (interactive)
  (async-shell-command "~/bin/tags"))
(global-set-key (kbd "C-c g g") 'generate-tags)

(use-package swiper-helm
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper-isearch))

(global-hl-line-mode)

(defun load-init-file ()
  "Load myinit.org for editing"
  (interactive)
  (find-file "~/.emacs.d/myinit.org"))

(global-set-key (kbd "C-c i") 'load-init-file)

;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c m s") 'magit-status)
  (global-set-key (kbd "C-c m d") 'magit-diff-dwim)
  (global-set-key (kbd "C-c m p") 'magit-pull-from-upstream)
  (global-set-key (kbd "C-c m l") 'magit-log-all)
  (global-set-key (kbd "C-c m b") 'magit-blame))

(defun cot-upload ()
  "Uploads the current changeset to Gerrit."
  (interactive)
  (async-shell-command "cot upload"))

(global-set-key (kbd "C-c m u") 'cot-upload)

(load-theme 'monokai t)

(global-display-line-numbers-mode 1)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (executable-find "python3") 3 0)
	  treemacs-deferred-git-apply-delay      0.5
	  treemacs-display-in-side-window        t
	  treemacs-eldoc-display                 t
	  treemacs-file-event-delay              5000
	  treemacs-file-follow-delay             0.2
	  treemacs-follow-after-init             t
	  treemacs-git-command-pipe              ""
	  treemacs-goto-tag-strategy             'refetch-index
	  treemacs-indentation                   2
	  treemacs-indentation-string            " "
	  treemacs-is-never-other-window         nil
	  treemacs-max-git-entries               5000
	  treemacs-missing-project-action        'ask
	  treemacs-no-png-images                 nil
	  treemacs-no-delete-other-windows       t
	  treemacs-project-follow-cleanup        nil
	  treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
	  treemacs-recenter-distance             0.1
	  treemacs-recenter-after-file-follow    nil
	  treemacs-recenter-after-tag-follow     nil
	  treemacs-recenter-after-project-jump   'always
	  treemacs-recenter-after-project-expand 'on-distance
	  treemacs-show-cursor                   nil
	  treemacs-show-hidden-files             t
	  treemacs-silent-filewatch              nil
	  treemacs-silent-refresh                nil
	  treemacs-sorting                       'alphabetic-desc
	  treemacs-space-between-root-nodes      t
	  treemacs-tag-follow-cleanup            t
	  treemacs-tag-follow-delay              1.5
	  treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
		 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
	("M-0"       . treemacs-select-window)
	("C-x t 1"   . treemacs-delete-other-windows)
	("C-x t t"   . treemacs)
	("C-x t B"   . treemacs-bookmark)
	("C-x t C-t" . treemacs-find-file)
	("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode))

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -m json.tool" (current-buffer) t)))

;; Load files
(require 'cc-mode)
(require 'helm)
(require 'yaml-mode)
(require 'cl-lib)
(require 'uniquify)
(require 'fill-column-indicator)
(require 'xcscope)

; Editing Modes
(setq auto-mode-alist (cons '("\\.c$"       . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cxx$"     . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.hxx$"     . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.java$"    . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pl$"      . perl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pm$"      . perl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.txt$"     . text-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.py$"      . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.json\\'"  . js-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.yml\\'"   . yaml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.proto\\'" . protobuf-mode) auto-mode-alist))

; Remove initial splash-screen
(setq initial-scratch-message "")

; Don't display start message
(setq inhibit-startup-message t)

; Unique Names
(setq uniquify-buffer-name-style 'forward)

;; Fci Mode
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda ()
    (if buffer-file-name (fci-mode 1))))
(global-fci-mode 1)
(setq fci-rule-width 1)
(setq fci-rule-column 79)
(setq fci-handle-truncate-lines nil)
(setq fci-rule-color "red")
(global-whitespace-mode 1)
(setq whitespace-style '(face trailing))

; Highlight Mode
(global-hi-lock-mode 1)

; Compile Shortcuts
(define-key global-map [(control meta c)]  'compile)
(define-key global-map [(control k)]  'kill-whole-line)
(setq shell-file-name "bash")
(setq shell-command-switch "-lc")

;; Cscope for Emacs Install external package
(cscope-setup)
(setq cscope-do-not-update-database t)
(setq cscope-display-cscope-buffer nil)
(define-key global-map [(meta shift d)]  'cscope-set-initial-directory)
(define-key global-map [(meta shift f)]  'cscope-find-this-file)
(define-key global-map [(meta shift s)]  'cscope-find-this-symbol)
(define-key global-map [(meta shift g)]  'cscope-find-global-definition)
(define-key global-map [(meta shift n)]  'cscope-next-symbol)
(define-key global-map [(meta shift p)]  'cscope-prev-symbol)
(define-key global-map [(meta shift c)]  'cscope-display-buffer)
(define-key global-map [(meta shift b)]
  'cscope-find-global-definition-no-prompting)
(define-key global-map [(meta shift x)]
  'cscope-find-functions-calling-this-function)

; Turn on extra whitespace highlight
(setq-default show-trailing-whitespace t)

; Toggle window dedication
(defun toggle-window-dedicated ()
"Toggle whether the current active window is dedicated or not"
(interactive)
(message
 (if (let (window (get-buffer-window (current-buffer)))
       (set-window-dedicated-p window
                               (not (window-dedicated-p window))))
     "Window '%s' is dedicated"
   "Window '%s' is normal")
 (current-buffer)))

(define-key global-map [(meta shift l)] 'toggle-window-dedicated)

; Save backup and autosave file in a seperate folder
(setq backup-directory-alist  `((".*" . ,"~/.emacsbackup/")))
(setq auto-save-file-name-transforms `((".*" ,"~/.emacsbackup/" t)))

; Roll out compilation buffer
(setq-default compilation-scroll-output t)

; Show full path of file in frame title
(setq-default frame-title-format
              '("%f" (dired-directory dired-directory "%b")))

; Change yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

; Auto revert-buffers
(global-auto-revert-mode 1)

; Don't ask for confirmation when loading large files
(setq large-file-warning-threshold nil)

; Highlight matching braces
(show-paren-mode 1)

;; Display details about closing brace
(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
    echo area. Has no effect if the character before point is not of
    the syntax class ')'."
  (interactive)
  (let* (
         (cb (char-before (point)))
         (matching-text (and cb
                             (char-equal (char-syntax cb) ?\) )
                             (blink-matching-open)
                             )
                        )
         )
    )
  )

; Enable word wrap
(global-visual-line-mode 1)

; Remove menu bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

; Represent space by .
(setq whitespace-display-mappings '((space-mark ?\  [?.])
                                    (newline-mark ?\n [?$ ?\n])
                                    (tab-mark ?\t [?\\ ?\t])))

; Column Number Modes
(setq column-number-mode t)

;; Set symbol for the border
(set-face-inverse-video-p 'vertical-border nil)
(set-face-background 'vertical-border (face-background 'default))
(set-display-table-slot standard-display-table
                       'vertical-border
                       (make-glyph-code ?|))

;; Set face for highlight regex color
(setq hi-lock-auto-select-face t)

; Linum Mode
(setq linum-format (lambda
                     (line)
                     (propertize
                      (format (concat "%"
                                      (number-to-string
                                       (length
                                        (number-to-string
                                         (line-number-at-pos
                                          (point-max)))))
                                      "d ")
                              line)
                      'face
                      'linum)))

;; Speedbar
(global-set-key (kbd "<f6>") 'sr-speedbar-toggle)

; Move between emacs buffer
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

; Kill Yank Paste. Suck it VIM
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Copied line") (list (line-beginning-position) (line-beginning-position
  2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

; Make #if 0 comment face
(defun my-c-mode-font-lock-if0 (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)

(defun my-c-mode-common-hook ()
  (font-lock-add-keywords
   nil
   '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

; Ansi-term Shortcuts
(global-set-key [f1] 'ansi-term)

; Kill term buffer on exit
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

; Make bash default shell
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

; Make UTF-8 in term
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)

; Yank and paste in term
;; Just make the terminal text mode based using C-x C-j

; Make urls clickable in term
; Solarized for term
(defun my-term-hook ()
  (goto-address-mode)
  (define-key term-raw-map "\C-y" 'my-term-paste)
  (let ((base03  "#002b36")
        (base02  "#073642")
        (base01  "#586e75")
        (base00  "#657b83")
        (base0   "#839496")
        (base1   "#93a1a1")
        (base2   "#eee8d5")
        (base3   "#fdf6e3")
        (yellow  "#b58900")
        (orange  "#cb4b16")
        (red     "#dc322f")
        (magenta "#d33682")
        (violet  "#6c71c4")
        (blue    "#268bd2")
        (cyan    "#2aa198")
        (green   "#859900"))
    (setq ansi-term-color-vector
          (vconcat `(unspecified ,base02 ,red ,green ,yellow ,blue
                                  ,magenta ,cyan ,base2)))))

; Add Hook to term
; (add-hook 'term-mode-hook 'my-term-hook)

; Compile color
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(transient-mark-mode t)
(global-font-lock-mode t)
(column-number-mode t)
(setq blink-cursor-mode t)
(setq indicate-empty-lines t)

;; imenu - Jump to definition in same File
(global-set-key (kbd "C-c d") 'imenu)

;; No confirmation on revert
(setq revert-without-query '(".*"))

; Keep search highlighted always
(setq lazy-highlight-cleanup nil)
(setq isearch-allow-scroll t)

; Smooth scrolling
(setq scroll-step 1)

; Collapse/Expand functions
(defun hs-enable-and-toggle ()
  (interactive)
  (hs-minor-mode 1)
  (hs-toggle-hiding))

(defun hs-enable-and-hideshow-all (&optional arg)
  "Hide all blocks. If prefix argument is given, show all blocks."
  (interactive "P")
  (hs-minor-mode 1)
  (if arg
      (hs-show-all)
      (hs-hide-all)))

(global-set-key (kbd "C-c <up>")    'hs-enable-and-toggle)
(global-set-key (kbd "C-c <down>")  'hs-enable-and-hideshow-all)

; Clang formatter
(setq clang-format-executable
      "/home/cohesity/workspace/toolchain/x86_64-linux/llvm-latest/bin/clang-format")
(fset 'c-indent-region 'clang-format-region)
(global-set-key (kbd "C-c TAB") 'clang-format-region)

;; Better mouse
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

; Tramp use ssh
(setq tramp-default-method "ssh")

;; COT upload
(defun cot-upload ()
  "Uploads the current changeset to Gerrit."
  (interactive)
  (async-shell-command "cot upload"))

;; Change window sizes
(global-set-key (kbd "<C-S-up>") 'shrink-window)
(global-set-key (kbd "<C-S-down>") 'enlarge-window)
(global-set-key (kbd "<C-S-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-S-right>") 'enlarge-window-horizontally)

;; Helm config
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-autoresize-mode 1)
(setq helm-split-window-in-side-p t)
(helm-mode 1)

;; Helm at bottom of the screen
(add-to-list 'display-buffer-alist
                    `(,(rx bos "*helm" (* not-newline) "*" eos)
                         (display-buffer-in-side-window)
                         (inhibit-same-window . t)
                         (window-height . 0.4)))
;; Helm Gtags
(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'helm)

;; Add spell check to comments
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)

;; Dumb jump
(dumb-jump-mode)

;; Function name on top
(semantic-mode 1)
