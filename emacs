;;; Emacs Configuration File
(setq user-full-name "Himanshu Mehra")

; Load Path
(add-to-list 'load-path "~/.myemacs/")
(add-to-list 'load-path "~/.myemacs/evil")
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/cl-lib")

; Load files
(require 'package)
(require 'cl-lib)
(require 'uniquify)
(require 'xcscope)
(require 'fill-column-indicator)
(require 'sr-speedbar)
(require 'evil)
(require 'idle-highlight-mode)

; Packages
(package-initialize)

; Editing Modes
(setq auto-mode-alist (cons '("\\.java$" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cxx$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.hxx$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.c$" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pl$" . perl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pm$" . perl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.txt$" . text-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.json\\'" . js-mode) auto-mode-alist))

; Remove initial splash-screen
(setq initial-scratch-message "")

; Don't display start message
(setq inhibit-startup-message t)

; Unique Names
(setq uniquify-buffer-name-style 'forward)

; Fci Mode
(add-hook 'after-change-major-mode-hook 'fci-mode)
(setq fci-rule-width 1)
(setq fci-rule-column 80)
(setq fci-rule-color "white")
(global-whitespace-mode 1)
(setq whitespace-style '(face trailing))

; Highlight Mode
(global-hi-lock-mode 1)

; Compile Shortcuts
(define-key global-map [(control meta c)]  'compile)
(setq shell-file-name "bash")
(setq shell-command-switch "-ic")

; Auto complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(global-auto-complete-mode t)

; Cscope for Emacs Install external package
(setq cscope-do-not-update-database t)
(setq cscope-display-cscope-buffer nil)
(define-key global-map [(meta shift d)]  'cscope-set-initial-directory)
(define-key global-map [(meta shift f)]  'cscope-find-this-file)
(define-key global-map [(meta shift s)]  'cscope-find-this-symbol)
(define-key global-map [(meta shift g)]  'cscope-find-global-definition)
(define-key global-map [(meta shift n)]  'cscope-next-symbol)
(define-key global-map [(meta shift p)]  'cscope-prev-symbol)
(define-key global-map [(meta c)]        'cscope-display-buffer)
(define-key global-map [(meta shift b)]
  'cscope-find-global-definition-no-prompting)
(define-key global-map [(meta shift x)]
  'cscope-find-functions-calling-this-function)

; Turn on extra whitespace highlight
(setq-default show-trailing-whitespace t)

; Stop auto indent
(setq c-basic-offset 4)
(turn-on-font-lock)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

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

; Display details about closing brace
(defadvice show-paren-function (after my-echo-paren-matching-line activate)
  "If a matching paren is off-screen, echo the matching line."
  (when (char-equal (char-syntax (char-before (point))) ?\))
    (let ((matching-text (blink-matching-open)))
      (when matching-text
        (message matching-text)))))

; Enable word wrap
(global-visual-line-mode 1)

; Remove menu bar
(menu-bar-mode -1)

; Represent space by .
(setq whitespace-display-mappings '((space-mark ?\  [?.])
                                    (newline-mark ?\n [?$ ?\n])
                                    (tab-mark ?\t [?\\ ?\t])))

; Make Comments Red Emacs 22 or below
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)
(set-face-foreground 'font-lock-comment-face "red")

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

; Speedbar
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
(defun my-term-paste (&amp ;optional string)
 (interactive)
 (process-send-string
  (get-buffer-process (current-buffer))
  (if string string (current-kill 0)))))

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
(add-hook 'term-mode-hook 'my-term-hook)

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

; Ido Modes
(require 'ido)
(ido-mode)
(setq ido-completion-buffer "*Ido Completions*")
(setq ido-completion-buffer-all-completions t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(icomplete-mode 99)

;; Cplink
(defun cplink-revert-buffer()
  "cplink current-buffer and revert it"
  (interactive)
  (call-process-shell-command (format "ww -copy %s" buffer-file-name))
  (revert-buffer buffer-file-name t)
  (message "cplinked file"))

(global-set-key (kbd "C-c r") 'cplink-revert-buffer)

;; imenu - Jump to definition in same File
(global-set-key (kbd "C-c d") 'imenu)

;; No confirmation on revert
(setq revert-without-query '(".*"))


(custom-set-variables)
(custom-set-faces
 '(isearch ((((class color) (min-colors 8)) (:background "yellow" :foreground "black"))))
 '(lazy-highlight ((((class color) (min-colors 8)) (:background "yellow" :foreground "black")))))

; Keep search highlighted always
(setq lazy-highlight-cleanup nil)

; Speedbar on left
(setq sr-speedbar-right-side nil)
