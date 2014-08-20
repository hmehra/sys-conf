
;;; Emacs Configuration File

; Load Path
(add-to-list 'load-path "~/.myemacs/")

; Load Files
(require 'package)
(require 'uniquify)
(require 'xcscope)
(require 'protobuf-mode)
(require 'fill-column-indicator)
(require 'yang-mode)

; Package sources
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("ELPA" . "http://tromey.com/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))


; Editing Modes
(setq auto-mode-alist (cons '("\\.java$" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cxx$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.hxx$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.c$" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pl$" . perl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pm$" . perl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.txt$" . text-mode) auto-mode-alist))

; Remove initial splash-screen
(setq initial-scratch-message "")

; Don't display start message
(setq inhibit-startup-message t)

; Background
(custom-set-faces '(default ((t (:background "black" :foreground "grey"))))
                  '(fringe ((t (:background "black")))))

;Remove Scrollbar - Required for emacs gui client
;(scroll-bar-mode 0)

; Line Numbers
(custom-set-variables '(column-number-mode t) '(global-linum-mode t))

; Unique Names
(setq uniquify-buffer-name-style 'forward)

; Compile Shortcut
(define-key global-map [(control q)]  'compile)
(setq compile-command "cd ~/prototype-project/ && ./waf")


; Cscope for Emacs Install external package
(setq cscope-do-not-update-database t)
(setq cscope-display-cscope-buffer nil)
(define-key global-map [(meta shift f)]  'cscope-find-this-file)
(define-key global-map [(meta shift s)]  'cscope-find-this-symbol)
(define-key global-map [(meta shift g)]  'cscope-find-global-definition)
(define-key global-map [(meta shift n)]  'cscope-next-symbol)
(define-key global-map [(meta shift p)]  'cscope-prev-symbol)
(define-key global-map [(meta shift k)]  'cscope-next-file)
(define-key global-map [(meta shift j)]  'cscope-prev-file)
(define-key global-map [(meta b)]        'cscope-display-buffer)
(define-key global-map [(meta shift b)]
  'cscope-find-global-definition-no-prompting)
(define-key global-map [(meta shift x)]
  'cscope-find-functions-calling-this-function)

; Untabify
(defun untab-all ()
  (untabify (point-min) (point-max))
   nil ) ; did not write buffer to disk

(defun add-write-contents-hooks-hook ()
  (add-hook 'write-contents-hooks 'untab-all nil   t ))

; Turn on extra whitespace highlight
(setq-default show-trailing-whitespace t)

; Stop auto indent
(setq c-basic-offset 4)
(turn-on-font-lock)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

; Cscope Indexer
(defun index-files ()
 "Cscope-Index Command"
(interactive)
(message "Building Database")
(shell-command "cd $HOME/bin && ./cscope-indexer")
(message  "Done"))

(define-key global-map [(meta shift c)] 'index-files)
(setq tags-table-list '("~/prototype-project"))


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

; Fill Column Indicator
(add-hook 'after-change-major-mode-hook 'fci-mode)
(setq fci-rule-color "darkblue")
(setq fci-rule-column 80)

; Save backup and autosave file in a seperate folder
(setq backup-directory-alist  `((".*" . ,"~/.emacsbackup/")))
(setq auto-save-file-name-transforms `((".*" ,"~/.emacsbackup/" t)))

; Roll out compilation buffer
(setq-default compilation-scroll-output t)

; Show full path of file in frame title
(setq-default frame-title-format
              '("%f" (dired-directory dired-directory "%b")))


; Show full file path in mode line
(setq-default mode-line-buffer-identification
              (list 'buffer-file-name
                    (propertized-buffer-identification "%12f")
                    (propertized-buffer-identification "%12b")))

(add-hook 'dired-mode-hook
          (lambda ()
            (setq mode-line-buffer-identification
                  '(:eval
                    (propertized-buffer-identification
                     (if (< (length default-directory) 17)
                         (concat default-directory
                                 (make-string (- 17 (length default-directory))
                                              ?\s))
                       default-directory))))))
(put 'downcase-region 'disabled nil)

; Change yes/no to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

; Auto revert-buffers
(global-auto-revert-mode 1)

; Highlight matching braces
(show-paren-mode 1)

; Don't ask for confirmation when loading large files
(setq large-file-warning-threshold nil)

; Display details about closing brace
(defadvice show-paren-function (after my-echo-paren-matching-line activate)
  "If a matching paren is off-screen, echo the matching line."
  (when (char-equal (char-syntax (char-before (point))) ?\))
    (let ((matching-text (blink-matching-open)))
      (when matching-text
        (message matching-text)))))
