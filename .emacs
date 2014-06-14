
;;; Emacs Configuration File

; Load Path
(add-to-list 'load-path "~/.myemacs/")

; Load Files
(require 'uniquify)
(require 'xcscope)
(require 'protobuf-mode)
(require 'fill-column-indicator)
(require 'yang-mode)

; Editing Modes
(setq auto-mode-alist (cons '("\\.java$" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cxx$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.hxx$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.c$" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pl$" . perl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pm$" . perl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.txt$" . text-mode) auto-mode-alist))


(setq initial-scratch-message "") ; Remove initial splash-screen
(setq inhibit-startup-message t)  ; Don't display start message

; Background
(custom-set-faces '(default ((t (:background "black" :foreground "grey"))))
                  '(fringe ((t (:background "black")))))

;Remove Scrollbar
(scroll-bar-mode 0)

; Line Numbers
(custom-set-variables '(column-number-mode t) '(global-linum-mode t))

; Compile Shortcut
(define-key global-map [(control q)]  'compile)
(setq compile-command "cd ~/prototype-project/ && ./waf")


; Auto Complete Install external package
(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)


; Cscope for Emacs Install external package
(setq cscope-do-not-update-database t)

(define-key global-map [(meta shift f)]  'cscope-find-this-file)
(define-key global-map [(meta shift s)]  'cscope-find-this-symbol)
(define-key global-map [(meta shift g)]  'cscope-find-global-definition)
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
 (shell-command "cd $HOME/bin && cscope-indexer -r -v")
 (message  "Done"))
 (define-key global-map [(meta shift c)] 'index-files)


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

(define-key global-map [(meta shift l)] 'toggle-window-dedicate)

; Fill Column Indicator
(add-hook 'after-change-major-mode-hook 'fci-mode)
(setq fci-rule-color "darkblue")
(setq fci-rule-column 80)

; Save backup and autosave file in a seperate folder
(setq backup-directory-alist  `((".*" . ,"~/.emacsbackup/")))
(setq auto-save-file-name-transforms `((".*" ,"~/.emacsbackup/")))

; (Un)Comment selected region
(define-key global-map (kbd "C-c C-c") 'comment-dwim)