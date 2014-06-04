
;;; Emacs Configuration File

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
(setq fci-rule-color "darkblue")
(setq fci-rule-column 80)
(setq-default tab-width 4)

(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #autosave# files
(scroll-bar-mode 0) ; Remove scrollbar

(setq-default indent-tabs-mode nil) ;Stop indentation by tab 

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
(load-file "/etc/emacs/xcscope.el")
(require 'xcscope)

(define-key global-map [(shift f)]  'cscope-find-this-file)
(define-key global-map [(shift s)]  'cscope-find-this-symbol)
(define-key global-map [(shift g)]  'cscope-find-global-definition)
(define-key global-map [(shift b)]  'cscope-find-global-definition-no-prompting)


; Yang Module
(load-file "~/.myemacs/yang-mode.el")











; Untabify
(defun untab-all ()
  (untabify (point-min) (point-max))
   nil ) ; did not write buffer to disk

(defun add-write-contents-hooks-hook ()
  (add-hook 'write-contents-hooks 'untab-all nil   t )) 
 
