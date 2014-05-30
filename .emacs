;;; Emacs Configuration File


;; Yang Module
(load-file "~/.myemacs/yang-mode.el")

;; Line Numbers
(custom-set-variables '(column-number-mode t)
                      '(global-linum-mode t))

;; Remove Initial splash-screen
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #autosave# files

;; Remove scrollbar
(scroll-bar-mode 0)

;; Background
(custom-set-faces
  '(default ((t (:background "black" :foreground "grey"))))
  '(fringe ((t (:background "black")))))
(setq fci-rule-color "darkblue")
(setq fci-rule-column 80)
(setq-default tab-width 4)


;Cscope for Emacs
(setq cscope-do-not-update-database t)
(load-file "/etc/emacs/xcscope.el")
(require 'xcscope)

;Cscope Key Bindings
(define-key global-map [(control f3)]  'cscope-set-initial-directory)
(define-key global-map [(control f4)]  'cscope-find-this-file)
(define-key global-map [(control f5)]  'cscope-find-this-symbol)
(define-key global-map [(control f6)]  'cscope-find-global-definition)
(define-key global-map [(control f7)]  'cscope-find-global-definition-no-prompting)
(define-key global-map [(control f8)]  'cscope-pop-mark)
(define-key global-map [(control f9)]  'cscope-next-symbol)
(define-key global-map [(control f10)] 'cscope-next-file)
(define-key global-map [(control f11)] 'cscope-prev-symbol)
(define-key global-map [(control f12)] 'cscope-prev-file)
(define-key global-map [(meta f9)]     'cscope-display-buffer)
(define-key global-map [(meta f10)]    'cscope-display-buffer-toggle)  

;;Stop indentation by tab 
(setq-default indent-tabs-mode nil)

;; Untabify
(defun untab-all ()
  (untabify (point-min) (point-max))
   nil ) ; did not write buffer to disk

(defun add-write-contents-hooks-hook ()
  (add-hook
   'write-contents-hooks
   'untab-all
     nil  ; APPEND  unrelated, explicit default nil as optional :)
     t )) ; LOCAL   non-nil => make hook local
 
