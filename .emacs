; .emacs -*- mode: Emacs-Lisp;-*-

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(setq visible-bell nil)
;(setq ring-bell-function 'ignore)
(setq ring-bell-function
      (lambda ()
	(invert-face 'mode-line)
	(run-with-timer 0.1 nil 'invert-face 'mode-line)))

(setq inhibit-startup-message t)

;(cua-mode t)
;(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
;(transient-mark-mode 1) ;; No region when it is not highlighted
;(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

(setq indent-tabs-mode nil) 
(setq auto-mode-alist (mapcar 'purecopy
			      '(("\\.rb$" . ruby-mode)
				("\\.c$" . c++-mode)
				("\\.h$" . c++-mode)
				("\\.cc$" . c++-mode)
				("\\.cpp$" . c++-mode)
				("\\.hpp$" . c++-mode)
				("\\.lsp$" . lisp-mode)
				("\\.el$" . lisp-mode)
				("\\.java$" . java-mode))))

(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (setq ruby-indent-level 2)))

(add-hook 'c++-mode-hook
	  '(lambda ()
	     (setq c-basic-offset 4
		   c-comment-only-line-offset 0)
	     (c-set-offset 'innamespace [4])
	     (c-set-offset 'statement-block-intro '+)
	     (c-set-offset 'substatement-open 0)
	     (c-set-offset 'label 0)
	     (c-set-offset 'statement-case-open '+)
	     (c-set-offset 'statement-cont '+)))

