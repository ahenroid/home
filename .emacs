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

(setq auto-mode-alist (mapcar 'purecopy
			      '(("\\.rb$" . ruby-mode)
				("\\.c$" . c++-mode)
				("\\.h$" . c++-mode)
				("\\.cc$" . c++-mode)
				("\\.cpp$" . c++-mode)
				("\\.hpp$" . c++-mode)
				("\\.lsp$" . lisp-mode)
				("\\.el$" . lisp-mode)
				("CMakeLists\\.txt\\'" . cmake-mode)
				("\\.cmake\\'" . cmake-mode)
				("\\.java$" . java-mode))))

(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode nil) 
	     (setq ruby-indent-level 2)))

(defun c-lineup-lambda-arg (langelem)
  ;; line up a lambda argument
  (save-excursion
    (when (eq 'arglist-cont-nonempty
	      (caar (last (butlast c-syntactic-context))))
      -1)))

(defun c-lineup-lambda-close (langelem)
  ;; line up a lambda argument
  (save-excursion
    (when (eq 'arglist-cont-nonempty
	      (caar (last (butlast c-syntactic-context))))
      -5)))

(c-add-style "puppetlabs"
	     '("k&r"
	       (c-offsets-alist
		(innamespace . [4])
		(namespace-close . [0])
		(arglist-intro . +)
		(arglist-cont . +)
		(defun-block-intro . 4)
		(statement-block-intro . 4)
		(brace-list-intro . 4)
		(inclass . (add + -1))
		(inline-open . 0)
		(statement-block-intro . (c-lineup-lambda-arg 4))
		(block-close . (c-lineup-lambda-close 0))
		(access-label -4))))

(add-hook 'c++-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode nil) 
	     (setq c-basic-offset 4)
	     (setq c-default-style "puppetlabs")))

;(add-hook 'c++-mode-hook
;	  '(lambda ()
;	     (setq indent-tabs-mode nil) 
;	     (setq c-basic-offset 4)
;	     (setq c-comment-only-line-offset 0)
;	     (c-set-offset 'innamespace [4])
;	     (c-set-offset 'knr-argdecl-intro 0)
;	     (c-set-offset 'statement-block-intro +)
;	     (c-set-offset 'substatement-open 0)
;	     (c-set-offset 'substatement-label 0)
;	     (c-set-offset 'label 0)
;	     (c-set-offset 'statement-case-open '+)
;	     (c-set-offset 'statement-cont '+)))

(defun cmake-rename-buffer ()
  "Renames a CMakeLists.txt buffer to cmake-<directory name>."
  (interactive)
  (when (and (buffer-file-name) (string-match "CMakeLists.txt" (buffer-name)))
    (setq parent-dir
	  (file-name-nondirectory (directory-file-name
				   (file-name-directory (buffer-file-name)))))
    (setq new-buffer-name (concat "cmake-" parent-dir))
    (rename-buffer new-buffer-name t)))

(add-hook 'cmake-mode-hook (function cmake-rename-buffer))
