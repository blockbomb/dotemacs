;;; init.el --- My Customization File for the EMACS experience
;;; Commentary:
;;; Change Log:
;;; Code:

;; Package managment
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("mepla" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives
	       '("gnu" . "http://epla.gnu.org/packages/")))
;;(setq url-proxy-services '(("http" . "usncwsa.diebold.com:8080")
;;			   ("https" . "usncwsa.diebold.com:8080")))

;; Auto load packages thatnn are missing
(defvar autoload-packages
  '(autopair
    auto-complete
    yasnippet			; snippet generator
    org-journal			; org-mode alteration for notes
    evil			; vim emulation layer for emacs
    dts-mode			; device tree syntax
    flycheck			; on the fly syntax extension checking
    mellow-theme		; theme that I like ;P
    magit			; Amazing git wrapper for emacs
    python-mode
    jedi			; python editor helper
    relative-line-numbers	; linenumbers are relative to point
    cython-mode			; editing mode for cython files
    json-mode			; editing mode for json files
    org-bullets			; prettier org mode headings
    auctex
    )
  "A list of packages to ensure are installed at launch.")

(defun autoload-packages-installed-p ()
  (cl-loop for p in autoload-packages
	   when (not (package-installed-p p)) do (cl-return nil)
	   finally (cl-return t)))

(unless (autoload-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p autoload-packages)
    (when (not (package-installed-p p))
      (package-install p))))
(provide 'autoload-package)

;; User packages
(add-to-list 'load-path "~/.emacs.d/elisp")

;; General Editing
(require 'dts-mode)

(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap 1)

(require 'auto-complete-config)
(ac-config-default)

(require 'yasnippet)
(yas-global-mode 1)
(global-flycheck-mode)

(require 'hideshow)
(define-key global-map "\C-\M-o" 'hs-toggle-hiding)
(define-key global-map "\C-\\" 'hs-toggle-selective-display)
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'python-mode-hook     'hs-minor-mode)

(defun hs-hide-leafs-recursive (minp maxp)
  "Hide blocks below point that do not contain further blocks in region (MINP MAXP)."
  (when (hs-find-block-beginning)
    (setq minp (1+ (point)))
    (funcall hs-forward-sexp-func 1)
    (setq maxp (1- (point))))
  (unless hs-allow-nesting
    (hs-discard-overlays minp maxp))
  (goto-char minp)
  (let ((leaf t))
    (while (progn
	     (forward-comment (buffer-size))
	     (and (< (point) maxp)
		  (re-search-forward hs-block-start-regexp maxp t)))
      (setq pos (match-beginning hs-block-start-mdata-select))
      (if (hs-hide-leafs-recursive minp maxp)
	  (save-excursion
	    (goto-char pos)
	    (hs-hide-block-at-point t)))
      (setq leaf nil))
    (goto-char maxp)
    leaf))

(defun hs-hide-leafs ()
  "Hide all blocks in the buffer that do not contain subordinate blocks.  The hook `hs-hide-hook` is run; see run-hooks."
  (interactive)
  (hs-life-goes-on
   (save-excursion
     (message "Hiding blocks ...")
     (save-excursion
       (goto-char (point-min))
       (hs-hide-leafs-recursive (point-min) (point-max)))
     (message "Hiding blocks ... done"))
   (run-hooks 'hs-hide-hook)))

;; Git integration
(require 'magit)
(define-key global-map "\C-xg" 'magit-status)
(define-key global-map "\C-x\M-g" 'magit-dispatch-popup)

(require 'sgml-mode)
(require 'nxml-mode)
(add-to-list 'hs-special-modes-alist
	     '(nxml-mode
	       "<!--\\|<[^/>]*[^/]>"
	       "-->\\|</[^/>]*[^/]>"

	       "<!--"
	       sgml-skip-tag-forward
	       nil))
(add-hook 'nxml-mode-hook 'hs-minor-mode)

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python". python-mode))

(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq python-shell-interpreter "/usr/bin/python3")

;; C-Programming Section
(setq-default c-basic-offset 4
	      c-default-style "linux")

;; Notes N' dat
(require 'org)
(setq org-journal-dir "~/org/journal")
(setq org-agenda-file-regexp "^.*\.org$\\|[0-9]+$")
(setq org-agenda-files '("~/org" "~/org/journal"))
(require 'org-journal)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-agenda-include-diary t)
(setq org-log-done t)
(add-to-list 'auto-mode-alist '(".+\.org$" . org-journal-mode))
(add-to-list 'auto-mode-alist '("[0-9]+$" . org-journal-mode))
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; load some tex processing
(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq org-latex-create-formula-image-program 'dvipng)



(desktop-save-mode 1)

;; Cusomize look and feel
(add-to-list 'default-frame-alist '(alpha 95 90))
(tool-bar-mode -1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "fc0c179ce77997ecb6a7833310587131f319006ef2f630c5a1fec1a9307bff45" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
