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
;(setq url-proxy-services '(("http" . "usncwsa.diebold.com:8080")
;			   ("https" . "usncwsa.diebold.com:8080")))

;; Auto load packages thatnn are missing
(defvar autoload-packages
  '(autopair
    auto-complete
    yasnippet        ; snippet generator
    org-journal      ; org-mode alteration for notes
    evil             ; vim emulation layer for emacs
    dts-mode        ; device tree syntax 
    flycheck         ; on the fly syntax extension checking
    mellow-theme
    magit
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
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-journal-mode))
(add-to-list 'auto-mode-alist '("[0-9]+" . org-journal-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes
   (quote
    ("28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "fc0c179ce77997ecb6a7833310587131f319006ef2f630c5a1fec1a9307bff45" default)))
 '(org-agenda-files
   (quote
    ("/home/blockb/org/work.org" "/home/blockb/org/journal/20150529" "/home/blockb/org/journal/20150605" "/home/blockb/org/journal/20150606" "/home/blockb/org/journal/20150609" "/home/blockb/org/journal/20150610" "/home/blockb/org/journal/20151020" "/home/blockb/org/journal/20151125" "/home/blockb/org/journal/20160104" "/home/blockb/org/journal/20160111" "/home/blockb/org/journal/20160112" "/home/blockb/org/journal/20160119" "/home/blockb/org/journal/20160120" "/home/blockb/org/journal/20160122"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
