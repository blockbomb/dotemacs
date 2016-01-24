;;package managment
(require 'package)
(add-to-list 'package-archives
	     '("mepla" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives
	       '("gnu" . "http://epla.gnu.org/packages/")))
;(setq url-proxy-services '(("http" . "usncwsa.diebold.com:8080")
;			   ("https" . "usncwsa.diebold.com:8080")))
(package-initialize)

;; General Editing 
(require 'dts-mode)
(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap 1)

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
 '(custom-enabled-themes (quote (leuven)))
 '(custom-safe-themes
   (quote
    ("fc0c179ce77997ecb6a7833310587131f319006ef2f630c5a1fec1a9307bff45" default)))
 '(org-agenda-files
   (quote
    ("/home/blockb/org/work.org" "/home/blockb/org/journal/20150529" "/home/blockb/org/journal/20150605" "/home/blockb/org/journal/20150606" "/home/blockb/org/journal/20150609" "/home/blockb/org/journal/20150610" "/home/blockb/org/journal/20151020" "/home/blockb/org/journal/20151125" "/home/blockb/org/journal/20160104" "/home/blockb/org/journal/20160111" "/home/blockb/org/journal/20160112" "/home/blockb/org/journal/20160119" "/home/blockb/org/journal/20160120" "/home/blockb/org/journal/20160122"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

