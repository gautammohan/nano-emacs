;; -*- lexical-binding: t -*-
;; -------------------------------------------------------------------
;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020 - N Λ N O developers 
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>
;; -------------------------------------------------------------------
;;
;; Nano mode line format:
;;
;; [ status | name (primary)               secondary | item1 | item2 ]
;;
;; -------------------------------------------------------------------
(require 'subr-x)

;; -------------------------------------------------------------------
(defun vc-branch ()
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "#" (substring-no-properties vc-mode
                                 (+ (if (eq backend 'Hg) 2 3) 2))))  nil))

(defun nano-mode-name ()
  (if (listp mode-name) (car mode-name) mode-name))


;; From https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
;; ---------------------------------------------------------------------
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))

;; -------------------------------------------------------------------
(defun nano-modeline-compose (status name primary secondary)
  "Compose a string with provided information"
  (let* ((char-width    (window-font-width nil 'header-line))
         (window        (get-buffer-window (current-buffer)))
         (space-up       +0.15)
         (space-down     -0.20)
	 (prefix (propertize (concat (evil-generate-mode-line-tag evil-state) "| " (nano-mode-name) " ") 'face 'nano-face-header-faded))
         (left (concat
                (propertize " "  'face 'nano-face-header-default
			    'display `(raise ,space-up))
                (propertize name 'face 'nano-face-header-strong)
                (propertize " "  'face 'nano-face-header-default
			    'display `(raise ,space-down))
		(propertize primary 'face 'nano-face-header-default)))
         (right (concat secondary " "))
         (available-width (- (window-total-width) 
			     (length prefix) (length left) (length right)
			     (/ (window-right-divider-width) char-width)))
	 (available-width (max 1 available-width)))
    (concat prefix
	    left
	    (propertize (make-string available-width ?\ )
                        'face 'nano-face-header-default)
	    (propertize right 'face `(:inherit nano-face-header-default
                                      :foreground ,nano-color-faded)))))

;; ---------------------------------------------------------------------
(defun nano-modeline-calendar-mode-p ()
  (derived-mode-p 'calendar-mode))

(defun nano-modeline-calendar-mode () "")

;; Calendar (no header, only overline)
(with-eval-after-load 'calendar
  (defun calendar-setup-header ()
    (setq header-line-format "")
    (face-remap-add-relative
     'header-line `(:overline ,(face-foreground 'default)
                    :height 0.5
                    :background ,(face-background 'default))))
  (add-hook 'calendar-initial-window-hook #'calendar-setup-header)

  ;; From https://emacs.stackexchange.com/questions/45650
  (add-to-list 'display-buffer-alist
               `(,(rx string-start "*Calendar*" string-end)
                 (display-buffer-below-selected))))

;; ---------------------------------------------------------------------
(defun nano-modeline-org-capture-mode-p ()
  (bound-and-true-p org-capture-mode))

(defun nano-modeline-org-capture-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Capture"
                         "(org)"
                         ""))

(with-eval-after-load 'org-capture
  (defun org-capture-turn-off-header-line ()
    (setq-local header-line-format (default-value 'header-line-format))
    ;; (fit-window-to-buffer nil nil 8)
    ;; (face-remap-add-relative 'header-line '(:background "#ffffff"))
    (message nil))
  (add-hook 'org-capture-mode-hook
            #'org-capture-turn-off-header-line))

;; ---------------------------------------------------------------------
(setq Info-use-header-line nil)
(defun nano-modeline-info-breadcrumbs ()
  (let ((nodes (Info-toc-nodes Info-current-file))
        (cnode Info-current-node)
	(node Info-current-node)
        (crumbs ())
        (depth Info-breadcrumbs-depth)
	line)
    (while  (> depth 0)
      (setq node (nth 1 (assoc node nodes)))
      (if node (push node crumbs))
      (setq depth (1- depth)))
    (setq crumbs (cons "Top" (if (member (pop crumbs) '(nil "Top"))
			         crumbs (cons nil crumbs))))
    (forward-line 1)
    (dolist (node crumbs)
      (let ((text
	     (if (not (equal node "Top")) node
	       (format "%s"
		       (if (stringp Info-current-file)
			   (file-name-sans-extension
			    (file-name-nondirectory Info-current-file))
			 Info-current-file)))))
	(setq line (concat line (if (null line) "" " > ")
                                (if (null node) "..." text)))))
    (if (and cnode (not (equal cnode "Top")))
        (setq line (concat line (if (null line) "" " > ") cnode)))
    line))

(defun nano-modeline-info-mode-p ()
  (derived-mode-p 'Info-mode))

(defun nano-modeline-info-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Info"
                         (concat "("
                                 (nano-modeline-info-breadcrumbs)
                                 ")")
                         ""))

;; ---------------------------------------------------------------------
(defun nano-modeline-org-agenda-mode-p ()
  (derived-mode-p 'org-agenda-mode))

(defun nano-modeline-org-agenda-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Agenda"
                         ""
                         (format-time-string "%A %-e %B %Y")))

;; ---------------------------------------------------------------------
(defun nano-modeline-term-mode-p ()
  (derived-mode-p 'term-mode))

(defun nano-modeline-vterm-mode-p ()
  (derived-mode-p 'vterm-mode))

(defun nano-modeline-term-mode ()
  (nano-modeline-compose " >_ "
                         "Terminal"
                         (concat "(" shell-file-name ")")
                         (shorten-directory default-directory 32)))

;; ---------------------------------------------------------------------
(setq org-mode-line-string nil)
(with-eval-after-load 'org-clock
  (add-hook 'org-clock-out-hook
            '(lambda () (setq org-mode-line-string nil)
                        (force-mode-line-update))))

(defun nano-modeline-org-clock-mode-p ()
  org-mode-line-string)

(defun nano-modeline-org-clock-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (nano-mode-name))
          (branch      (vc-branch))
          (position    (format-mode-line "%l:%c")))
      (nano-modeline-compose (nano-modeline-status)
                             buffer-name 
                             (concat "(" mode-name
                                     (if branch (concat ", "
                                             (propertize branch 'face 'italic)))
                                     ")" )
                             org-mode-line-string)))

;; ---------------------------------------------------------------------
(defun nano-modeline-docview-mode-p ()
  (derived-mode-p 'doc-view-mode))

(defun nano-modeline-docview-mode ()
  (let ((buffer-name (format-mode-line "%b"))
	(mode-name   (nano-mode-name))
	(branch      (vc-branch))
	(page-number (concat
		      (number-to-string (doc-view-current-page)) "/"
		      (or (ignore-errors
			    (number-to-string (doc-view-last-page-number)))
			  "???"))))
    (nano-modeline-compose
     (nano-modeline-status)
     buffer-name
     (concat "(" mode-name
	     (if branch (concat ", "
				(propertize branch 'face 'italic)))
	     ")" )
     page-number)))

;; ---------------------------------------------------------------------
(defun nano-modeline-pdf-view-mode-p ()
  (derived-mode-p 'pdf-view-mode))

(defun nano-modeline-pdf-view-mode ()
  (let ((buffer-name (format-mode-line "%b"))
	(mode-name   (nano-mode-name))
	(branch      (vc-branch))
	(page-number (concat
		      (number-to-string (pdf-view-current-page)) "/"
		      (or (ignore-errors
			    (number-to-string (pdf-cache-number-of-pages)))
			  "???"))))
    (nano-modeline-compose
     "RW"
     buffer-name
     (concat "(" mode-name
	     (if branch (concat ", "
				(propertize branch 'face 'italic)))
	     ")" )
     page-number)))

;; ---------------------------------------------------------------------
(defun buffer-menu-mode-header-line ()
  (face-remap-add-relative
   'header-line `(:background ,(face-background 'nano-face-subtle))))
(add-hook 'Buffer-menu-mode-hook
          #'buffer-menu-mode-header-line)

;; ---------------------------------------------------------------------
(defun nano-modeline-completion-list-mode-p ()
  (derived-mode-p 'completion-list-mode))

(defun nano-modeline-completion-list-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (nano-mode-name))
          (position    (format-mode-line "%l:%c")))

      (nano-modeline-compose (nano-modeline-status)
                             buffer-name "" position)))

;; ---------------------------------------------------------------------
(defun nano-modeline-prog-mode-p ()
  (derived-mode-p 'prog-mode))

(defun nano-modeline-text-mode-p ()
  (derived-mode-p 'text-mode))

(defun nano-modeline-default-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (nano-mode-name))
          (branch      (vc-branch))
          (position    (format-mode-line "%l:%c")))
      (nano-modeline-compose (nano-modeline-status)
                             buffer-name
                             (concat "(" mode-name
                                     (if branch (concat ", "
                                            (propertize branch 'face 'italic)))
                                     ")" )
                             position)))
  
;; ---------------------------------------------------------------------
(defun nano-modeline ()
  "Install a header line whose content is dependend on the major mode"
  (interactive)
  (setq-default header-line-format
  '(:eval (nano-modeline-default-mode))))

;; ---------------------------------------------------------------------
(defun nano-modeline-update-windows ()
  "Modify the mode line depending on the presence of a window
below or a buffer local variable 'no-mode-line'."
  (dolist (window (window-list))
    (with-selected-window window
	  (with-current-buffer (window-buffer window)
        (if (or (not (boundp 'no-mode-line)) (not no-mode-line))
            (set-window-parameter window 'mode-line-format
                                  (cond ((not mode-line-format) 'none)
                                        ((one-window-p t 'visible) (list ""))
                                        ((eq (window-in-direction 'below) (minibuffer-window)) (list ""))
                                        ((not (window-in-direction 'below)) (list ""))
                                        (t 'none))))))))

(add-hook 'window-configuration-change-hook 'nano-modeline-update-windows)

(setq eshell-status-in-modeline nil)
;; (setq-default mode-line-format (list "%-"))
(setq-default mode-line-format "")
(nano-modeline)

(provide 'nano-modeline)



