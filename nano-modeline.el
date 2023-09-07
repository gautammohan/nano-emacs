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
;; Customized Nano mode line format:
;;
;; [Evil Mode | Major Mode] buffer-name (project, vc-branch, remote-host) line:col
;; -------------------------------------------------------------------
(require 'subr-x)
(require 'evil)
(require 'seq)

;; -------------------------------------------------------------------
(defun nano-mode-name ()
  (if (listp mode-name) (car mode-name) mode-name))

;; RW / buffer or file / saved or unsaved. RW = faded vs normal
(defun nano-buffer-status ()
  "Computes the face of the buffer name. If a buffer contains
modifications its font width increases. If a buffer is read only,
it has a faded color."
  (propertize (buffer-name)
              'face `(:inherit ,(if (buffer-modified-p)
                                    'nano-face-header-strong
                                  'nano-face-header-default)
                               :foreground ,(if (and buffer-file-name
                                                     buffer-read-only)
                                                nano-color-faded
                                              nano-color-foreground))))
(defun nano-modeline-status ()
  "Return status: Popper status, Evil state (determines color), and Major mode"
  (let ((evil-tag (string-trim
                   (evil-generate-mode-line-tag evil-state)
                   "[<> ]+"
                   "[<> ]+"))
        (name (nano-mode-name))
        (popper-status (if popper-popup-status " POP |" "")))
    (propertize (concat popper-status " " evil-tag " | " name " ") 'face
                (cond ((string= evil-tag "G") 'nano-face-header-salient)
                      ((string= evil-tag "I") 'nano-face-header-popout)
                      (t                      'nano-face-header-faded)))))

(defun nano-modeline-context ()
  "Compute the following buffer information and display the ones that exist:
     - vc branch
     - tramp host path
     - project(ile) name
  "
  (let ((vc-context (substring-no-properties (string-trim-left (if vc-mode vc-mode "") "[[:alnum:] ]+:")))
        (tramp-host (unless (file-remote-p default-directory) ""))
        (project-name (replace-regexp-in-string "^-$" "" (projectile-project-name))))
    (concat "(" (mapconcat 'identity
                       (seq-remove #'seq-empty-p `(,project-name ,vc-context ,tramp-host))
                       ", ")
            ")")))
        
;; -------------------------------------------------------------------
(defun nano-modeline-compose (status name primary secondary)
  "Compose a string with provided information"
  (let* ((char-width    (window-font-width nil 'header-line))
         (window        (get-buffer-window (current-buffer)))
         (space-up       +0.15)
         (space-down     -0.20)
         (left (concat
                (propertize " "  'face 'nano-face-header-default
			    'display `(raise ,space-up))
                (nano-buffer-status)
                (propertize " "  'face 'nano-face-header-default
			    'display `(raise ,space-down))
		(propertize primary 'face 'nano-face-header-default)))
         (right (concat secondary " "))
         (available-width (- (window-total-width) 
			     (length status) (length left) (length right)
			     (/ (window-right-divider-width) char-width)))
	 (available-width (max 1 available-width)))
    (concat status
	    left
	    (propertize (make-string available-width ?\ )
                        'face 'nano-face-header-default)
	    (propertize right 'face `(:inherit nano-face-header-default
                                      :foreground ,nano-color-faded)))))
;; ---------------------------------------------------------------------
(defun nano-modeline ()
  "Install a header line whose content is dependend on the major mode"
  (interactive)
  (setq-default header-line-format
                '(:eval (let ((buffer-name (format-mode-line "%b"))
                              (position    (format-mode-line "%l:%c")))
                          (nano-modeline-compose (nano-modeline-status)
                                                 buffer-name
                                                 (nano-modeline-context)
                                                 position)))))

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
(setq-default mode-line-format "")
(nano-modeline)

(provide 'nano-modeline)



