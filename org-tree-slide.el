;;; org-tree-slide.el --- A presentation tool for org-mode
;;
;; Copyright (C) 2011 Takaaki ISHIKAWA
;;
;; Author: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Maintainer: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Twitter: @takaxp
;; Website: http://takaxp.com/
;; Repository: https://github.com/takaxp/org-tree-slide
;; Keywords: org-mode, presentation, narrowing
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;; History:
;;    v1.1.1 (2011-10-28@16:16) # Add functions to start and stop slide view
;;    v1.0.0 (2011-09-28@20:59) # Release an init version
;;
;;; Usage:
;;    1. Put this elisp into your load-path
;;    2. Add (requre 'org-tree-slide) in your .emacs
;;    3. Open an org-mode file 
;;    4. M-x tree-slide-play, now you in slide view
;;    5. <right>/<left> will move slides, mode line will be changed
;;    6. M-x tree-slide-stop, return to normal view
;;
;;; Note:
;;    - Make sure key maps below when you introduce this elisp.

(require 'org-timer)

(defconst org-tree-slide "1.1.1"
  "The version number of the org-tree-slide.el")

(defcustom tree-slide-title nil
  "Specify the title of presentation. The title is shown in a header area. If this variable is nil, the name of current buffer will be displayed as a slide title."
  :type 'string
  :group 'org-tree-slide)

;(defcustom tree-slide-header-background-color "#FFFFFF"
;  "Specify the color of header background."
;  :type 'string
;  :group 'org-tree-slide)

;(defcustom tree-slide-header-foreground-color "#666699"
;  "Specify the color of header background."
;  :type 'string
;  :group 'org-tree-slide)

(define-key org-mode-map (kbd "C-x s p") 'tree-slide-play)
(define-key org-mode-map (kbd "C-x s s") 'tree-slide-stop)
;(define-key org-mode-map (kbd "<f5>") 'org-narrow-to-subtree)
;(define-key org-mode-map (kbd "<S-f5>") 'widen)

(defun tree-slide-play (&optional arg)
  "Start slide view with the first tree of the org-mode buffer. If you all this function with a prefix (C-u), you can set a countdown timer to control your presentation."
  (interactive "P")
  (unless tree-slide-active
    (when arg
      (org-timer-set-timer))
    (setq tree-slide-active t)
    (apply-control-keybindings)
    (move-to-the-first-heading)
    (tree-slide-display-tree-with-narrow)
    (message "Hello! Org-tree slideshow is starting now.")))

(defun tree-slide-stop ()
  "Stop the slide view, and redraw the org-mode buffer with OVERVIEW."
  (interactive)
  (when tree-slide-active
    (setq tree-slide-active nil)
    (widen)
    (org-overview)
    (move-to-the-first-heading)
    (hide-slide-header)
    (remove-control-keybindings)
    (org-timer-pause-or-continue 'stop)
    (message "Quit, Bye!")))

(defvar tree-slide-active nil
  "Flag to check if the mode is ON or OFF.")
(defvar tree-slide-right-key-assigned nil
  "Store the previous command assigned to <right>.")
(defvar tree-slide-left-key-assigned nil
  "Store the previous command assigned to <left>.")
(defvar tree-slide-mode-line-format-assigned nil
  "Store the previous mode-line-format.")
(defvar tree-slide-footer-overlay nil
  "Flag to check the status of overlay for a slide header.")

(defun tree-slide-display-tree-with-narrow ()
  "Show a tree with narrowing and also set a header at the head of slide."
  (hide-slide-header)
  (hide-subtree)
  (show-entry)
  (show-children)
  (org-cycle-hide-drawers 'all)
  (org-narrow-to-subtree)
  (show-slide-header))

(defun show-slide-header ()
  (save-excursion
    (setq tree-slide-footer-overlay
	  (make-overlay (point-min) (+ 1 (point-min))))
    (overlay-put tree-slide-footer-overlay 'after-string " ")
;    (overlay-put tree-slide-footer-overlay
;		 'face '((background-color . (tree-slide-header-background-color))
;			 (foreground-color . (tree-slide-header-foreground-color)));)
    (overlay-put tree-slide-footer-overlay
		 'face
		 '((foreground-color . "#696969")
		   (background-color . "#FFFFFF") bold))
    (overlay-put tree-slide-footer-overlay 'display
		 (concat "  [ " 
			 (unless tree-slide-title
			   (buffer-name))
			 " ] (" (format-time-string "%Y-%m-%d") ")\n\n")))
  (forward-char 1))

(defun hide-slide-header ()
  (save-excursion
    (when tree-slide-footer-overlay
      (delete-overlay tree-slide-footer-overlay))))

(defun move-to-the-first-heading ()
  (widen)
  (goto-char (point-min))
  (when (org-before-first-heading-p)
    (outline-next-heading)))

(defun tree-slide-move-next-tree ()
  "Show the next slide"
  (interactive)
  (when tree-slide-active
    (if (org-before-first-heading-p) (outline-next-heading)
      (hide-subtree)
      (widen)
      (outline-next-heading))
    (tree-slide-display-tree-with-narrow)))

(defun tree-slide-move-previous-tree ()
  "Show the previous slide"
  (interactive)
  (when tree-slide-active
    (unless (org-before-first-heading-p)
      (hide-subtree)
      (widen)
      (unless (org-on-heading-p) 
	(outline-previous-heading))
      (outline-previous-heading)
      (tree-slide-display-tree-with-narrow))))

(defun save-previous-propaties ()
  (setq tree-slide-right-key-assigned (lookup-key org-mode-map (kbd "<right>")))
  (setq tree-slide-left-key-assigned (lookup-key org-mode-map (kbd "<left>")))
  (setq tree-slide-mode-line-format-assigned mode-line-format))

(defun remove-control-keybindings ()
  (define-key org-mode-map (kbd "<right>") tree-slide-right-key-assigned)
  (define-key org-mode-map (kbd "<left>")  tree-slide-left-key-assigned)
  (setq mode-line-format tree-slide-mode-line-format-assigned))

(defun apply-control-keybindings ()
  (save-previous-propaties)
  (define-key org-mode-map (kbd "<right>") 'tree-slide-move-next-tree)
  (define-key org-mode-map (kbd "<left>")  'tree-slide-move-previous-tree)
  (setq mode-line-format
	'("-"
	  mode-line-mule-info
	  mode-line-modified
;	  mode-line-frame-identification
	  mode-line-buffer-identification
	  " [slide playing] / Stop: M-x tree-slide-stop / "
	  global-mode-string
	  "-%-")))

(provide 'org-tree-slide)

;;; org-tree-slide.el ends here
