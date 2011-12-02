;;; org-tree-slide.el --- A presentation tool for org-mode
;;
;; Copyright (C) 2011 Takaaki ISHIKAWA
;;
;; Author: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Maintainer: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Twitter: @takaxp
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
;;
;;; Requirement:
;;    org-mode 6.33x or higher version
;;    The latest version of the org-mode at http://orgmode.org/ is recommended.
;;
;;; History:
;;    v2.0.1 (2011-12-02@18:29) # Change function names, ots- is introduced.
;;    v2.0.0 (2011-12-01@17:41) # Add profiles and support org 6.33x
;;    v1.2.5 (2011-10-31@18:34) # Add CONTENT view to see all the subtrees.
;;    v1.2.3 (2011-10-30@20:42) # Add a variable to control slide-in duration
;;    v1.2.1 (2011-10-30@16:10) # Add slide-in visual effect
;;    v1.1.1 (2011-10-28@16:16) # Add functions to start and stop slide view
;;    v1.0.0 (2011-09-28@20:59) # Release an init version
;;
;;; Usage:
;;    1. Put this elisp into your load-path
;;    2. Add (requre 'org-tree-slide) in your .emacs
;;    3. Open an org-mode file 
;;    4. M-x org-tree-slide-play, now you in slide view
;;    5. <right>/<left> will move slides, mode line will be changed
;;    6. M-x org-tree-slide-stop, return to normal view
;;
;;; Note:
;;    - Make sure key maps below when you introduce this elisp.
;;    - Customize variables, M-x customize-group ENT org-tree-slide ENT

(require 'org)
(require 'org-timer)

(defconst org-tree-slide "2.0.1"
  "The version number of the org-tree-slide.el")

(defgroup org-tree-slide nil
  "User variables for org-tree-slide."
  :group 'org-structure)

(defcustom org-tree-slide-title nil
  "Specify the title of presentation. The title is shown in a header area. 
   If this variable is nil, the name of current buffer will be displayed
   as a slide title."
  :type 'string
  :group 'org-tree-slide)

(defcustom org-tree-slide-auto-play-period 0
  "If this variable is greater than 0, the slide show move to the next tree
   automatically, and the value specify an interval."
  :type 'float
  :group 'org-tree-slide)

(defcustom org-tree-slide-header t
  "The status of displaying the slide header"
  :type 'boolean
  :group 'org-tree-slide)

(defcustom org-tree-slide-slide-in-effect t
  "Using a visual effect of slide-in for displaying trees."
  :type 'boolean
  :group 'org-tree-slide)

(defcustom org-tree-slide-slide-in-brank-lines 10
  "Specify the number of brank lines, the slide will move from this line."
  :type 'integer
  :group 'org-tree-slide)

(defcustom org-tree-slide-slide-in-waiting 0.02
  "Specify the duration waiting the next update of overlay."
  :type 'float
  :group 'org-tree-slide)

(defcustom org-tree-slide-heading-emphasis nil
  "Specify to use a custom face heading, or not"
  :type 'boolean
  :group 'org-tree-slide)

(defcustom org-tree-slide-previous-key (kbd "<left>")
  "Specify the key for moving to the next slide."
  :type 'string
  :group 'org-tree-slide)

(defcustom org-tree-slide-next-key (kbd "<right>")
  "Specify the key for moving to the next slide."
  :type 'string
  :group 'org-tree-slide)

(defface org-tree-slide-heading-level-2-init
  '((t (:inherit outline-2)))
  "Level 2."
  :group 'org-tree-slide)

(defface org-tree-slide-heading-level-3-init
  '((t (:inherit outline-3)))
  "Level 3."
  :group 'org-tree-slide)

(defface org-tree-slide-heading-level-2
  '((t (:inherit outline-2 :height 1.4 :bold t)))
  "Level 2."
  :group 'org-tree-slide)

(defface org-tree-slide-heading-level-3
  '((t (:inherit outline-3 :height 1.3 :bold t)))
  "Level 3."
  :group 'org-tree-slide)

;;; The default key bindings for org-tree-slide.
(define-key org-mode-map (kbd "C-x s p") 'org-tree-slide-play)
(define-key org-mode-map (kbd "C-x s s") 'org-tree-slide-stop)
(define-key org-mode-map (kbd "C-x s c") 'org-tree-slide-content)
(define-key org-mode-map (kbd "C-x s a") 'org-tree-slide-auto-play-start)
;(define-key org-mode-map (kbd "<f5>") 'org-narrow-to-subtree)
;(define-key org-mode-map (kbd "<S-f5>") 'widen)

(defvar ots-active nil
  "A flag to check if the slideshow is ACTIVE or not.")

(defun org-tree-slide-play (&optional arg)
  "Start slide view with the first tree of the org-mode buffer.
   If you all this function with a prefix (C-u), you can set 
   a countdown timer to control your presentation."
  (interactive "P")
  (if (ots-active-p) (message "org-tree-slide is ACTIVE.")
    (setq ots-active t)
    (when arg
      (org-timer-set-timer))
    (when org-tree-slide-heading-emphasis
      (ots-apply-custom-heading-face t))
    (ots-apply-control-keybindings)
    (ots-move-to-the-first-heading)
    (ots-display-tree-with-narrow)
    (message "Hello! This is org-tree-slide :-)")))

(defun org-tree-slide-stop ()
  "Stop the slide view, and redraw the org-mode buffer with OVERVIEW."
  (interactive)
  (when (ots-active-p)
    (setq ots-active nil)
    (widen)
    (org-overview)
    (ots-move-to-the-first-heading)
    (ots-hide-slide-header)
    (ots-remove-control-keybindings)
    (org-timer-pause-or-continue 'stop)
    (ots-apply-custom-heading-face nil)
    (message "Quit, Bye!")))

(defun org-tree-slide-content ()
  "Change the display for viewing content of the org file during
   the slide view mode is active."
  (interactive)
  (when (ots-active-p)
    (ots-hide-slide-header)
    (ots-move-to-the-first-heading)
    (org-overview)
    (org-content)
    (message "<<  CONTENT  >>")))

(defun org-tree-slide-simple-profile ()
  "Set variables for simple use."
  (interactive)
  (setq org-tree-slide-header nil)
  (setq org-tree-slide-slide-in-effect nil)
  (setq org-tree-slide-heading-emphasis nil)
  (message "simple profile: ON"))

(defun org-tree-slide-presentation-profile ()
  "Set variables for presentation use."
  (interactive)
  (setq org-tree-slide-header t)
  (setq org-tree-slide-slide-in-effect t)
  (setq org-tree-slide-heading-emphasis nil)
  (message "presentation profile: ON"))

(defun org-tree-slide-display-header-toggle ()
  "Toggle displaying the slide header"
  (interactive)
  (setq org-tree-slide-header (not org-tree-slide-header))
  (unless org-tree-slide-header
    (ots-hide-slide-header))
  (ots-display-tree-with-narrow))

(defun org-tree-slide-slide-in-effect-toggle ()
  "Toggle using slide-in effect"
  (interactive)
  (setq org-tree-slide-slide-in-effect (not org-tree-slide-slide-in-effect))
  (ots-display-tree-with-narrow))

(defun org-tree-slide-heading-emphasis-toggle ()
  (interactive)
  (setq org-tree-slide-heading-emphasis (not org-tree-slide-heading-emphasis))
  (ots-apply-custom-heading-face org-tree-slide-heading-emphasis))

(defun org-tree-slide-move-next-tree ()
  "Display the next slide"
  (interactive)
  (when (ots-active-p)
    (message "   Next >>")
    (cond ((or (and (ots-before-first-heading-p) (not (org-on-heading-p)))
	      (= (point-at-bol) 1)) ; support single top level tree
	   (outline-next-heading))
	  ((or (ots-first-heading-with-narrow-p) (not (org-on-heading-p)))
	   (hide-subtree)
	   (widen)
	   ;; (if (> 7.3 (string-to-number org-version)) ; for 6.33x
	   ;;     (ots-hide-slide-header)	       
	   ;;     (org-content))
	   (outline-next-heading))
	  (t nil))
    (ots-display-tree-with-narrow)))

(defun org-tree-slide-move-previous-tree ()
  "Display the previous slide"
  (interactive)
  (when (ots-active-p)
    (message "<< Previous")
    (hide-subtree)
    (widen)
    (ots-hide-slide-header)		; for at the first heading
    (cond ((ots-before-first-heading-p)
	   (message "The first slide!"))
	  ((not (org-on-heading-p))
	   (outline-previous-heading)
	   (outline-previous-heading))
	  (t (outline-previous-heading)))
    (ots-display-tree-with-narrow)
    ;; To avoid error of missing header in Emacs24
    (if (= emacs-major-version 24)
	(goto-char (point-min)))))

;;; Internal functions
(defvar ots-right-key-assigned nil
  "Store the previous command assigned to <right>.")
(defvar ots-left-key-assigned nil
  "Store the previous command assigned to <left>.")
(defvar ots-modeline-assigned nil
  "Store the previous mode-line-format.")
(defvar ots-header-overlay nil
  "Flag to check the status of overlay for a slide header.")

(defun ots-display-tree-with-narrow ()
  "Show a tree with narrowing and also set a header at the head of slide."
  (goto-char (point-at-bol))
  (org-show-entry)
  (show-children)
  (org-cycle-hide-drawers 'all)
  (org-narrow-to-subtree)
  (when org-tree-slide-slide-in-effect
    (ots-slide-in org-tree-slide-slide-in-brank-lines))
  (when org-tree-slide-header
    (ots-show-slide-header)))

(defun ots-slide-in (brank-lines)
  (while (< 2 brank-lines)
    (ots-set-slide-header brank-lines)
    (sit-for org-tree-slide-slide-in-waiting)
    (ots-hide-slide-header)
    (setq brank-lines (1- brank-lines))))

(defun ots-set-slide-header (brank-lines)
  (ots-hide-slide-header)
  (setq ots-header-overlay
	(make-overlay (point-min) (+ 1 (point-min))))
  (overlay-put ots-header-overlay 'after-string " ")
  (overlay-put ots-header-overlay
	       'face
	       '((foreground-color . "#696969")
		 (background-color . "#FFFFFF") bold))
  (if org-tree-slide-header
      (overlay-put ots-header-overlay 'display
		   (concat "  [ " 
			   (unless org-tree-slide-title
			     (buffer-name))
			   " ] (" (format-time-string "%Y-%m-%d") ")"
			   (ots-get-brank-lines brank-lines)))
    (overlay-put ots-header-overlay 'display
		 (ots-get-brank-lines brank-lines))))

(defun ots-get-brank-lines (lines)
  (let ((breaks ""))
    (while (< 0 lines)
      (setq lines (1- lines))
      (setq breaks (concat breaks "\n")))
    breaks))

(defun ots-show-slide-header ()
  (ots-set-slide-header 2)
  (forward-char 1))

(defun ots-hide-slide-header ()
  (when ots-header-overlay
    (delete-overlay ots-header-overlay)))

(defun ots-move-to-the-first-heading ()
  (widen)
  (goto-char (point-min))
  (when (ots-before-first-heading-p)
    (outline-next-heading)))

(defun ots-save-previous-propaties ()
  (setq ots-right-key-assigned
	(lookup-key org-mode-map org-tree-slide-next-key))
  (setq ots-left-key-assigned
	(lookup-key org-mode-map org-tree-slide-previous-key))
  (setq ots-modeline-assigned mode-line-format))

(defun ots-remove-control-keybindings ()
  (define-key org-mode-map org-tree-slide-next-key ots-right-key-assigned)
  (define-key org-mode-map org-tree-slide-previous-key ots-left-key-assigned)
  (setq mode-line-format ots-modeline-assigned))

(defun ots-apply-control-keybindings ()
  (ots-save-previous-propaties)
  (define-key org-mode-map
    org-tree-slide-next-key 'org-tree-slide-move-next-tree)
  (define-key org-mode-map
    org-tree-slide-previous-key 'org-tree-slide-move-previous-tree)
  (setq mode-line-format
	'(" -"
	  mode-line-mule-info
	  mode-line-modified
	  " "
;	  mode-line-frame-identification
	  mode-line-buffer-identification
	  " [playing] / Stop: C-x s s / "
	  global-mode-string
	  "-%-")))

(defun ots-apply-custom-heading-face (status)
  "Change status of heading face."
  (cond (status
	 (custom-set-faces
	  '(org-level-2 ((t (:inherit org-tree-slide-heading-level-2))))
	  '(org-level-3 ((t (:inherit org-tree-slide-heading-level-3)))))
	 (message "Face: ON"))
	(t
	 (custom-set-faces
	  '(org-level-2 ((t (:inherit org-tree-slide-heading-level-2-init))))
	  '(org-level-3 ((t (:inherit org-tree-slide-heading-level-3-init)))))
	 (message "Face: OFF"))))

(defun ots-active-p ()
  (and ots-active (equal 'org-mode major-mode)))

(defun ots-narrowing-p ()
  "Check the current status if narrowing or not"
  (not (and (= (point-min) 1) (= (point-max) (1+ (buffer-size))))))

(defun ots-before-first-heading-p ()
  "Extension of org-before-first-heading-p to support org 6.33x.
#+TITLE: title     ; t
#+STARTUP: content ; t
* first            ; t
  hoge             ; nil
** second          ; nil
** third           ; nil
"
  (and (org-before-first-heading-p) (not (ots-narrowing-p))))
  
(defun ots-first-heading-with-narrow-p ()
  "Check the current point is on the first heading with narrowing.
** first           ; t
   hoge            ; nil
   hoge            ; nil
*** second         ; nil
    hoge           ; nil
*** third          ; nil
"
  (and (ots-narrowing-p) (= (point-at-bol) (point-min))))


;;; Test....
;(defcustom org-tree-slide-header-background-color "#FFFFFF"
;  "Specify the color of header background."
;  :type 'string
;  :group 'org-tree-slide)

;(defcustom org-tree-slide-header-foreground-color "#666699"
;  "Specify the color of header background."
;  :type 'string
;  :group 'org-tree-slide)

(defun org-tree-slide-auto-play-start (skip-slides)
  "Start auto play, type `C-g' to stop it"
  (interactive "nHow many slide play auto? ")
  (message "Skip %d slides ..." skip-slides)
  (sit-for 1)
  (cond 
   ((not org-tree-slide-slide-in-effect)
    (message "Please M-x org-tree-slide-slide-in-effect-toggle"))
   (ots-active
    (let((stop-count skip-slides)
	 (count 0))
      (while (< count stop-count)
	(org-tree-slide-move-next-tree)
	(message "auto play %s" count)
	(sleep-for 0.5)
	(setq count (1+ count)))
      (org-tree-slide-content)))
   (t
    (message "Start slide show first with C-x s p :-)"))))

(provide 'org-tree-slide)

;;; org-tree-slide.el ends here
