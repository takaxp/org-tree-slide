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
;;    v2.4.0 (2011-12-08@10:51) # Support TODO pursuit in a slideshow
;;    v2.3.2 (2011-12-08@09:22) # Reduce redundant processing
;;    v2.3.1 (2011-12-07@20:30) # Add a new profile to control narrowing status
;;    v2.3.0 (2011-12-07@16:17) # Support displaying a slide number
;;    v2.2.0 (2011-12-07@02:15) # Support minor mode
;;    v2.1.7 (2011-12-06@00:26) # Support TITLE/AUTHOR/EMAIL in a header
;;    v2.1.5 (2011-12-05@17:08) # Fix an issue of title display
;;    v2.1.3 (2011-12-05@15:08) # Fix the end of slide for skip ccontrol
;;    v2.1.1 (2011-12-05@11:08) # Add skip control by heading level
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
;;    4. Toggle org-tree-slide-mode, M-x org-mode-slide-mode
;;    5. <right>/<left> will move slides, you can find "TSlide" in mode line.
;;    6. Toggle org-tree-slide-mode again, return to normal view
;;
;;; Note:
;;    - Make sure key maps below when you introduce this elisp.
;;    - Customize variables, M-x customize-group ENT org-tree-slide ENT

(require 'org)
(require 'org-timer)

(defconst org-tree-slide "2.4.0"
  "The version number of the org-tree-slide.el")

(defgroup org-tree-slide nil
  "User variables for org-tree-slide."
  :group 'org-structure)

(defcustom org-tree-slide-skip-outline-level 0
  "Skip the current slide if the level is higher than or equal to this variable.
   `0': never skip at any heading
   e.g. set `4', 
   *** heading A  ; display as a slide
       entry
   **** heading B ; skip! do not display as the next slide
   **** heading C ; skip! 
   *** heading D  ; display as the next slide
"
  :type 'integer
  :group 'org-tree-slide)

(defcustom org-tree-slide-title nil
  "Specify the title of presentation. The title is shown in a header area. 
   If you have `#+TITLE:' line in your org buffer, it wil be used as a title
   of the slide. If this variable is nil and no `#+TITLE:' line, the name of
   current buffer will be displayed."
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

(defcustom org-tree-slide-cursor-init nil
  "Specify a cursor position when the slide start.
  `t': the cursor will move automatically to the head of buffer."
  :type 'boolean
  :group 'org-tree-slide)

(defcustom org-tree-slide-slide-in-waiting 0.02
  "Specify the duration waiting the next update of overlay."
  :type 'float
  :group 'org-tree-slide)

(defcustom org-tree-slide-heading-emphasis nil
  "Specify to use a custom face heading, or not"
  :type 'boolean
  :group 'org-tree-slide)

(defcustom org-tree-slide-skip-done t
  "Specify to show TODO item only or not."
  :type'boolean
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

(defvar org-tree-slide-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x s c") 'org-tree-slide-content)
    (define-key map (kbd "C-x s a") 'org-tree-slide-auto-play-start)
    (define-key map (kbd "<left>") 'org-tree-slide-move-previous-tree)
    (define-key map (kbd "<right>") 'org-tree-slide-move-next-tree)
    map)
  "The default key bindings for org-tree-slide.")

(defvar org-tree-slide-mode-hook nil)
(defvar display-tree-slide-string nil)
(define-minor-mode org-tree-slide-mode
  "A presentation tool for org-mode.

Usage:
  - Install this elisp into your load-path.
  - Set minimal recommendation settings in .emacs
    (global-set-key (kbd \"<f8>\") 'org-tree-slide-mode)
    (global-set-key (kbd \"S-<f8>\") 'org-tree-slide-skip-done-toggle)
  - Open an org file
  - Type <f8> to start org-tree-slide-mode
  - Type <left>/<right> to move between trees
  - To exit this minor mode, just type <f8> again

Profiles:
  - Simple
    M-x org-tree-slide-simple-profile

  - Presentation
    M-x org-tree-slide-presentation-profile

  - TODO Pursuit
    M-x org-tree-slide-narrowing-control-profile
    M-x org-tree-slide-skip-done-toggle
"
  :lighter (:eval (ots-update-modeline))
  :keymap org-tree-slide-mode-map
  :group 'org-tree-slide
  :require 'org
  (setq display-tree-slide-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (if org-tree-slide-mode
      (progn
	(ots-setup)
	(run-hooks 'org-mode-slide-mode-hook))
    (ots-abort)))

(defvar ots-slide-number " TSlide")
(defun ots-update-modeline ()
  (if (and (ots-active-p) (org-on-heading-p))
      (setq ots-slide-number (format " %s" (ots-count-slide (point))))
    ots-slide-number))

(defun org-tree-slide-play-with-timer ()
  "Start slideshow with setting a count down timer."
  (interactive)
  (org-timer-set-timer)
  (unless (ots-active-p)
    (org-tree-slide-mode)))

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
  (setq org-tree-slide-cursor-init t)
  (message "simple profile: ON"))

(defun org-tree-slide-presentation-profile ()
  "Set variables for presentation use."
  (interactive)
  (setq org-tree-slide-header t)
  (setq org-tree-slide-slide-in-effect t)
  (setq org-tree-slide-heading-emphasis nil)
  (setq org-tree-slide-cursor-init t)
  (message "presentation profile: ON"))

(defun org-tree-slide-narrowing-control-profile ()
  "Set variables to switch a status of narrowing or widen."
  (interactive)
  (setq org-tree-slide-header nil)
  (setq org-tree-slide-slide-in-effect nil)
  (setq org-tree-slide-heading-emphasis nil)
  (setq org-tree-slide-cursor-init nil)
  (message "narrowing control profile: ON"))

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
  "Toggle applying emphasis to heading"
  (interactive)
  (setq org-tree-slide-heading-emphasis (not org-tree-slide-heading-emphasis))
  (ots-apply-custom-heading-face org-tree-slide-heading-emphasis))

(defun org-tree-slide-skip-done-toggle ()
  "Toggle show TODO item only or not"
  (interactive)
  (setq org-tree-slide-skip-done (not org-tree-slide-skip-done))
  (if org-tree-slide-skip-done
      (message "TODO Pursuit: ON") (message "TODO Pursuit: OFF")))

(defun org-tree-slide-move-next-tree ()
  "Display the next slide"
  (interactive)
  (when (ots-active-p)
    (message "   Next >>")
    (cond ((or (and (ots-before-first-heading-p) (not (org-on-heading-p)))
	      (= (point-at-bol) 1)) ; support single top level tree
	   (ots-outline-next-heading))
	  ((or (ots-first-heading-with-narrow-p) (not (org-on-heading-p)))
	   (hide-subtree)
	   (widen)
	   (ots-outline-next-heading))
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
	   (ots-outline-previous-heading)
	   (ots-outline-previous-heading))
	  (t (ots-outline-previous-heading)))
    (ots-display-tree-with-narrow)
    ;; To avoid error of missing header in Emacs24
    (if (= emacs-major-version 24)
	(goto-char (point-min)))))

;;; Internal functions
(defvar ots-header-overlay nil
  "Flag to check the status of overlay for a slide header.")

(defun ots-setup ()
  (when (equal major-mode 'org-mode)
    (or (memq 'display-tree-slide-string global-mode-string)
	(setq global-mode-string
	      (append global-mode-string '(display-tree-slide-string))))
    (ots-play)))

(defun ots-abort ()
  (when (equal major-mode 'org-mode)
    (ots-stop)))

(defun ots-play ()
  "Start slide view with the first tree of the org-mode buffer."
  (ots-apply-local-header-to-slide-header)
  (when org-tree-slide-heading-emphasis
    (ots-apply-custom-heading-face t))
  (when (or org-tree-slide-cursor-init (ots-before-first-heading-p))
    (ots-move-to-the-first-heading))
  (ots-display-tree-with-narrow)
  (message "Hello! This is org-tree-slide :-)"))

(defun ots-stop ()
  "Stop the slide view, and redraw the org-mode buffer with #+STARTUP:."
  (widen)
  (org-show-siblings)
  (when (or org-tree-slide-cursor-init (ots-before-first-heading-p))
    (goto-char (point-min))
    (org-overview)
    (cond ((equal "content" org-tree-slide-startup)
	   (message "CONTENT: %s" org-tree-slide-startup)
	   (org-content))
	  ((equal "showall" org-tree-slide-startup)
	   (message "SHOW ALL: %s" org-tree-slide-startup)
	   (org-cycle '(64)))
	  (t nil)))
  (ots-hide-slide-header)
  (org-timer-pause-or-continue 'stop)
  (ots-apply-custom-heading-face nil)
  (message "Quit, Bye!"))

(defun ots-display-tree-with-narrow ()
  "Show a tree with narrowing and also set a header at the head of slide."
  (goto-char (point-at-bol))
  (hide-subtree)			; support CONTENT (subtrees are shown)
  (org-show-entry)
  (show-children)
  (org-cycle-hide-drawers 'all)
  (org-narrow-to-subtree)
  ;; (setq display-tree-slide-string (ots-count-slide (point)))
  (when org-tree-slide-slide-in-effect
    (ots-slide-in org-tree-slide-slide-in-brank-lines))
  (when org-tree-slide-header
    (ots-show-slide-header)))

(defun ots-outline-next-heading ()
  (ots-outline-select-method
   (ots-outline-skip-p
    (if (outline-next-heading) t 'last)
    (org-outline-level))
   'next))

(defun ots-outline-previous-heading ()
  (ots-outline-select-method
   (ots-outline-skip-p
    (if (outline-previous-heading) t 'first)
    (org-outline-level))
   'previous))

(defun ots-outline-select-method (action direction)
  (cond ((and (equal action 'skip) (equal direction 'next))
	 (ots-outline-next-heading))
	((and (equal action 'skip) (equal direction 'previous))
	 (ots-outline-previous-heading))
	((and (equal action 'last) (equal direction 'next))
	 (ots-outline-previous-heading)) ; Return back.
	((and (equal action 'first) (equal direction 'previous))
	 (ots-move-to-the-first-heading)) ; Stay the first heading
	(t nil)))

(defun ots-outline-skip-p (has-target-outline current-level)
  (cond ((equal has-target-outline 'last)
	 'last)
	((equal has-target-outline 'first)
	 'first)
	((and (> org-tree-slide-skip-outline-level 0)
	      (<= org-tree-slide-skip-outline-level current-level)) 'skip)
	((and org-tree-slide-skip-done
	      (not
	       (looking-at
		(concat org-outline-regexp-bol org-not-done-regexp))) 'skip))
	(t nil)))

(defun ots-slide-in (brank-lines)
  (while (< 2 brank-lines)
    (ots-set-slide-header brank-lines)
    (sit-for org-tree-slide-slide-in-waiting)
    (ots-hide-slide-header)
    (setq brank-lines (1- brank-lines))))

(defvar org-tree-slide-email nil
  "If you have `#+EMAIL:' line in your org buffer, it will be used as
   an address of the slide.")

(defvar org-tree-slide-author nil
  "If you have `#+AUTHOR:' line in your org buffer, it will be used as
   a name of the slide author.")

(defvar org-tree-slide-startup "overview"
  "If you have `#+STARTUP:' line in your org buffer, the org buffer will
   be shown with corresponding status (content, showall, overview:default).")

(defun ots-apply-local-header-to-slide-header ()
  (save-excursion
    (ots-move-to-the-first-heading)
    (let ((limit (point)))
      (ots-set-header-variable-by-rexep
       'org-tree-slide-title "#\\+TITLE:[ \t]*\\(.*\\)$" limit)
      (ots-set-header-variable-by-rexep
       'org-tree-slide-author "#\\+AUTHOR:[ \t]*\\(.*\\)$" limit)
      (ots-set-header-variable-by-rexep
       'org-tree-slide-email "#\\+EMAIL:[ \t]*\\(.*\\)$" limit)
      (ots-set-header-variable-by-rexep
       'org-tree-slide-startup "#\\+STARTUP:[ \t]*\\(.*\\)$" limit))))

(defun ots-set-header-variable-by-rexep (header-variable regexp limit)
  (goto-char 1)
  (set header-variable
       (if (re-search-forward regexp limit t) (match-string 1) nil)))

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
			   (if org-tree-slide-title org-tree-slide-title
			     (buffer-name))
			   " ]\n"
			   (format-time-string "%Y-%m-%d") "  "
			   (when org-tree-slide-author
			     (concat org-tree-slide-author "  "))
			   (when org-tree-slide-email
			     (concat "<" org-tree-slide-email ">"))
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

(defun ots-count-slide (target-point)
  (save-excursion
    (save-restriction
      (ots-move-to-the-first-heading)	; include widen
      (let
	  ((count 0)
	   (previous-point 0)
	   (current-slide 0))
	(while (/= (point) previous-point) ; convergence point
	  (setq count (1+ count))
	  (when (<= (point) target-point)
	    (setq current-slide count))
	  (setq previous-point (point))
	  (ots-outline-next-heading))
	(format "[%d/%d]" current-slide count)))))

(defun ots-active-p ()
  (and org-tree-slide-mode (equal major-mode 'org-mode)))

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
   ((ots-active-p)
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
