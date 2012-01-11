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
;;    The latest version of the org-mode is recommended.
;;                      (see http://orgmode.org/)
;;
;;; Usage:
;;    1. Put this elisp into your load-path
;;    2. Add (require 'org-tree-slide) in your .emacs
;;    3. Open an org-mode file 
;;    4. Toggle org-tree-slide-mode (M-x org-tree-slide-mode)
;;       then Slideshow will start and you can find "TSlide" in mode line.
;;    5. <left>/<right> will move between slides
;;    6. `C-x s c' will show CONTENT of the org buffer
;;       Select a heading and type <right>, then Slideshow will start again.
;;    7. Toggle org-tree-slide-mode again to exit this minor mode
;;
;;; Recommended minimum settings:
;;    (global-set-key (kbd "<f8>") 'org-tree-slide-mode)
;;    (global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)
;;
;;  and three useful profiles are available.
;;
;;    1. Simple use
;;       M-x org-tree-slide-simple-profile
;;
;;    2. Presentation use
;;       M-x org-tree-slide-presentation-profile
;;
;;    3. TODO Pursuit with narrowing
;;       M-x org-tree-slide-narrowing-control-profile
;;
;;    Type `C-h f org-tree-slide-mode', you can find more detail.
;;
;;; Note:
;;    - Make sure key maps below when you introduce this elisp.
;;    - Customize variables, M-x customize-group ENT org-tree-slide ENT

(require 'org)
(require 'org-timer)
(require 'org-clock)			; org-clock-in, -out, -clocking-p

(defconst org-tree-slide "2.5.4"
  "The version number of the org-tree-slide.el")

(defgroup org-tree-slide nil
  "User variables for org-tree-slide."
  :group 'org-structure)

(defcustom org-tree-slide-skip-outline-level 0
  "Skip slides if a heading level is higher than or equal to this variable.
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

(defcustom org-tree-slide-header t
  "The status of displaying the slide header"
  :type 'boolean
  :group 'org-tree-slide)

(defcustom org-tree-slide-slide-in-effect t
  "Using a visual effect of slide-in for displaying trees."
  :type 'boolean
  :group 'org-tree-slide)

(defcustom org-tree-slide-cursor-init t
  "Specify a cursor position when exit slideshow.
  `t': the cursor will move automatically to the head of buffer.
  nil: keep the same position."
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

(defcustom org-tree-slide-skip-done nil
  "Specify to show TODO item only or not."
  :type 'boolean
  :group 'org-tree-slide)

(defcustom org-tree-slide-modeline-display 'outside
  "Specify how to display the slide number in mode line.
   'outside: shown in the mode line outside of lighter
   'lighter: shown in lighter (slow)
   nil: nothing to be shown"
  :type 'symbol
  :group 'org-tree-slide)

(defvar org-tree-slide-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x s c") 'org-tree-slide-content)
    (define-key map (kbd "C-x s r") 'org-tree-slide-resume)
    (define-key map (kbd "<left>") 'org-tree-slide-move-previous-tree)
    (define-key map (kbd "<right>") 'org-tree-slide-move-next-tree)
    map)
 "The default key bindings for org-tree-slide.")

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

(defvar org-tree-slide-mode-hook nil)
(defvar display-tree-slide-string nil)

;;;###autoload
(define-minor-mode org-tree-slide-mode
  "A presentation tool for org-mode.

Usage:
  - Set minimal recommendation settings in .emacs
    (global-set-key (kbd \"<f8>\") 'org-tree-slide-mode)
    (global-set-key (kbd \"S-<f8>\") 'org-tree-slide-skip-done-toggle)
  - Open an org file
  - Type <f8> to start org-tree-slide-mode
  - Type <left>/<right> to move between trees
  - To exit this minor mode, just type <f8> again.

Profiles:

  - [ Simple ]
 => M-x `org-tree-slide-simple-profile'

    1. No header display
    2. No slide-in effect
    3. The cursor will move to the head of buffer when exit
    4. No slide number display in mode line
    5. Display every type of tree

  - [ Presentation ]
 => M-x `org-tree-slide-presentation-profile'

    1. Display header
    2. Enable slide-in effect
    3. The cursor will move to the head of buffer when exit
    4. Display slide number in mode line
    5. Display every type of tree

  - [ TODO Pursuit with narrowing ]
 => M-x `org-tree-slide-narrowing-control-profile'

    1. No header display
    2. No slide-in effect
    3. The cursor will keep the same position when exit
    4. Display slide number in mode line
    5. Display TODO trees only
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

;;;###autoload
(defun org-tree-slide-play-with-timer ()
  "Start slideshow with setting a count down timer."
  (interactive)
  (org-timer-set-timer)
  (unless (ots-active-p)
    (org-tree-slide-mode)))

;;;###autoload
(defun org-tree-slide-without-init-play ()
  "Start slideshow without the init play. Just enter org-tree-slide-mode."
  (interactive)
  (org-tree-slide-mode)
  (widen)
  (org-overview)  
  (goto-char 1))

;;;###autoload
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

;;;###autoload
(defun org-tree-slide-simple-profile ()
  "Set variables for simple use.
  `org-tree-slide-header'            => nil
  `org-tree-slide-slide-in-effect'   => nil
  `org-tree-slide-heading-emphasis'  => nil
  `org-tree-slide-cursor-init'       => t
  `org-tree-slide-modeline-display'  => nil
  `org-tree-slide-skip-done'         => nil
"
  (interactive)
  (setq org-tree-slide-header nil)
  (setq org-tree-slide-slide-in-effect nil)
  (setq org-tree-slide-heading-emphasis nil)
  (setq org-tree-slide-cursor-init t)
  (setq org-tree-slide-modeline-display nil)
  (setq org-tree-slide-skip-done nil)
  (message "simple profile: ON"))

;;;###autoload
(defun org-tree-slide-presentation-profile ()
  "Set variables for presentation use.
  `org-tree-slide-header'            => t
  `org-tree-slide-slide-in-effect'   => t
  `org-tree-slide-heading-emphasis'  => nil
  `org-tree-slide-cursor-init'       => t
  `org-tree-slide-modeline-display'  => 'outside
  `org-tree-slide-skip-done'         => nil
"
  (interactive)
  (setq org-tree-slide-header t)
  (setq org-tree-slide-slide-in-effect t)
  (setq org-tree-slide-heading-emphasis nil)
  (setq org-tree-slide-cursor-init t)
  (setq org-tree-slide-modeline-display 'outside)
  (setq org-tree-slide-skip-done nil)
  (message "presentation profile: ON"))

;;;###autoload
(defun org-tree-slide-narrowing-control-profile ()
  "Set variables for TODO pursuit with narrowing.
  `org-tree-slide-header'            => nil
  `org-tree-slide-slide-in-effect'   => nil
  `org-tree-slide-heading-emphasis'  => nil
  `org-tree-slide-cursor-init'       => nil
  `org-tree-slide-modeline-display'  => 'lighter
  `org-tree-slide-skip-done'         => t
"
  (interactive)
  (setq org-tree-slide-header nil)
  (setq org-tree-slide-slide-in-effect nil)
  (setq org-tree-slide-heading-emphasis nil)
  (setq org-tree-slide-cursor-init nil)
  (setq org-tree-slide-modeline-display 'lighter)
  (setq org-tree-slide-skip-done t)
  (message "narrowing control profile: ON"))

;;;###autoload
(defun org-tree-slide-display-header-toggle ()
  "Toggle displaying the slide header"
  (interactive)
  (setq org-tree-slide-header (not org-tree-slide-header))
  (unless org-tree-slide-header
    (ots-hide-slide-header))
  (ots-display-tree-with-narrow))

;;;###autoload
(defun org-tree-slide-slide-in-effect-toggle ()
  "Toggle using slide-in effect"
  (interactive)
  (setq org-tree-slide-slide-in-effect (not org-tree-slide-slide-in-effect))
  (ots-display-tree-with-narrow))

;;;###autoload
(defun org-tree-slide-heading-emphasis-toggle ()
  "Toggle applying emphasis to heading"
  (interactive)
  (setq org-tree-slide-heading-emphasis (not org-tree-slide-heading-emphasis))
  (ots-apply-custom-heading-face org-tree-slide-heading-emphasis))

;;;###autoload
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
	       ;; support single top level tree
	       ;; FIXME: no header org buffer + CONTENT view is not supported
	       (and (= (point-at-bol) 1) (not (ots-narrowing-p))))
	   (ots-outline-next-heading))
	  ((or (ots-first-heading-with-narrow-p) (not (org-on-heading-p)))
	   (hide-subtree)
	   (widen)
	   (ots-outline-next-heading))
	  (t nil))
    (when (and org-tree-slide-skip-done
	       (looking-at (concat "^\\*+ " org-not-done-regexp)))
      ;; (org-clock-in)
      )
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
    (when (and org-tree-slide-skip-done
	       (looking-at (concat "^\\*+ " org-not-done-regexp)))
      ;; (org-clock-in)
      )
    (ots-display-tree-with-narrow)
    ;; To avoid error of missing header in Emacs24
    (if (= emacs-major-version 24)
	(goto-char (point-min)))))


;;; Internal functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar ots-slide-number " TSlide")
(defun ots-update-modeline ()
  (cond ((equal org-tree-slide-modeline-display 'lighter)
	 (if (and (ots-active-p) (org-on-heading-p))
	     (setq ots-slide-number (format " %s" (ots-count-slide (point))))
	   ots-slide-number))
	((equal org-tree-slide-modeline-display 'outside) "")
	(t " TSlide")))

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
  (org-timer-stop)
  (ots-apply-custom-heading-face nil)
  (when (and org-tree-slide-skip-done
	     (looking-at (concat "^\\*+ " org-not-done-regexp)))
    (when (org-clocking-p)
      ;; (org-clock-out)
      ))
  (message "Quit, Bye!"))

(defun ots-display-tree-with-narrow ()
  "Show a tree with narrowing and also set a header at the head of slide."
  (goto-char (point-at-bol))
  (hide-subtree)			; support CONTENT (subtrees are shown)
  (org-show-entry)
  (show-children)
  (org-cycle-hide-drawers 'all)
  (org-narrow-to-subtree)
  (setq display-tree-slide-string
	(if (equal org-tree-slide-modeline-display 'outside)
	    (ots-count-slide (point))
	  ""))
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
		;; 6.33x does NOT suport org-outline-regexp-bol 
		(concat "^\\*+ " org-not-done-regexp))) 'skip))
	(t nil)))

(defun ots-slide-in (brank-lines)
  (while (< 2 brank-lines)
    (ots-set-slide-header brank-lines)
    (sit-for org-tree-slide-slide-in-waiting)
    (ots-hide-slide-header)
    (setq brank-lines (1- brank-lines))))

(defvar org-tree-slide-title nil
  "If you have `#+TITLE:' line in your org buffer, it wil be used as a title
   of the slide. If the buffer has no `#+TITLE:' line, the name of
   current buffer will be displayed.")

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
  (goto-char 1)  
  (unless (looking-at "^\\*+ ")
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
	   (current-slide 0)
	   (current-point (point)))
	(while (/= current-point previous-point) ; convergence point
	  (setq count (1+ count))
	  (when (<= current-point target-point)
	    (setq current-slide count))	; FIXME
	  (setq previous-point current-point)
	  (ots-outline-next-heading)
	  (setq current-point (point)))
	(when org-tree-slide-skip-done	; FIXME ignore the first tree
					; but, that is TODO, cannot count it.
	  (setq current-slide (1- current-slide))
	  (setq count (1- count)))
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


(provide 'org-tree-slide)

;;; org-tree-slide.el ends here
