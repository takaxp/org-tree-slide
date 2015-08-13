;;; org-tree-slide.el --- A presentation tool for org-mode
;;
;; Copyright (C) 2011-2015 Takaaki ISHIKAWA
;;
;; Author: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Maintainer: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Twitter: @takaxp
;; Repository: https://github.com/takaxp/org-tree-slide
;; Keywords: org-mode, presentation, narrowing
;;
;; Committers: Yuuki ARISAWA (@uk-ar)
;;             Eric S Fraga
;;             Eike Kettner
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
;;(require 'org-clock)			; org-clock-in, -out, -clocking-p

(defconst org-tree-slide "2.8.4"
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

(defcustom org-tree-slide-fold-subtrees-skipped t
  "If this flag is true, the subtrees in a slide will be displayed in fold.
   When nil, the body of the subtrees will be revealed.
"
  :type 'boolean
  :group 'org-tree-slide)

(defcustom org-tree-slide-hide-subtrees-commented t
  "If this flag is true, the commented subtrees in a slide will be hidden
   When nil, the heading of the subtrees will be revealed.
"
  :type 'boolean
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

(defcustom org-tree-slide-never-touch-face nil
  "If t, do NOT touch any face setting."
  :type 'boolean
  :group 'org-tree-slide)

(defcustom org-tree-slide-skip-done nil
  "Specify to show TODO item only or not."
  :type 'boolean
  :group 'org-tree-slide)

(defcustom org-tree-slide-skip-comments t
  "Specify to skip COMMENT item or not."
  :type 'boolean
  :group 'org-tree-slide)

(defcustom org-tree-slide-activate-message
  "Hello! This is org-tree-slide :-)"
  "Message in mini buffer when org-tree-slide is activated."
  :type 'string
  :group 'org-tree-slide)

(defcustom org-tree-slide-deactivate-message
  "Quit, Bye!"
  "Message in mini buffer when org-tree-slide is deactivated."
  :type 'string
  :group 'org-tree-slide)

(defcustom org-tree-slide-modeline-display 'outside
  "Specify how to display the slide number in mode line.
   'lighter: shown in lighter (update info actively, then it's slow)
   'outside: update infomation when moving to the next/previous slide
   nil: nothing to be shown"
  :type 'symbol
  :group 'org-tree-slide)

(defvar org-tree-slide-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x s c") 'org-tree-slide-content)
    ;;    (define-key map (kbd "C-x s r") 'org-tree-slide-resume) ;; TODO
    (define-key map (kbd "C-<") 'org-tree-slide-move-previous-tree)
    (define-key map (kbd "C->") 'org-tree-slide-move-next-tree)
    map)
  "The keymap for `org-tree-slide'.")

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

(defvar org-tree-slide-mode nil)
;; These hooks was obsoleted, and will be deleted by Oct. 2015.
(defvar org-tree-slide-mode-play-hook nil
  "[obsolate] A hook run when org-tree-slide--play is evaluated to start the slide show")
(defvar org-tree-slide-mode-stop-hook nil
  "[obsolate] A hook run when org-tree-slide--stop is evaluated to stop the slide show")
(defvar org-tree-slide-mode-before-narrow-hook nil
  "[obsolate] A hook run before evaluating org-tree-slide--display-tree-with-narrow")
(defvar org-tree-slide-mode-after-narrow-hook nil
  "[obsolate] A hook run after evaluating org-tree-slide--display-tree-with-narrow")

;; Updated hooks
(defvar org-tree-slide-play-hook nil
  "A hook run when org-tree-slide--play is evaluated to start the slide show")
(defvar org-tree-slide-stop-hook nil
  "A hook run when org-tree-slide--stop is evaluated to stop the slide show")
(defvar org-tree-slide-before-narrow-hook nil
  "A hook run before evaluating org-tree-slide--display-tree-with-narrow")
(defvar org-tree-slide-after-narrow-hook nil
  "A hook run after evaluating org-tree-slide--display-tree-with-narrow")
(defvar org-tree-slide-before-move-next-hook nil
  "A hook run before moving to the next slide")
(defvar org-tree-slide-before-move-previous-hook nil
  "A hook run before moving to the previous slide")

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
  :lighter (:eval (org-tree-slide--update-modeline))
  :keymap org-tree-slide-mode-map
  :group 'org-tree-slide
  :require 'org
  (if org-tree-slide-mode
      (org-tree-slide--setup)
    (org-tree-slide--abort)))

;;;###autoload
(defun org-tree-slide-play-with-timer ()
  "Start slideshow with setting a count down timer."
  (interactive)
  (org-timer-set-timer)
  (unless (org-tree-slide--active-p)
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
  (when (org-tree-slide--active-p)
    (org-tree-slide--hide-slide-header)
    (org-tree-slide--move-to-the-first-heading)
    (org-overview)
    (org-content (if (> org-tree-slide-skip-outline-level 0)
                     (1- org-tree-slide-skip-outline-level)))
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
  `org-tree-slide-skip-comments'     => t
"
  (interactive)
  (setq org-tree-slide-header nil)
  (setq org-tree-slide-slide-in-effect nil)
  (setq org-tree-slide-heading-emphasis nil)
  (setq org-tree-slide-cursor-init t)
  (setq org-tree-slide-modeline-display nil)
  (setq org-tree-slide-skip-done nil)
  (setq org-tree-slide-skip-comments t)
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
  `org-tree-slide-skip-comments'     => t
"
  (interactive)
  (setq org-tree-slide-header t)
  (setq org-tree-slide-slide-in-effect t)
  (setq org-tree-slide-heading-emphasis nil)
  (setq org-tree-slide-cursor-init t)
  (setq org-tree-slide-modeline-display 'outside)
  (setq org-tree-slide-skip-done nil)
  (setq org-tree-slide-skip-comments t)
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
  `org-tree-slide-skip-comments'     => t
"
  (interactive)
  (setq org-tree-slide-header nil)
  (setq org-tree-slide-slide-in-effect nil)
  (setq org-tree-slide-heading-emphasis nil)
  (setq org-tree-slide-cursor-init nil)
  (setq org-tree-slide-modeline-display 'lighter)
  (setq org-tree-slide-skip-done t)
  (setq org-tree-slide-skip-comments t)
  (message "narrowing control profile: ON"))

;;;###autoload
(defun org-tree-slide-display-header-toggle ()
  "Toggle displaying the slide header"
  (interactive)
  (setq org-tree-slide-header (not org-tree-slide-header))
  (unless org-tree-slide-header
    (org-tree-slide--hide-slide-header))
  (org-tree-slide--display-tree-with-narrow))

;;;###autoload
(defun org-tree-slide-slide-in-effect-toggle ()
  "Toggle using slide-in effect"
  (interactive)
  (setq org-tree-slide-slide-in-effect (not org-tree-slide-slide-in-effect))
  (org-tree-slide--display-tree-with-narrow))

;;;###autoload
(defun org-tree-slide-heading-emphasis-toggle ()
  "Toggle applying emphasis to heading"
  (interactive)
  (setq org-tree-slide-heading-emphasis (not org-tree-slide-heading-emphasis))
  (org-tree-slide--apply-custom-heading-face org-tree-slide-heading-emphasis))

(defvar org-tree-slide--previous-line 0)
;;;###autoload
(defun org-tree-slide-skip-done-toggle ()
  "Toggle show TODO item only or not"
  (interactive)
  (setq org-tree-slide-skip-done (not org-tree-slide-skip-done))
  (setq org-tree-slide--previous-line -1) ; to update modeline intentionally
  (org-tree-slide--show-slide-header)
  (if org-tree-slide-skip-done
      (message "TODO Pursuit: ON") (message "TODO Pursuit: OFF")))

;;;###autoload
(defun org-tree-slide-skip-comments-toggle ()
  "Toggle show COMMENT item or not"
  (interactive)
  (setq org-tree-slide-skip-comments (not org-tree-slide-skip-comments))
  (if org-tree-slide-skip-comments
      (message "COMMENT: HIDE") (message "COMMENT: SHOW")))

(defun org-tree-slide-move-next-tree ()
  "Display the next slide"
  (interactive)
  (when (org-tree-slide--active-p)
    (unless (equal org-tree-slide-modeline-display 'outside)
      (message "   Next >>"))
    (cond
     ((or
       (or (and (org-tree-slide--before-first-heading-p)
                (not (org-at-heading-p)))
           (and (= (point-at-bol) 1) (not (org-tree-slide--narrowing-p))))
       (or (org-tree-slide--first-heading-with-narrow-p)
           (not (org-at-heading-p))))
      (run-hooks 'org-tree-slide-before-move-next-hook)      
      (widen)
      (org-tree-slide--outline-next-heading))
     ;; stay the same slide (for CONTENT MODE, on the subtrees)
     (t nil))
    ;;    (when (and org-tree-slide-skip-done (looking-at (concat "^\\*+ " org-not-done-regexp))) (org-clock-in) )
    (org-tree-slide--display-tree-with-narrow)))

(defun org-tree-slide-move-previous-tree ()
  "Display the previous slide"
  (interactive)
  (when (org-tree-slide--active-p)
    (unless (equal org-tree-slide-modeline-display 'outside)
      (message "<< Previous"))
    (org-tree-slide--hide-slide-header)		; for at the first heading
    (run-hooks 'org-tree-slide-before-move-previous-hook)
    (widen)
    (cond
     ((org-tree-slide--before-first-heading-p)
      (message "before first heading (org-tree-slide)" ))
     ((not (org-at-heading-p))
      (org-tree-slide--outline-previous-heading)
      (org-tree-slide--outline-previous-heading))
     (t (org-tree-slide--outline-previous-heading)))
    ;;    (when (and org-tree-slide-skip-done (looking-at (concat "^\\*+ " org-not-done-regexp))) (org-clock-in) )
    (org-tree-slide--display-tree-with-narrow)
    ;; To avoid error of missing header in Emacs24
    (if (= emacs-major-version 24)
        (goto-char (point-min)))))

;;; Internal functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar org-tree-slide--slide-number nil)
(make-variable-buffer-local 'org-tree-slide--slide-number)
(setq-default org-tree-slide--slide-number " TSlide")

(defun org-tree-slide--line-number-at-pos ()
  (save-excursion
    (save-restriction
      (widen)
      (line-number-at-pos))))

(defun org-tree-slide--update-modeline ()
  (when (org-tree-slide--active-p)
    (cond
     ((equal org-tree-slide-modeline-display 'lighter)
      (setq org-tree-slide--slide-number
            (format " %s" (org-tree-slide--count-slide (point))))
      (setq org-tree-slide--previous-line (org-tree-slide--line-number-at-pos))
      org-tree-slide--slide-number)
     ;; just return the current org-tree-slide--slide-number quickly.
     ((equal org-tree-slide-modeline-display 'outside)
      org-tree-slide--slide-number)
     (t " TSlide"))))

(defvar org-tree-slide--header-overlay nil
  "Flag to check the status of overlay for a slide header.")

(defun org-tree-slide--setup ()
  (when (org-tree-slide--active-p)
    (org-tree-slide--play)))

(defun org-tree-slide--abort ()
  (when (equal major-mode 'org-mode)
    (org-tree-slide--stop)))

(defun org-tree-slide--play ()
  "Start slide view with the first tree of the org-mode buffer."
  (run-hooks 'org-tree-slide-mode-play-hook)
  (run-hooks 'org-tree-slide-play-hook)
  (org-tree-slide--apply-local-header-to-slide-header)
  (when org-tree-slide-heading-emphasis
    (org-tree-slide--apply-custom-heading-face t))
  (when (or org-tree-slide-cursor-init (org-tree-slide--before-first-heading-p))
    (org-tree-slide--move-to-the-first-heading))
  (org-tree-slide--beginning-of-tree)
  (when (org-tree-slide--heading-skip-p)
    (org-tree-slide--outline-next-heading))
  (org-tree-slide--display-tree-with-narrow)
  (when org-tree-slide-activate-message
    (message "%s" org-tree-slide-activate-message)))

(defvar org-tree-slide-startup "overview"
  "If you have `#+STARTUP:' line in your org buffer, the org buffer will
   be shown with corresponding status (content, showall, overview:default).")

(defun org-tree-slide--stop ()
  "Stop the slide view, and redraw the org-mode buffer with #+STARTUP:."
  (widen)
  (org-show-siblings)
  (when (or org-tree-slide-cursor-init (org-tree-slide--before-first-heading-p))
    (goto-char (point-min))
    (org-overview)
    (cond ((equal "content" org-tree-slide-startup)
           (message "CONTENT: %s" org-tree-slide-startup)
           (org-content))
          ((equal "showall" org-tree-slide-startup)
           (message "SHOW ALL: %s" org-tree-slide-startup)
           (org-cycle '(64)))
          (t nil)))
  (org-tree-slide--hide-slide-header)
  (when org-timer-start-time
    (org-timer-stop))
  (when org-tree-slide-heading-emphasis
    (org-tree-slide--apply-custom-heading-face nil))
  ;;  (when (and org-tree-slide-skip-done (looking-at (concat "^\\*+ " org-not-done-regexp))) (when (org-clocking-p) (org-clock-out) ) )
  (run-hooks 'org-tree-slide-mode-stop-hook)
  (run-hooks 'org-tree-slide-stop-hook)
  (when org-tree-slide-deactivate-message
    (message "%s" org-tree-slide-deactivate-message)))

(defun org-tree-slide--display-tree-with-narrow ()
  "Show a tree with narrowing and also set a header at the head of slide."
  (run-hooks 'org-tree-slide-mode-before-narrow-hook)
  (run-hooks 'org-tree-slide-before-narrow-hook)
  (when (equal org-tree-slide-modeline-display 'outside)
    (setq org-tree-slide--slide-number
          (format " %s" (org-tree-slide--count-slide (point))))
    (setq org-tree-slide--previous-line (org-tree-slide--line-number-at-pos)))
  (goto-char (point-at-bol))
  (unless (org-tree-slide--before-first-heading-p)
    (hide-subtree)	; support CONTENT (subtrees are shown)
    (org-show-entry)
    ;; If this is the last level to be displayed, show the full content
    (if (and (not org-tree-slide-fold-subtrees-skipped)
             (org-tree-slide--heading-level-skip-p (1+ (org-outline-level))))
        (org-tree-slide--show-subtree)
      (show-children))
    ;;    (org-cycle-hide-drawers 'all) ; disabled due to performance reduction
    (org-narrow-to-subtree))
  (when org-tree-slide-slide-in-effect
    (org-tree-slide--slide-in org-tree-slide-slide-in-brank-lines))
  (when org-tree-slide-header
    (org-tree-slide--show-slide-header))
  (run-hooks 'org-tree-slide-after-narrow-hook)
  (run-hooks 'org-tree-slide-mode-after-narrow-hook))

(defun org-tree-slide--show-subtree ()
  "Hide all subtrees that "
  (interactive "P")
  (save-excursion
    (outline-back-to-heading)
    (outline-map-region
     (lambda ()
       (if (org-tree-slide--heading-skip-comment-p)
           (org-tree-slide--hide-subtree)
         (show-subtree)))
     (point)
     (progn (outline-end-of-subtree)
            (if (eobp) (point-max) (1+ (point)))))))



(defun org-tree-slide--hide-subtree ()
  "Hides subtree. The headline is revealed if org-tree-slide-hide-subtrees-commented is nil, else the headline is hidden"
  (if org-tree-slide-hide-subtrees-commented
      (let* ((from (progn (outline-back-to-heading) (point)))
             (to (progn (outline-end-of-subtree) (point)))
             (temp (remove-overlays from to 'invisible 'outline))
             (o (make-overlay from to nil)))
        ;;(overlay-put o 'evaporate t)
        (overlay-put o 'invisible 'outline)
        (overlay-put o 'isearch-open-invisible
                     (or outline-isearch-open-invisible-function
                         'outline-isearch-open-invisible))
        )
    (hide-subtree)
    ))


(defun org-tree-slide--outline-next-heading ()
  (org-tree-slide--outline-select-method
   (org-tree-slide--outline-skip-type
    (if (outline-next-heading) t 'last)
    (org-outline-level))
   'next))

(defun org-tree-slide--outline-previous-heading ()
  (org-tree-slide--outline-select-method
   (org-tree-slide--outline-skip-type
    (if (outline-previous-heading) t 'first)
    (org-outline-level))
   'previous))

(defvar org-tree-slide--all-skipped t
  "A flag to know if all trees are skipped")

(defun org-tree-slide--outline-select-method (action direction)
  (cond ((and (equal action 'last) (equal direction 'next))
         (unless org-tree-slide--all-skipped
           (org-tree-slide--outline-previous-heading)))  ; Return back.
        ((and (equal action 'first) (equal direction 'previous))
         (unless org-tree-slide--all-skipped
           (org-tree-slide--move-to-the-first-heading))) ; Stay first heading
        ((and (equal action 'skip) (equal direction 'next))
         (org-tree-slide--outline-next-heading))      ; recursive call
        ((and (equal action 'skip) (equal direction 'previous))
         (org-tree-slide--outline-previous-heading))  ; recursive call
        (t 
         (setq org-tree-slide--all-skipped nil)
         nil)))

(defun org-tree-slide--heading-skip-p ()
  "This method assume the cursor exist at the heading.
** COMMENT         ; t
   hoge            ; nil
   hoge            ; nil
*** hoge           ; nil
"
  (or (or (org-tree-slide--heading-done-skip-p)
          (org-tree-slide--heading-level-skip-p))
      (org-tree-slide--heading-skip-comment-p)))

(defun org-tree-slide--heading-level-skip-p (&optional level)
  (and (> org-tree-slide-skip-outline-level 0)
       (<= org-tree-slide-skip-outline-level (or level (org-outline-level)))))

(defun org-tree-slide--heading-done-skip-p ()
  (and org-tree-slide-skip-done
       (not
        (looking-at
         ;; 6.33x does NOT support org-outline-regexp-bol
         (concat "^\\*+ " org-not-done-regexp)))))

(defun org-tree-slide--heading-skip-comment-p ()
  (and org-tree-slide-skip-comments
       (looking-at (concat "^\\*+ " org-comment-string))))

(defun org-tree-slide--outline-skip-type (has-target-outline current-level)
  (cond ((equal has-target-outline 'last) 'last)
        ((equal has-target-outline 'first) 'first)
        ((org-tree-slide--heading-skip-p) 'skip)
        (t nil)))

(defun org-tree-slide--slide-in (brank-lines)
  (while (< 2 brank-lines)
    (org-tree-slide--set-slide-header brank-lines)
    (sit-for org-tree-slide-slide-in-waiting)
    (org-tree-slide--hide-slide-header)
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

(defun org-tree-slide--apply-local-header-to-slide-header ()
  (save-excursion
    (org-tree-slide--move-to-the-first-heading)
    (let ((limit (point)))
      (org-tree-slide--set-header-var-by-regxep
       'org-tree-slide-title "#\\+TITLE:[ \t]*\\(.*\\)$" limit)
      (org-tree-slide--set-header-var-by-regxep
       'org-tree-slide-author "#\\+AUTHOR:[ \t]*\\(.*\\)$" limit)
      (org-tree-slide--set-header-var-by-regxep
       'org-tree-slide-email "#\\+EMAIL:[ \t]*\\(.*\\)$" limit)
      (org-tree-slide--set-header-var-by-regxep
       'org-tree-slide-startup "#\\+STARTUP:[ \t]*\\(.*\\)$" limit))))

(defun org-tree-slide--set-header-var-by-regxep (header-variable regexp limit)
  (goto-char 1)
  (set header-variable
       (if (re-search-forward regexp limit t) (match-string 1) nil)))

(defface org-tree-slide-header-overlay-face
  '((((class color) (background dark))
     (:bold t :foreground "white" :background "black"))
    (((class color) (background light))
     (:bold t :foreground "black" :background "white"))
    (t (:bold t :foreground "black" :background "white")))
  "Face for org-tree-slide--header-overlay")

(defun org-tree-slide--set-slide-header (brank-lines)
  (org-tree-slide--hide-slide-header)
  (setq org-tree-slide--header-overlay
        (make-overlay (point-min) (+ 1 (point-min))))
  (overlay-put org-tree-slide--header-overlay 'after-string " ")
  (overlay-put org-tree-slide--header-overlay
               'face
               'org-tree-slide-header-overlay-face)
  (if org-tree-slide-header
      (overlay-put org-tree-slide--header-overlay 'display
                   (concat (if org-tree-slide-title org-tree-slide-title
                             (buffer-name))
                           "\n"
                           (format-time-string "%Y-%m-%d") "  "
                           (when org-tree-slide-author
                             (concat org-tree-slide-author "  "))
                           (when org-tree-slide-email
                             (concat "<" org-tree-slide-email ">"))
                           (org-tree-slide--get-brank-lines brank-lines)))
    (overlay-put org-tree-slide--header-overlay 'display
                 (org-tree-slide--get-brank-lines brank-lines))))

(defun org-tree-slide--get-brank-lines (lines)
  (let ((breaks ""))
    (while (< 0 lines)
      (setq lines (1- lines))
      (setq breaks (concat breaks "\n")))
    breaks))

(defun org-tree-slide--show-slide-header ()
  (org-tree-slide--set-slide-header 2)
  (forward-char 1))

(defun org-tree-slide--hide-slide-header ()
  (when org-tree-slide--header-overlay
    (delete-overlay org-tree-slide--header-overlay)))

(defun org-tree-slide--move-to-the-first-heading ()
  (setq org-tree-slide--all-skipped t)
  (widen)
  (goto-char 1)
  (unless (looking-at "^\\*+ ")
    (outline-next-heading))
  (when (org-tree-slide--heading-skip-p)
    (setq org-tree-slide--all-skipped t)
    (org-tree-slide--outline-next-heading)))

(defun org-tree-slide--apply-custom-heading-face (status)
  "Change status of heading face."
  (unless org-tree-slide-never-touch-face
    (cond (status
           (custom-set-faces
            '(org-level-2 ((t (:inherit org-tree-slide-heading-level-2))))
            '(org-level-3 ((t (:inherit org-tree-slide-heading-level-3))))))
          (t
           (custom-set-faces
            '(org-level-2 ((t (:inherit org-tree-slide-heading-level-2-init))))
            '(org-level-3 ((t (:inherit org-tree-slide-heading-level-3-init)))))
           ))))

(defun org-tree-slide--count-slide (&optional pos)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((count 0)
            (current-slide 0)
            (current-point (or pos (point))))
        (when (and (looking-at "^\\*+ ") (not (org-tree-slide--heading-skip-p)))
          (setq count 1)
          (setq current-slide 1))
        (while (outline-next-heading)
          (when (not (org-tree-slide--heading-skip-p))
            (setq count (1+ count))
            (when (>= current-point (point))
              (setq current-slide (1+ current-slide)))))
        (cond
         ((= count 0) "[-/-]") ; no headings
         ((= current-slide 0) (format "[-/%d]" count)) ; before first heading
         (t
          (format "[%d/%d]" current-slide count)))))))

(defun org-tree-slide--active-p ()
  (and org-tree-slide-mode (equal major-mode 'org-mode)))

(defun org-tree-slide--narrowing-p ()
  "Check the current status if narrowing or not"
  (not (and (= (point-min) 1) (= (point-max) (1+ (buffer-size))))))

(defun org-tree-slide--before-first-heading-p ()
  "Extension of org-before-first-heading-p to support org 6.33x.
#+TITLE: title     ; t
#+STARTUP: content ; t
* first            ; t
  hoge             ; nil
** second          ; nil
** third           ; nil
"
  (and (org-before-first-heading-p) (not (org-tree-slide--narrowing-p))))

(defun org-tree-slide--first-heading-with-narrow-p ()
  "Check the current point is on the first heading with narrowing.
** first           ; t
   hoge            ; nil
   hoge            ; nil
*** second         ; nil
    hoge           ; nil
*** third          ; nil
"
  (and (org-tree-slide--narrowing-p) (= (point-at-bol) (point-min))))

(defun org-tree-slide--last-tree-p (target)
  "Check if the target point is in the last heading or it's body.
** n-1             ; nil
** n               ; t
   hoge            ; t
"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char target)
      (if (org-tree-slide--beginning-of-tree)
          (= (point) (org-tree-slide--last-heading-position))
        nil))))

(defun org-tree-slide--last-heading-position ()
  "Return the position of the last heading. If the position does not exist in the buffer, then return nil."
  (save-excursion
    (save-restriction
      (goto-char (buffer-size))
      (org-tree-slide--beginning-of-tree))))

(defun org-tree-slide--beginning-of-tree ()
  "Return beginning point of the line, or t. If the position does not exist in the buffer, then return nil."
  (beginning-of-line)
  (if (org-at-heading-p)
      (point)
    (outline-previous-heading))) ; return position or nil.

(provide 'org-tree-slide)

;;; org-tree-slide.el ends here
