;; These functions shall be loaded for Emacs 25.1 or earlier.
;; outline-show-children <- show-children
;; outline-show-subtree <- show-subtree
;; outline-hide-subtree <- hide-subtree

(defun outline-show-children (&optional level)
  "Show all direct subheadings of this heading.
Prefix arg LEVEL is how many levels below the current level should be shown.
Default is enough to cause the following heading to appear."
  (interactive "P")
  (setq level
	(if level (prefix-numeric-value level)
	  (save-excursion
	    (outline-back-to-heading)
	    (let ((start-level (funcall outline-level)))
	      (outline-next-heading)
	      (if (eobp)
		  1
		(max 1 (- (funcall outline-level) start-level)))))))
  (let (outline-view-change-hook)
    (save-excursion
      (outline-back-to-heading)
      (setq level (+ level (funcall outline-level)))
      (outline-map-region
       (lambda ()
	 (if (<= (funcall outline-level) level)
	     (outline-show-heading)))
       (point)
       (progn (outline-end-of-subtree)
	      (if (eobp) (point-max) (1+ (point)))))))
  (run-hooks 'outline-view-change-hook))


(defun outline-show-subtree (&optional event)
  "Show everything after this heading at deeper levels.
If non-nil, EVENT should be a mouse event."
  (interactive (list last-nonmenu-event))
  (save-excursion
    (when (mouse-event-p event)
      (mouse-set-point event))
    (outline-flag-subtree nil)))

(defun outline-hide-subtree (&optional event)
  "Hide everything after this heading at deeper levels.
If non-nil, EVENT should be a mouse event."
  (interactive (list last-nonmenu-event))
  (save-excursion
    (when (mouse-event-p event)
      (mouse-set-point event))
    (outline-flag-subtree t)))
