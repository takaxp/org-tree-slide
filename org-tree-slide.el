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
;;    v1.0.0 (2011-09-28@20:59) # Release an init version
;;
;;; Usage:
;;    1. Put this elisp into your load-path
;;    2. Add (requre 'org-tree-slide) in your .emacs
;;    3. Open an org-mode file, just type <right>/<left>, now you in slide view
;;
;;; Note:
;;    - Make sure key maps below when you introduce this elisp.

(defconst org-tree-slide "1.0.0"
  "The version number of the org-tree-slide.el")

(define-key org-mode-map (kbd "<right>") 'tree-slide-move-next-tree)
(define-key org-mode-map (kbd "<left>")  'tree-slide-move-previous-tree)
(define-key org-mode-map (kbd "C-c s p") 'tree-slide-play)
(define-key org-mode-map (kbd "C-c s s") 'tree-slide-stop)
;(define-key org-mode-map (kbd "<f5>") 'org-narrow-to-subtree)
;(define-key org-mode-map (kbd "<S-f5>") 'widen)

(defun tree-slide-move-next-tree ()
  "Show the next slide"
  (interactive)
  (if (org-before-first-heading-p) (outline-next-heading)
    (hide-subtree)
    (widen)
    (outline-next-heading))
  (tree-slide-display-tree-with-narrow))

(defun tree-slide-move-previous-tree ()
  "Show the previous slide"
  (interactive)
  (unless (org-before-first-heading-p)
    (hide-subtree)
    (widen)
    (outline-previous-heading)
    (tree-slide-display-tree-with-narrow)))

(defun tree-slide-play ()
  "Start slide view with the first tree of the org-mode buffer"
  (interactive)
  (move-to-the-first-heading)
  (tree-slide-display-tree-with-narrow))

(defun tree-slide-stop ()
  "Stop slide view, show the org-mode buffer with OVERVIEW"
  (interactive)
  (widen)
  (org-overview)
  (move-to-the-first-heading))

(defun tree-slide-display-tree-with-narrow ()
  (hide-subtree)
  (show-entry)
  (show-children)
  (org-cycle-hide-drawers 'all)
  (org-narrow-to-subtree))

(defun move-to-the-first-heading ()
  (widen)
  (goto-char (point-min))
  (when (org-before-first-heading-p)
    (outline-next-heading)))

(provide 'org-tree-slide)

;;; org-tree-slide.el ends here
