;;; followcursor-mode.el --- highlight line in other buffer containing word in current buffer

;; Copyright (C) 2017, T.v.Dein <tlinden@cpan.org>

;; This file is NOT part of Emacs.

;; This  program is  free  software; you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along  with  this program;  if  not,  write  to the  Free  Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Version: 0.01
;; Author: T.v.Dein <tlinden@cpan.org>
;; Keywords: files
;; URL: https://github.com/tlinden/followcursor
;; License: GNU General Public License >= 2

;;; Commentary:

;; To use, first add to your config:

;;     (require 'followcursor-mode

;; Then prepare two windows with different buffers, enable the mode
;; in one of them with:

;;     (followcursor-mode)

;; Move around in that buffer as you wish. Whenever `point' is on a
;; match, it will be highlighted in both buffers.

;;; Code:

;;;; Deps

(require 'hi-lock)

;;;; Customizables

(defgroup followcursor nil
  "Follow Cursoe Mode."
  :prefix "followcursor-"
  :group 'emacs)

(defcustom followcursor-highlight-there 'highlight-lines-matching-regexp
  ;; also: highlight-regexp
  "Function used to highlight matching things in there other buffer.")

(defcustom followcursor-highlight-here 'highlight-regexp
  "Function used to highlight matching things in there current buffer.")

(defcustom followcursor-mark-thing 'followcursor-mark-symbol-lazy
  "Custom mark function, must set region.
Use the default or someting like:
'(lambda() (thing-at-point 'word))")

;;;; Variables

(make-variable-buffer-local
 (defvar followcursor--last-point nil
   "Buffer local variable storing last recorded cursor position."))

(make-variable-buffer-local
 (defvar followcursor-previous-thing nil
   "Buffer local variable storing last thing at point, if any"))

;;;; Internal Functions

(defun followcursor--setup-ok ()
  "Indicate if it is ok to run followcursor.
Return t if there are currently 2 visible windows."
  (eq (count-windows) 2))

(defun followcursor--mark-and-highlight (mark-what hl-here hl-there)
  "Marks thing at point using form ''WHAT and highlights using form ''WHICH."
  (interactive)
  (when (followcursor--setup-ok)
    (let ((thing (funcall mark-what)))
      (if thing
        (unless (eq thing followcursor-previous-thing)
          ;; highlight here
          (hi-lock-mode 0)
          (funcall hl-here thing)
          (other-window 1)
          ;; highlight there
          (hi-lock-mode 0)
          (funcall hl-there thing)
          ;; go to here buffer
          (other-window 1)
          (setq followcursor-previous-thing thing))
        (followcursor-remove-highlights)))))

;;;; Public Functions

(defun followcursor-remove-highlights ()
  (interactive)
  (hi-lock-mode 0)
  (other-window 1)
  (hi-lock-mode 0)
  (other-window 1))

(defun followcursor-mark-symbol-lazy ()
  "Mark symbol at point, be tolerant about what constitutes a symbol."
  (interactive)
  (let ((thing nil)
        (beg nil)
        (end nil))
    (when (not (looking-at "[ \t\n]"))
      (save-excursion
        (unless (looking-back "[ \t\n]")
          (backward-word)
          (while (looking-back "[-_\.]")
            (backward-word)))
        (setq beg (point))
        (forward-word)
        (while (looking-at "[-_\.]")
          (forward-word))
        (setq end (point)))
      (unless (eq beg end)
        (buffer-substring-no-properties beg end)))))

(defun followcursor-mode-enable ()
  "Enable followcursor-mode."
  (interactive)
  (add-hook 'post-command-hook 'followcursor-mark-when-moved)
  (hi-lock-mode 0)
  (setq followcursor--last-point (point))
  (followcursor-mark-when-moved))

(defun followcursor-mode-disable ()
  "Disable followcursor-mode."
  (interactive)
  (remove-hook 'post-command-hook 'followcursor-mark-when-moved)
  (followcursor-remove-highlights))

;;;###autoload
(defun followcursor-mark-when-moved ()
  "`post-hook-command' which runs when `point' moves.
`followcursor-mode' must be enabled, only 2 windows must be visible.
Mark current `followcursor-mark-thing' using `followcursor-highlight-here'
and use `followcursor-highlight-there' for marking in the other window."
  (interactive)
  (unless (window-minibuffer-p)
    (when followcursor-mode
      (unless (eq this-command 'followcursor-mode)
        (unless (eq followcursor--last-point (point))
          (followcursor--mark-and-highlight
           followcursor-mark-thing
           followcursor-highlight-here
           followcursor-highlight-there))))))

;;;; Minor Mode

;;;###autoload
(define-minor-mode followcursor-mode
  "Highlight lines in window A containing thing-at-point in window B."

  :init-value nil
  :lighter "FC"
  :group "followcursor"
  (if followcursor-mode
      (followcursor-mode-enable)
    (followcursor-mode-disable)))

(provide 'followcursor-mode)

;;; followcursor-mode.el ends here

