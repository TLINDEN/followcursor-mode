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

;; Version: 0.02
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

;; You  will  be   asked  for  a  `thing-at-point'  to   be  used  for
;; highlighting,  the  default  being  `lazyword',  which  is  a  mode
;; indepenent  thing matching  words including  some punctuation.  You
;; may, however, use any other symbol from the list.

;; Move around in that buffer as you wish. Whenever `point' is on a
;; match, it will be highlighted in both buffers.

;;; Code:

;;;; Deps

(require 'hi-lock)
(require 'thingatpt)

;;;; Customizables

(defgroup followcursor nil
  "Follow Cursoe Mode."
  :prefix "followcursor-"
  :group 'emacs)

(defcustom followcursor-highlight-there 'highlight-lines-matching-regexp
  ;; also: highlight-regexp
  "Function used to highlight matching things in there other buffer."
  :group 'followcursor)

(defcustom followcursor-highlight-here 'highlight-regexp
  "Function used to highlight matching things in there current buffer."
  :group 'followcursor)

(defcustom followcursor-mark-thing 'lazyword
  "THING at point to highlight.

The default thing, `lazyword', is defined by `followcursor-mode' for
convenience.

For a list of THINGS, read the documentation of `thing-at-point'."
  :group 'followcursor)

(defcustom followcursor-available-things
  '(word lazyword symbol list filename url email line)
  "THINGs usable with `thing-at-point' to be prompted for.

If you define your own THINGs, add them here."
  :group 'followcursor)

;;;; Variables

(make-variable-buffer-local
 (defvar followcursor--last-point nil
   "Buffer local variable storing last recorded cursor position."))

(make-variable-buffer-local
 (defvar followcursor-previous-thing nil
   "Buffer local variable storing last thing at point, if any"))

(make-variable-buffer-local
 (defvar followcursor-font-lock-here nil
   "font-lock-mode enabled before followcursor-mode?"))

(make-variable-buffer-local
 (defvar followcursor-font-lock-there nil
   "font-lock-mode enabled before followcursor-mode?"))

;;;; Internal Functions

(defun followcursor--setup-ok ()
  "Indicate if it is ok to run followcursor.
Return t if there are currently 2 visible windows."
  (eq (count-windows) 2))

(defun followcursor--get-thing-at-point (thing)
  "Return THING thing-at-point.
Just a wrapper to omit string properties, if any."
  (interactive)
  (thing-at-point thing t))

(defun followcursor--mark-and-highlight (mark-what hl-here hl-there)
  "Marks thing at point using form ''WHAT and highlights using form ''WHICH."
  (interactive)
  (when (followcursor--setup-ok)
    (let ((thing (followcursor--get-thing-at-point mark-what)))
      (if thing
        (unless (eq thing followcursor-previous-thing)
          ;; highlight here
          (hi-lock-mode 0)
          (funcall hl-here thing)
          (font-lock-ensure (point-min) (point-max))
          (other-window 1)
          ;; highlight there
          (hi-lock-mode 0)
          (funcall hl-there thing)
          (font-lock-ensure (point-min) (point-max))
          ;; go to here buffer
          (other-window 1)
          (setq followcursor-previous-thing thing))
        (followcursor-remove-highlights)))))

(defun followcursor--enable-font-lock ()
  "Enable font-lock, if not yet enabled.
Store current state."
  (unless font-lock-mode
    (setq followcursor-font-lock-here t)
    (font-lock-mode))
  (other-window 1)
  (unless font-lock-mode
    (setq followcursor-font-lock-there t)
    (font-lock-mode))
  (other-window 1))

(defun followcursor--disable-font-lock ()
  "Disable font-lock if it were disabled previously."
  (when followcursor-font-lock-here
    (font-lock-mode nil)
    (setq followcursor-font-lock-here nil))
  (other-window 1)
  (when followcursor-font-lock-there
    (font-lock-mode nil)
    (setq followcursor-font-lock-there nil))
  (other-window 1))

;;;; Thing-at-Point wrapper[s]

;; Thing[s] used  as the default,  so that  we don' depend  on current
;; mode's syntax  table. However,  IF the user  want's to  use current
;; mode, she  can just  select another thing  on startup  or configure
;; one.

(defun lazyword-bounds-of-lazyword-at-point ()
  "Return the start and end points of a lazyword at the current point."
  (let ((pattern "[:alnum:][:punct:]"))
    (save-excursion
      (skip-chars-backward pattern)
      (if (looking-at (concat "[" pattern "]+"))
          (cons (point) (match-end 0))
        nil))))

(put 'lazyword 'bounds-of-thing-at-point
     'lazyword-bounds-of-lazyword-at-point)

(defun followcursor--ask-for-thing-type ()
  "Ask with completion for THING to be used for highlighting."
  (interactive)
  (let ((sym (completing-read
              (concat "which thing to mark? ["
                      (symbol-name followcursor-mark-thing) "] ")
              followcursor-available-things)))
    (if (string= "" sym)
        (symbol-name followcursor-mark-thing)
      sym)))

;;;; Public Functions

(defun followcursor-remove-highlights ()
  (interactive)
  (hi-lock-mode 0)
  (other-window 1)
  (hi-lock-mode 0)
  (other-window 1))

(defun followcursor-mode-enable ()
  "Enable followcursor-mode."
  (interactive)
  (add-hook 'post-command-hook 'followcursor-mark-when-moved)
  (hi-lock-mode 0)
  (setq followcursor--last-point (point)
        followcursor-mark-thing (intern (followcursor--ask-for-thing-type)))
  (followcursor--enable-font-lock)
  (followcursor-mark-when-moved))

(defun followcursor-mode-disable ()
  "Disable followcursor-mode."
  (interactive)
  (remove-hook 'post-command-hook 'followcursor-mark-when-moved)
  (followcursor-remove-highlights)
  (followcursor--disable-font-lock))

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

