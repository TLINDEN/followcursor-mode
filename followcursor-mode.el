(require 'hi-lock)

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

;; FIXME: implement ;;;;;;;;;;;
(defcustom followcursor-ask-for-regex nil
  "If set to t, ask for regex on mode start to use for marking.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-variable-buffer-local
 (defvar followcursor--last-point nil
   "Buffer local variable storing last recorded cursor position."))

(make-variable-buffer-local
 (defvar followcursor-previous-thing nil
   "Buffer local variable storing last thing at point, if any"))

(defun followcursor--setup-ok ()
  "Indicate if it is ok to run followcursor.
Return t if there are currently 2 visible windows."
  (interactive)
  (if (eq (count-windows) 2)
      t
    nil))

(defun followcursor-remove-highlights ()
  (interactive)
  (hi-lock-mode 0)
  (other-window 1)
  (hi-lock-mode 0)
  (other-window 1))

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

(defun followcursor-mark-symbol-lazy ()
  "Mark symbol at point, be tolerant about what constitutes a symbol."
  (interactive)
  (let ((thing nil)
        (beg nil)
        (end nil))
    (when (not (looking-at "[ \t\n]"))
      (save-excursion
        (backward-word)
        (while (looking-back "[-_\.]")
          (backward-word))
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

(defun fcmh ()
  (interactive)
  (followcursor--mark-and-highlight
           followcursor-mark-thing
           followcursor-highlight-here
           followcursor-highlight-there)
  )

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

;; followcursor-mode.el ends here

