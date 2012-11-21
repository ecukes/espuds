;;; espuds-helpers.el --- Helper functions


(defun espuds-fake-eval (contents)
  "Dump contents to a temp file and then load it."
  (let ((file (make-temp-file "espuds-")))
    (with-temp-file file
      (insert contents))
    (load file)))

(defun espuds-buffer-contents ()
  "Return all text in current buffer."
  (buffer-string))

(defun espuds-region ()
  "Return the text selected by region, if any."
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    ""))

(defun espuds-quit ()
  "Quit without signal."
  (flet ((signal (&rest args) nil))
    (keyboard-quit)))

(defmacro save-column (&rest body)
  "Save current column; execute BODY; restore current column."
  `(let ((orig-column (current-column)))
     ,@body
     (move-to-column orig-column t)))

(defun espuds-previous-line (&optional n)
  "The function `previous-line' behave differently depending on
context. Use `forward-line' as a workaround."
  (or n (setq n (or n 1)))
  (if (> (line-number-at-pos (point)) n)
      (save-column
       (forward-line (- n)))
    (goto-char (point-min))))

(defun espuds-next-line (&optional n)
  "The function `next-line' behave differently depending on
context. Use `forward-line' as a workaround."
  (or n (setq n (or n 1)))
  (if (>
       (-
        (line-number-at-pos (point-max))
        (line-number-at-pos (point)))
       n)
      (save-column
       (forward-line n))
    (goto-char (point-max))))

(provide 'espuds-helpers)

;;; espuds-helpers.el ends here
