;;; espuds-helpers.el --- Helper functions


(defun espuds-fake-eval (contents)
  "Dumps contents to a temp file and then loads it."
  (let ((file (make-temp-file "ecukes-")))
    (with-temp-file file
      (insert contents))
    (load file)))

(defun espuds-buffer-contents ()
  "Returns all text in current buffer."
  (buffer-string))

(defun espuds-region ()
  "Returns the text selected by region."
  (buffer-substring-no-properties (region-beginning) (region-end)))


(provide 'espuds-helpers)

;;; espuds-helpers.el ends here
