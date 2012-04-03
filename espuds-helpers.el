;;; espuds-helpers.el --- Helper functions


(defun espuds-fake-eval (contents)
  "Dump contents to a temp file and then load it."
  (let ((file (make-temp-file "ecukes-")))
    (with-temp-file file
      (insert contents))
    (load file)))

(defun espuds-buffer-contents ()
  "Return all text in current buffer."
  (buffer-string))

(defun espuds-region ()
  "Return the text selected by region."
  (buffer-substring-no-properties (region-beginning) (region-end)))


(provide 'espuds-helpers)

;;; espuds-helpers.el ends here
