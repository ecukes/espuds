;;; espuds-helpers.el --- Helper functions


(defun espuds-fake-eval (contents)
  "Dumps contents to a temp file and then loads it."
  (let ((file (make-temp-file "ecukes-")))
    (with-temp-file file
      (insert contents))
    (load file)))

(defun espuds-buffer-contents ()
  "Returns all text in current buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun espuds-region ()
  "Returns the text selected by region."
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun should-or-should-not-see (see expected compare-fn)
  "Asserts that buffer text matches or not matches (depending on see)
EXPECTED with COMPARE-FN."
  (let* ((actual (espuds-buffer-contents))
         (found (funcall compare-fn expected actual)))
    (assert
     (if (string-match-p "not" see) (not found) found) nil
     (concat
      "Expected \"" expected "\", but was  \"" actual "\""))))


(provide 'espuds-helpers)

;;; espuds-helpers.el ends here
