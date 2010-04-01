;;; espuds-movement.el --- Movement related definitions


;; Goes to LINE if it exist.
;;
;; Usage:
;;   When I go to line "12"
(When "^I go to line \"\\([0-9]+\\)\"$"
      (lambda (line)
        (let ((num-lines (count-lines (point-min) (point-max)))
              (line-num (string-to-number line)))
          (if (> line-num num-lines)
              (assert nil nil (concat "Tried to go to line " line ", but buffer does only have " (number-to-string num-lines) " lines."))
            (goto-line line-num)))))

;; Goes to POINT if it exist.
;;
;; Usage:
;;   When I go to point "12"
(When "^I go to point \"\\([0-9]+\\)\"$"
      (lambda (point)
        (let ((size (buffer-size)) (point-num (string-to-number point)))
          (if (> point-num size)
              (assert nil nil (concat "Tried to go to point " point ", but buffer does only have " (number-to-string size) " characters."))
            (goto-char point-num)))))

;; Goes to WORD if it exist and places the cursor in the middle of it.
;;
;; Usage:
;;   When I go to word "SOME WORD"
(When "^I go to word \"\\(.+\\)\"$"
      (lambda (word)
        (goto-char (point-min))
        (unless (re-search-forward (concat "\\b" word "\\b") nil t)
          (assert nil nil (concat "The word \"" word "\" does not exist in the current buffer.")))
        (backward-char (/ (length word) 2))))


(provide 'espuds-movement)

;;; espuds-movement.el ends here
