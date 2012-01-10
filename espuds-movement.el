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

;; Checks that the cursor is after some text.
;;
;; Usage:
;;   Then the cursor should be after "Foo"
(Then "^the cursor should be after \"\\(.+\\)\"$"
      (lambda (right)
        (assert (looking-back (regexp-quote right)))))

;; Checks that the cursor is before some text.
;;
;; Usage:
;;   Then the cursor should be before "Foo"
(Then "^the cursor should be before \"\\(.+\\)\"$"
      (lambda (left)
        (assert (looking-at (regexp-quote left)))))

;; Checks that the cursor is between some text.
;;
;; Usage:
;;   Then the cursor should be between "Foo" and "Bar"
(Then "^the cursor should be between \"\\(.+\\)\" and \"\\(.+\\)\"$"
      (lambda (left right)
        (assert
         (and
          (looking-back (regexp-quote left))
          (looking-at (regexp-quote right))))))

;; Places the cursor between text.
;;
;; Usage:
;;   When I place the cursor between "Foo" and "Bar"
(When "^I place the cursor between \"\\(.+\\)\" and \"\\(.+\\)\"$"
      (lambda (left right)
        (goto-char (point-min))
        (assert (search-forward (concat left right) nil t))
        (backward-char (length right))))

;; Places the cursor at the beginning of buffer.
;;
;; Usage:
;;   When I go to beginning of buffer
(When "^I go to beginning of buffer$" 'beginning-of-buffer)

;; Places the cursor at the end of buffer.
;;
;; Usage:
;;   When I go to end of buffer
(When "^I go to end of buffer$" 'end-of-buffer)


(provide 'espuds-movement)

;;; espuds-movement.el ends here
