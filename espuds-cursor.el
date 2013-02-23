;;; espuds-cursor.el --- Cursor related definitions

(eval-when-compile
  (require 'cl))
(require 'espuds-helpers)

(When "^I go to line \"\\([0-9]+\\)\"$"
  "Goes to LINE if it exist.

Examples:
 - When I go to line \"12\""
 (lambda (line)
   (let ((num-lines (count-lines (point-min) (point-max)))
         (line-num (string-to-number line))
         (message "Requested line '%s', but buffer only has '%d' line(s)."))
     (assert (<= line-num num-lines) nil message line num-lines)
     (espuds-goto-line line-num))))

(When "^I go to point \"\\([0-9]+\\)\"$"
  "Goes to POINT if it exist.

Examples:
 - When I go to point \"12\""
 (lambda (point)
   (let ((size (buffer-size))
         (point-num (string-to-number point))
         (message "Requested point '%s', but buffer only has '%d' point(s)."))
     (assert (<= (1- point-num) size) nil message point-num size)
     (goto-char point-num))))

(When "^I go to word \"\\(.+\\)\"$"
  "Go to WORD if it exist.

Examples:
 - When I go to word \"SOME WORD\""
  (lambda (word)
    (goto-char (point-min))
    (let ((search (re-search-forward (format "\\b%s\\b" word) nil t))
          (message "Can not go to word '%s' since it does not exist in the current buffer: %s"))
      (assert search nil message word (espuds-buffer-contents)))
    (backward-char (length word))))

(Then "^the cursor should be at point \"\\(.+\\)\"$"
  "Checks that the cursor is at a specific position.

Examples:
 - Then the cursor should be at point \"12\""
  (lambda (point)
    (let ((message "Expected cursor to be at point '%s', but was at '%s'"))
      (assert (= (string-to-number point) (point)) nil message point (point)))))

(Then "^the cursor should be before \"\\(.+\\)\"$"
  "Checks that the cursor is before some text.

Examples:
 - Then the cursor should be before \"Foo\""
  (lambda (expected)
    (let ((actual
           (progn
             (buffer-substring-no-properties (point) (min (point-max) (+ (point) 5)))))
          (message "Expected '%s' to be before point but was '%s'."))
      (assert (looking-at (regexp-quote expected)) nil message expected actual))))

(Then "^the cursor should be after \"\\(.+\\)\"$"
  "Checks that the cursor is after some text.

Examples:
 - Then the cursor should be after \"Foo\""
  (lambda (expected)
    (let ((actual
           (progn
             (buffer-substring-no-properties (point) (max (point-min) (- (point) 5)))))
          (message "Expected '%s' to be after point but was '%s'."))
      (assert (looking-back (regexp-quote expected)) nil message expected actual))))

(Then "^the cursor should be between \"\\(.+\\)\" and \"\\(.+\\)\"$"
  "Checks that the cursor is between some text.

Examples:
 - Then the cursor should be between \"Foo\" and \"Bar\""
  (lambda (left right)
    (let ((search
           (and
            (looking-back (regexp-quote left))
            (looking-at (regexp-quote right))))
          (message "Expected '%s' to be left of point and '%s' to be right of point, but was: '%s[CURSOR]%s'")
          (before
           (buffer-substring-no-properties
            (max (point-min) (- (point) 5))
            (point)))
          (after
           (buffer-substring-no-properties
            (point)
            (min (point-max) (+ (point) 5)))))
      (assert search nil message left right before after))))

(When "^I place the cursor between \"\\(.+\\)\" and \"\\(.+\\)\"$"
  "Places the cursor between text.

Examples:
 - When I place the cursor between \"Foo\" and \"Bar\""
  (lambda (left right)
    (goto-char (point-min))
    (let ((search (search-forward (concat left right) nil t))
          (message "Can not place cursor between '%s' and '%s', because there is no such point: '%s'"))
      (assert search nil message left right (espuds-buffer-contents)))
    (backward-char (length right))))

(When "^I place the cursor before \"\\(.+\\)\"$"
  "Places the cursor before first instance of text.

Examples:
 - When I place the cursor before \"Foo\""
  (lambda (arg)
    (goto-char (point-min))
    (let ((search (search-forward arg nil t))
          (message "Can not place cursor before '%s', because there is no such point: '%s'"))
      (backward-char (length arg))
      (assert search nil message arg (espuds-buffer-contents)))))

(When "^I place the cursor after \"\\(.+\\)\"$"
  "Places the cursor after first instance of text.

Examples:
 - When I place the cursor after \"Foo\""
  (lambda (arg)
    (goto-char (point-min))
    (let ((search (search-forward arg nil t))
          (message "Can not place cursor after '%s', because there is no such point: '%s'"))
      (assert search nil message arg (espuds-buffer-contents)))))

(When "^I go to beginning of buffer$"
  "Places the cursor at the beginning of buffer.

Examples:
 - When I go to beginning of buffer"
  'beginning-of-buffer)

(When "^I go to end of buffer$"
  "Places the cursor at the end of buffer.

Examples:
 - When I go to end of buffer"
  'end-of-buffer)

(When "^I go to beginning of line$"
  "Places the cursor at the beginning of the line.

Examples:
 - When I go to beginning of line"
  'move-beginning-of-line)


(When "^I go to end of line$"
  "Places the cursor at the end of the line.

Examples:
 - When I go to end of line"
  'move-end-of-line)


(provide 'espuds-cursor)

;;; espuds-cursor.el ends here
