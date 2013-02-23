;;; espuds-text.el --- Text related definitions

(eval-when-compile
  (require 'cl))
(require 's)
(require 'espuds-helpers)

(When "^I insert\\(?: \"\\(.+\\)\"\\|:\\)$"
  "Inserts CONTENTS into the current buffer.

Examples:
 - When I insert \"CONTENTS\"
 - When I insert:
     \"\"\"
     CONTENTS
     \"\"\""
  (lambda (contents)
    (insert contents)))

(Then "^I should see\\(?: \"\\(.+\\)\"\\|:\\)$"
  "Asserts that the current buffer includes some text.

Examples:
 - Then I should see \"CONTENTS\"
 - Then I should see:
     \"\"\"
     CONTENTS
     \"\"\""
  (lambda (expected)
    (let ((actual (espuds-buffer-contents))
          (message "Expected '%s' to be part of '%s', but was not."))
      (assert (s-contains? expected actual) nil message expected actual))))

(Then "^I should not see\\(?: \"\\(.+\\)\"\\|:\\)$"
  "Asserts that the current buffer does not include some text.

Examples:
 - Then I should not see \"CONTENTS\"
 - Then I should not see:
     \"\"\"
     CONTENTS
     \"\"\""
  (lambda (expected)
    (let ((actual (espuds-buffer-contents))
          (message "Expected '%s' to not be part of '%s', but was."))
      (assert (not (s-contains? expected actual)) nil message expected actual))))

(Then "^I should see pattern\\(?: \"\\(.+\\)\"\\|:\\)$"
  "Asserts that the current buffer matches some text.

Examples:
 - Then I should see pattern \"CONTENTS\"
 - Then I should see pattern:
     \"\"\"
     CONTENTS
     \"\"\""
  (lambda (expected)
    (let ((actual (espuds-buffer-contents))
          (message "Expected to see pattern '%s' in '%s', but did not."))
      (assert
       (s-matches? expected actual) nil message expected actual))))

(Then "^I should not see pattern\\(?: \"\\(.+\\)\"\\|:\\)$"
  "Asserts that the current buffer does not match some text.

Examples:
 - Then I should not see pattern \"CONTENTS\"
 - Then I should not see pattern:
     \"\"\"
     CONTENTS
     \"\"\""
  (lambda (expected)
    (let ((actual (espuds-buffer-contents))
          (message "Expected to not see pattern '%s' in '%s', but did."))
      (assert
       (not (s-matches? expected actual)) nil message expected actual))))

(When "^I select \"\\(.+\\)\"$"
  "Selects TEXT if found. Otherwise signal an error.

Examples:
 - When I select \"SOME TEXT\""
  (lambda (text)
    (goto-char (point-min))
    (let ((search (re-search-forward text nil t)))
      (assert search nil "The text '%s' was not found in the current buffer." text))
    (set-mark (point))
    (re-search-backward text)))

(Then "^I should not see anything$\\|^the buffer should be empty$"
  "Asserts that there nothing to see in the current buffer.

Examples:
 - Then I should not see anything
 - Then the buffer should be empty"
  (lambda ()
    (let ((message "Expected buffer to be empty, but had content: '%s'"))
      (assert (equal (buffer-size) 0) nil message (espuds-buffer-contents)))))


(provide 'espuds-text)

;;; espuds-text.el ends here
