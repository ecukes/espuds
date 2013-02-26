;;; espuds-buffer.el --- Buffer related definitions

(eval-when-compile
  (require 'cl))
(require 's)

(Given "^\\(?:I am in buffer\\|I switch to buffer\\) \"\\(.+\\)\"$"
  "Switches to BUFFER.

Examples:
 - When I switch to buffer \"Foo\"
 - Given I am in buffer \"*scratch*\""
  (lambda (buffer)
    (if (s-matches? "\\s-" buffer)
        (switch-to-buffer buffer)
      (let ((v (vconcat [?\C-x ?b] (string-to-vector buffer))))
        (execute-kbd-macro v)))))

(Then "^I should be in buffer \"\\(.+\\)\"$"
  "Asserts that the current buffer is BUFFER.

Examples:
 - Then I should be in buffer \"*scratch*\""
  (lambda (buffer)
    (let ((message "Expected to be in buffer '%s', but was in '%s'"))
      (assert (equal buffer (buffer-name)) nil message buffer (buffer-name)))))

(Then "^I should be in file \"\\(.+\\)\"$"
  "Asserts that the current buffer is connected to FILE.

Examples:
 - Then I should be in file \"/path/to/some/file\""
  (lambda (file)
    (let ((file-name (buffer-file-name)))
      (if file-name
          (let ((match (equal file (file-name-nondirectory file-name))))
            (assert match nil "Expected file to be '%s', but was '%s'." file file-name))
        (assert file-name nil "Expected file to be '%s', but not visiting any file." file)))))

(Given "^the buffer is empty$\\|^I clear the buffer$"
  "Clears all text in the current buffer.

Examples:
 - Given the buffer is empty
 - When I clear the buffer"
  (lambda ()
    (erase-buffer)))


(provide 'espuds-buffer)

;;; espuds-buffer.el ends here
