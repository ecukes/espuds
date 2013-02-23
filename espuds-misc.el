;;; espuds-misc.el --- Definitions that don't fit in any other file

(eval-when-compile
  (require 'cl))
(require 'espuds-helpers)

(When "^I turn on \\(.+\\)$"
  "Turns on some mode.

Examples:
 - When I turn on ruby-mode"
  (lambda (mode)
    (let ((v (vconcat [?\C-u 1 ?\M-x] (string-to-vector mode))))
      (execute-kbd-macro v))))

(When "^I set \\(.+\\) to \\(.+\\)$"
  "Set some variable

Examples:
 - When I set sentence-end-double-space to nil"
  (lambda (var val)
    (set (intern var) (read val))))

(When "^I load the following:$"
  "Loads CONTENTS with Emacs load command.

Examples:
 - When I load the following:
     \"\"\"
     CONTENTS
     \"\"\""
  (lambda (contents)
    (espuds-fake-eval contents)))

(When "^I open temp file \"\\(.+\\)\"$"
  "Creates a new temp file called FILE and opens it.

Examples:
 - When I open temp file \"SOME FILE\""
  (lambda (file)
    (find-file (make-temp-file file))))

(Then "^I should see message \"\\(.+\\)\"$"
  "Asserts that MESSAGE has been printed.

Examples:
 - Then I should see message \"MESSAGE\""
  (lambda (message)
    (let ((msg "Expected '%s' to be included in the list of printed messages, but was not."))
      (assert (equal (car (last ecukes-message-log)) message) nil msg message))))


(provide 'espuds-misc)

;;; espuds-misc.el ends here
