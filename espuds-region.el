;;; espuds-region.el --- region related definitions

(eval-when-compile
  (require 'cl))
(require 'espuds-helpers)

(Given "^there is no region selected$"
  "Deactivates mark.

Examples:
 - Given there is no region selected"
  (lambda ()
    (deactivate-mark)))

(Given "^transient mark mode is \\(active\\|inactive\\)$"
  "Activates transient mark mode.

Examples:
 - Given transient mark mode is active
 - Given transient mark mode is inactive"
  (lambda (status)
    (transient-mark-mode
     (if (string= status "active") 1 -1))))

(When "^I set the mark$"
  "Sets the mark at point.

Examples:
 - When I set the mark"
  (lambda ()
    (set-mark (point))))

(When "^I pop the mark$"
  "Pop and move point to the top position on the mark-ring

Examples:
 - When I pop the mark"
  (lambda ()
    (set-mark-command 4)))

(Then "^the region should be\\(?: \"\\(.*\\)\"\\|:\\)$"
  "Asserts that the selected region is same as EXPECTED.

Examples:
 - Then the region should be \"REGION\"
 - Then the region should be:
     \"\"\"
     REGION
     \"\"\""
  (lambda (expected)
    (let ((actual (espuds-region))
          (message "Expected the region to be '%s', but was '%s'."))
      (assert (equal expected actual) nil message expected actual))))

(Then "^the region should not be active$"
  "Asserts that the region is not active.

Examples:
 - Then the region should not be active"
  (lambda ()
    (let ((message "Expected the region not to be active, but it was."))
      (assert (not (region-active-p)) nil message))))

(provide 'espuds-region)

;;; espuds-region.el ends here
