;;; espuds-region.el --- region related definitions


;; Deactivates mark.
;;
;; Usage:
;;   Given there is no region selected
(Given "^there is no region selected$"
       (lambda ()
         (deactivate-mark)))

;; Activates transient mark mode.
;;
;; Usage:
;;   Given transient mark mode is active
;;   Given transient mark mode is inactive
(Given "^transient mark mode is \\(active\\|inactive\\)$"
       (lambda (status)
         (transient-mark-mode
          (if (string= status "active") 1 -1))))

;; Sets the mark at point.
;;
;; Usage:
;;   When I set the mark
(When "^I set the mark$"
      (lambda ()
        (set-mark (point))))

;; Asserts that the selected region is same as EXPECTED.
;;
;; Usage:
;;   Then the region should be "REGION"
;;
;;   Then the region should be:
;;   """
;;   REGION
;;   """
(Then "^the region should be\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (expected)
        (let ((actual (espuds-region))
              (message "Expected the region to be '%s', but was '%s'."))
          (assert (equal expected actual) nil message expected actual))))


(provide 'espuds-region)

;;; espuds-region.el ends here
