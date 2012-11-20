(require 'espuds-region)

(ert-deftest given-there-is-no-region-selected ()
  "Should deactivate mark."
  (with-mock
   (mock (deactivate-mark))
   (Given "there is no region selected")))

(ert-deftest given-transient-mark-mode-is-active ()
  "Should be active."
  (with-mock
   (mock (transient-mark-mode 1))
   (Given "transient mark mode is active")))

(ert-deftest given-transient-mark-mode-is-inactive ()
  "Should be inactive."
  (with-mock
   (mock (transient-mark-mode -1))
   (Given "transient mark mode is inactive")))

(ert-deftest when-i-set-the-mark ()
  "Should set mark."
  (with-mock
   (mock (set-mark 1))
   (When "I set the mark")))

(ert-deftest when-i-pop-the-mark ()
  "Should pop mark."
  (with-mock
   (mock (set-mark-command 4))
   (When "I pop the mark")))

(ert-deftest then-the-region-should-be-arg ()
  "Should be region."
  (with-mock
   (stub espuds-region => "REGION")
   (Then "the region should be \"REGION\"")))

(ert-deftest then-the-region-should-be-arg-no-region ()
  "Should not be region when no region."
  (with-mock
   (stub espuds-region => "")
   (mock
    (assert nil nil "Expected the region to be '%s', but was '%s'." "REGION" ""))
   (Then "the region should be \"REGION\"")))

(ert-deftest then-the-region-should-be-arg-wrong-region ()
  "Should not be region when wrong region."
  (with-mock
   (stub espuds-region => "REG")
   (mock
    (assert nil nil "Expected the region to be '%s', but was '%s'." "REGION" "REG"))
   (Then "the region should be \"REGION\"")))

(ert-deftest then-the-region-should-be-arg ()
  "Should be region."
  (with-mock
   (stub espuds-region => "REGION")
   (Then "the region should be:" "REGION")))

(ert-deftest then-the-region-should-be-arg-no-region ()
  "Should not be region when no region."
  (with-mock
   (stub espuds-region => "")
   (mock
    (assert nil nil "Expected the region to be '%s', but was '%s'." "REGION" ""))
   (Then "the region should be:" "REGION")))

(ert-deftest then-the-region-should-be-arg-wrong-region ()
  "Should not be region when wrong region."
  (with-mock
   (stub espuds-region => "REG")
   (mock
    (assert nil nil "Expected the region to be '%s', but was '%s'." "REGION" "REG"))
   (Then "the region should be:" "REGION")))

(ert-deftest then-the-region-should-not-be-active-when-active ()
  "Should have active region."
  (with-mock
   (stub region-active-p => t)
   (mock
    (assert nil nil "Expected the region not to be active, but it was."))
   (Then "the region should not be active")))

(ert-deftest then-the-region-should-not-be-active-when-inactive ()
  "Should not have active region."
  (with-mock
   (stub region-active-p => nil)
   (Then "the region should not be active")))
