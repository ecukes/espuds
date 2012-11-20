(require 'espuds-misc)

(ert-deftest when-i-turn-on-major-mode ()
  "Should turn on major mode."
  (with-playground
   (When "I turn on text-mode")
   (should (equal major-mode 'text-mode))))

(ert-deftest when-i-turn-on-minor-mode ()
  "Should turn on minor mode."
  (with-playground
   (with-mock
    (stub message)
    (When "I turn on longlines-mode"))
   (should longlines-mode)))

(ert-deftest when-i-set-variable-to-symbol-value ()
  "Should set variable to symbol value."
  (let ((variable))
    (When "I set variable to value")
    (should (equal variable 'value))))

(ert-deftest when-i-set-variable-to-string-value ()
  "Should set variable to string value."
  (let ((variable))
    (When "I set variable to \"value\"")
    (should (equal variable "value"))))

(ert-deftest when-i-set-variable-to-number-value ()
  "Should set variable to number value."
  (let ((variable))
    (When "I set variable to 1")
    (should (equal variable 1))))

(ert-deftest when-i-load-the-following ()
  "Should load content."
  (with-mock
   (mock (espuds-fake-eval "CONTENT"))
   (When "I load the following:" "CONTENT")))

(ert-deftest when-i-open-temp-file ()
  "Should open temp file."
  (with-mock
   (stub make-temp-file => "/tmp/tmp.xyz")
   (mock (find-file "/tmp/tmp.xyz"))
   (When "I open temp file \"tmp.xyz\"")))

(ert-deftest then-i-should-see-message-when-not-exists ()
  "Should not see message when not exists."
  (with-mock
   (let ((ecukes-message-log (list "foo" "bar")))
     (mock (assert nil nil "Expected '%s' to be included in the list of printed messages, but was not." "MESSAGE"))
     (Then "I should see message \"MESSAGE\""))))

(ert-deftest then-i-should-see-message-when-exists ()
  "Should see message when exists."
  (let ((ecukes-message-log (list "foo" "bar" "MESSAGE")))
    (Then "I should see message \"MESSAGE\"")))
