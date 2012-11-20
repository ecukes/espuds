(require 'espuds-helpers)

(ert-deftest fake-eval ()
  "Should fake eval."
  (with-playground
   (with-mock
    (stub make-temp-file => "/tmp/espuds-xyz")
    (mock (load "/tmp/espuds-xyz"))
    (mock (insert "CONTENT"))
    (espuds-fake-eval "CONTENT"))))

(ert-deftest buffer-contents ()
  "Should return buffer content."
  (with-playground
   (insert "CONTENT")
   (should (equal (espuds-buffer-contents) "CONTENT"))))

(ert-deftest region-mark-active ()
  "Should return region when mark active."
  (with-playground
   (insert "CONTENT")
   (let ((mark-active t))
     (with-mock
      (stub region-beginning => 1)
      (stub region-end => 5)
      (should (equal (espuds-region) "CONT"))))))

(ert-deftest region-mark-inactive ()
  "Should return empty string when mark is not active."
  (let ((mark-active nil))
    (should (equal (espuds-region) ""))))

(ert-deftest quit ()
  "Should quit."
  (with-mock
   (mock (keyboard-quit))
   (espuds-quit)))
