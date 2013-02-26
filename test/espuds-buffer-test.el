(require 'espuds-buffer)

(ert-deftest given-i-am-in-buffer ()
  "Should switch to buffer."
  (with-playground
   (Given "I am in buffer \"foo\"")
   (should (equal (buffer-name) "foo"))))

(ert-deftest given-i-switch-to-buffer ()
  "Should switch to buffer."
  (with-playground
   (Given "I switch to buffer \"foo\"")
   (should (equal (buffer-name) "foo"))))

(ert-deftest given-i-switch-to-buffer-spaces ()
  "Should switch to buffer spaces."
  (with-playground
   (Given "I switch to buffer \" foo bar \"")
   (should (equal (buffer-name) " foo bar "))))

(ert-deftest then-i-should-be-in-buffer-correct ()
  "Should be in buffer."
  (with-playground
   (let ((buffer "foo"))
     (Given "I switch to buffer \"%s\"" buffer)
     (Then "I should be in buffer \"%s\"" buffer))))

(ert-deftest then-i-should-be-in-buffer-not-same ()
  "Should not be in buffer."
  (with-playground
   (with-mock
    (stub buffer-name => "bar")
    (mock
     (assert nil nil "Expected to be in buffer '%s', but was in '%s'" "foo" "bar"))
    (Then "I should be in buffer \"foo\""))))

(ert-deftest then-i-should-be-in-file-not-visiting-file ()
  "Should not be in file when buffer not visiting file."
  (with-mock
   (stub buffer-file-name => nil)
   (mock
    (assert nil nil "Expected file to be '%s', but not visiting any file." "foo"))
   (Then "I should be in file \"foo\"")))

(ert-deftest then-i-should-be-in-file-different-file ()
  "Should not be in file when different file."
  (with-mock
   (stub buffer-file-name => "/path/to/foo")
   (mock
    (assert nil nil "Expected file to be '%s', but was '%s'." "bar" "/path/to/foo"))
   (Then "I should be in file \"bar\"")))

(ert-deftest then-i-should-be-in-file-same-file ()
  "Should not be in file when same file."
  (with-mock
   (stub buffer-file-name => "/path/to/foo")
   (Then "I should be in file \"foo\"")))

(ert-deftest given-the-buffer-is-empty ()
  "Should erase buffer."
  (with-playground
   (insert "CONTENT")
   (Given "the buffer is empty")
   (should (equal (buffer-string) ""))))

(ert-deftest given-i-clear-the-buffer ()
  "Should erase buffer."
  (with-playground
   (insert "CONTENT")
   (Given "I clear the buffer")
   (should (equal (buffer-string) ""))))
