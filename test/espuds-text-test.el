(require 'espuds-text)

(ert-deftest when-i-insert-arg ()
  "Should insert arg."
  (with-playground
   (When "I insert \"foo\"")
   (should (equal (buffer-string) "foo"))))

(ert-deftest when-i-insert-py-string ()
  "Should insert py string."
  (with-playground
   (When "I insert:" "bar")
   (should (equal (buffer-string) "bar"))))

(ert-deftest then-i-should-see-when-exists-arg ()
  "Should see when exists (arg)."
  (with-playground
   (insert "foo bar baz")
   (Then "I should see \"bar\"")))

(ert-deftest then-i-should-see-does-not-exist-arg ()
  "Should not see when not exist (arg)."
  (with-playground
   (with-mock
    (insert "foo bar baz")
    (mock
     (assert nil nil "Expected '%s' to be part of '%s', but was not." "qux" "foo bar baz"))
    (Then "I should see \"qux\""))))

(ert-deftest then-i-should-see-when-exists-py-string ()
  "Should see when exists (py-string)."
  (with-playground
   (insert "foo bar baz")
   (Then "I should see:" "bar")))

(ert-deftest then-i-should-see-does-not-exist-py-string ()
  "Should not see when not exist (py-string)."
  (with-playground
   (with-mock
    (insert "foo bar baz")
    (mock
     (assert nil nil "Expected '%s' to be part of '%s', but was not." "qux" "foo bar baz"))
    (Then "I should see:" "qux"))))

(ert-deftest then-i-should-not-see-does-not-exist-arg ()
  "Should not see when not exist (arg)."
  (with-playground
   (insert "foo bar baz")
   (Then "I should not see \"qux\"")))

(ert-deftest then-i-should-not-see-when-exists-arg ()
  "should see when exists (arg)."
  (with-playground
   (with-mock
    (insert "foo bar baz")
    (mock
     (assert nil nil "Expected '%s' to not be part of '%s', but was." "bar" "foo bar baz"))
    (Then "I should not see \"bar\""))))

(ert-deftest then-i-should-not-see-does-not-exist-py-string ()
  "Should not see when not exist (py-string)."
  (with-playground
   (insert "foo bar baz")
   (Then "I should not see:" "qux")))

(ert-deftest then-i-should-not-see-when-exists-py-string ()
  "should see when exists (py-string)."
  (with-playground
   (with-mock
    (insert "foo bar baz")
    (mock
     (assert nil nil "Expected '%s' to not be part of '%s', but was." "bar" "foo bar baz"))
    (Then "I should not see:" "bar"))))

(ert-deftest then-i-should-see-pattern-when-exists-arg ()
  "Should see pattern when exists (arg)."
  (with-playground
   (insert "foo bar baz")
   (Then "I should see pattern \"\\(.\\)\\1\"")))

(ert-deftest then-i-should-see-pattern-does-not-exist-arg ()
  "Should not see pattern when not exist (arg)."
  (with-playground
   (with-mock
    (insert "foo bar baz")
    (mock
     (assert nil nil "Expected to see pattern '%s' in '%s', but did not." "\\(.\\)\\1\\1" "foo bar baz"))
    (Then "I should see pattern \"\\(.\\)\\1\\1\""))))

(ert-deftest then-i-should-see-pattern-when-exists-py-string ()
  "Should see pattern when exists (py-string)."
  (with-playground
   (insert "foo bar baz")
   (Then "I should see pattern:" "\\(.\\)\\1")))

(ert-deftest then-i-should-see-pattern-does-not-exist-py-string ()
  "Should not see pattern when not exist (py-string)."
  (with-playground
   (with-mock
    (insert "foo bar baz")
    (mock
     (assert nil nil "Expected to see pattern '%s' in '%s', but did not." "qux" "foo bar baz"))
    (Then "I should see pattern:" "qux"))))

(ert-deftest then-i-should-not-see-pattern-does-not-exist-arg ()
  "Should not see pattern when not exist (arg)."
  (with-playground
   (insert "foo bar baz")
   (Then "I should not see pattern \"\\(.\\)\\1\\1\"")))

(ert-deftest then-i-should-not-see-pattern-when-exists-arg ()
  "Should not see pattern when exists (arg)."
  (with-playground
   (with-mock
    (insert "foo bar baz")
    (mock
     (assert nil nil "Expected to not see pattern '%s' in '%s', but did." "\\(.\\)\\1" "foo bar baz"))
    (Then "I should not see pattern \"\\(.\\)\\1\""))))

(ert-deftest then-i-should-not-see-pattern-does-not-exist-py-string ()
  "Should not see pattern when not exist (py-string)."
  (with-playground
   (insert "foo bar baz")
   (Then "I should not see pattern:" "qux")))

(ert-deftest then-i-should-not-see-pattern-when-exists-py-string ()
  "Should not see pattern when exists (py-string)."
  (with-playground
   (with-mock
    (insert "foo bar baz")
    (mock
     (assert nil nil "Expected to not see pattern '%s' in '%s', but did." "\\(.\\)\\1" "foo bar baz"))
    (Then "I should not see pattern:" "\\(.\\)\\1"))))

(ert-deftest when-i-select-does-not-exist ()
  "Should not select when not exist."
  (with-playground
   (with-mock
    (insert "foo bar baz")
    (stub re-search-backward)
    (mock
     (assert nil nil "The text '%s' was not found in the current buffer." "qux"))
    (When "I select \"qux\""))))

(ert-deftest when-i-select-does-exists ()
  "Should select when exists."
  (with-playground
   (with-mock
    (insert "foo bar baz")
    (When "I select \"bar\"")
    (should (equal (point) 5))
    (should (equal (mark) 8)))))

(ert-deftest then-i-should-not-see-anything-when-content ()
  "Should see when content."
  (with-playground
   (with-mock
    (insert "foo")
    (mock
     (assert nil nil "Expected buffer to be empty, but had content: '%s'" "foo"))
    (Then "I should not see anything"))))

(ert-deftest then-i-should-not-see-anything-when-no-content ()
  "Should not see when no content."
  (with-playground
   (Then "I should not see anything")))

(ert-deftest then-the-buffer-should-be-empty-when-content ()
  "Should see when content."
  (with-playground
   (with-mock
    (insert "foo")
    (mock
     (assert nil nil "Expected buffer to be empty, but had content: '%s'" "foo"))
    (Then "the buffer should be empty"))))

(ert-deftest then-the-buffer-should-be-empty-when-no-content ()
  "Should not see when no content."
  (with-playground
   (Then "the buffer should be empty")))
