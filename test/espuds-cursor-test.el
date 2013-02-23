(require 'espuds-cursor)

(ert-deftest when-i-go-to-line-does-not-exist ()
  "Should not go to line when does not exist."
  (with-playground
   (with-mock
    (insert "foo\nbar\nbaz")
    (stub espuds-goto-line)
    (mock
     (assert nil nil "Requested line '%s', but buffer only has '%d' line(s)." "4" 3))
    (When "I go to line \"4\""))))

(ert-deftest when-i-go-to-line-exists ()
  "Should go to line when exists."
  (with-playground
   (with-mock
    (insert "foo\nbar\nbaz")
    (mock (espuds-goto-line 2))
    (When "I go to line \"2\""))))

(ert-deftest when-i-go-to-point-does-not-exist ()
  "Should not go to point when does not exist."
  (with-playground
   (with-mock
    (insert "foobarbaz")
    (stub goto-char)
    (mock
     (assert nil nil "Requested point '%s', but buffer only has '%d' point(s)." 11 9))
    (When "I go to point \"11\""))))

(ert-deftest when-i-go-to-point-exists ()
  "Should go to point when exists."
  (with-playground
   (with-mock
    (insert "foobarbaz")
    (mock (goto-char 5))
    (When "I go to point \"5\""))))

(ert-deftest when-i-go-to-word-does-not-exist ()
  "Should not go to word when does not exist."
  (with-playground
   (with-mock
    (insert "foo bar baz")
    (stub backward-char)
    (mock
     (assert nil nil "Can not go to word '%s' since it does not exist in the current buffer: %s" "qux" "foo bar baz"))
    (When "I go to word \"qux\""))))

(ert-deftest when-i-go-to-word-exists ()
  "Should go to point when exists."
  (with-playground
   (insert "foo bar baz")
   (When "I go to word \"bar\"")
   (should (equal (point) 5))))

;; does not exist
;; exists, but not at point
;; exists and at point

(ert-deftest then-the-cursor-should-be-at-point-does-not-exist ()
  "Should not be at point when point does not exist."
  (with-playground
   (with-mock
    (insert "foo bar baz")
    (mock
     (assert nil nil "Expected cursor to be at point '%s', but was at '%s'" "15" 12))
    (Then "the cursor should be at point \"15\""))))

(ert-deftest then-the-cursor-should-be-at-point-exists-but-not-at-point ()
  "Should not be at point when not at point."
  (with-playground
   (with-mock
    (insert "foo bar baz")
    (mock
     (assert nil nil "Expected cursor to be at point '%s', but was at '%s'" "4" 3))
    (goto-char 3)
    (Then "the cursor should be at point \"4\""))))

(ert-deftest then-the-cursor-should-be-at-point-exists-and-at-point ()
  "Should be at point when at point."
  (with-playground
   (insert "foo bar baz")
   (goto-char 3)
   (Then "the cursor should be at point \"3\"")))

(ert-deftest then-the-cursor-should-be-before-is-not-before ()
  "Should not be before word when not before."
  (with-playground
   (with-mock
    (insert "foo bar baz")
    (mock
     (assert nil nil "Expected '%s' to be before point but was '%s'." "bar" "o bar"))
    (goto-char 3)
    (Then "the cursor should be before \"bar\""))))

(ert-deftest then-the-cursor-should-be-before-is-before ()
  "Should be before word when before."
  (with-playground
   (insert "foo bar baz")
   (goto-char 5)
   (Then "the cursor should be before \"bar\"")))

(ert-deftest then-the-cursor-should-be-after-is-not-after ()
  "Should not be after word when not after."
  (with-playground
   (with-mock
    (insert "foo bar baz")
    (mock
     (assert nil nil "Expected '%s' to be after point but was '%s'." "bar" "fo"))
    (goto-char 3)
    (Then "the cursor should be after \"bar\""))))

(ert-deftest then-the-cursor-should-be-after-is-after ()
  "Should be after word when after."
  (with-playground
   (insert "foo bar baz")
   (goto-char 8)
   (Then "the cursor should be after \"bar\"")))

(ert-deftest then-the-cursor-should-be-between-is-not-between ()
  "Should not be between when not between."
  (with-playground
   (with-mock
    (insert "foobarbaz")
    (mock
     (assert nil nil "Expected '%s' to be left of point and '%s' to be right of point, but was: '%s[CURSOR]%s'" "foo" "bar" "fo" "obarb"))
    (goto-char 3)
    (Then "the cursor should be between \"foo\" and \"bar\""))))

(ert-deftest then-the-cursor-should-be-between-is-between ()
  "Should be between when between."
  (with-playground
   (insert "foobarbaz")
   (goto-char 4)
   (Then "the cursor should be between \"foo\" and \"bar\"")))

(ert-deftest then-the-cursor-should-be-before-is-not-before ()
  "Should not be before when not before."
  (with-playground
   (with-mock
    (insert "foobarbaz")
    (mock
     (assert nil nil "Expected '%s' to be before point but was '%s'." "bar" "arbaz"))
    (goto-char 5)
    (Then "the cursor should be before \"bar\""))))

(ert-deftest then-the-cursor-should-be-before-is-before ()
  "Should be before when before."
  (with-playground
   (insert "foobarbaz")
   (goto-char 4)
   (Then "the cursor should be before \"bar\"")))

(ert-deftest then-the-cursor-should-be-after-is-not-after ()
  "Should not be after when not after."
  (with-playground
   (with-mock
    (insert "foobarbaz")
    (mock
     (assert nil nil "Expected '%s' to be after point but was '%s'." "bar" "foob"))
    (goto-char 5)
    (Then "the cursor should be after \"bar\""))))

(ert-deftest then-the-cursor-should-be-after-is-after ()
  "Should be after when after."
  (with-playground
   (insert "foobarbaz")
   (goto-char 7)
   (Then "the cursor should be after \"bar\"")))

(ert-deftest when-i-go-to-beginning-of-buffer ()
  "Should go to beginning of buffer."
  (with-playground
   (with-mock
    (mock (beginning-of-buffer))
    (When "I go to beginning of buffer"))))

(ert-deftest when-i-go-to-end-of-buffer ()
  "Should go to end of buffer."
  (with-playground
   (with-mock
    (mock (end-of-buffer))
    (When "I go to end of buffer"))))

(ert-deftest when-i-go-to-beginning-of-line ()
  "Should go to beginning of line."
  (with-playground
   (with-mock
    (mock (move-beginning-of-line))
    (When "I go to beginning of line"))))

(ert-deftest when-i-go-to-end-of-line ()
  "Should go to end of line."
  (with-playground
   (with-mock
    (mock (move-end-of-line))
    (When "I go to end of line"))))
