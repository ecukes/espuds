(require 'espuds-input)

(ert-deftest when-i-start-an-action-chain ()
  "Should start an action chain."
  (let ((espuds-action-chain [1 2 3])
        (espuds-chain-active nil))
    (When "I start an action chain")
    (should (equal espuds-action-chain nil))
    (should (equal espuds-chain-active t))))

(ert-deftest when-i-execute-the-action-chain ()
  "Should execute the action chain."
  (let ((espuds-action-chain [1 2 3])
        (espuds-chain-active t))
    (with-mock
     (mock (execute-kbd-macro [1 2 3]))
     (When "I execute the action chain")
     (should (equal espuds-chain-active nil)))))

(ert-deftest when-i-press-chain-active ()
  "Should add to action chain when chaining active."
  (let ((espuds-chain-active t)
        (espuds-action-chain nil))
    (with-mock
     (stub edmacro-parse-keys => "C-a")
     (When "I press \"C-a\"")
     (should (equal espuds-action-chain [67 45 97])))))

(ert-deftest when-i-press-chain-inactive ()
  "Should execute macro when chaining inactive."
  (with-playground
   (let ((espuds-chain-active nil))
     (insert "foo")
     (should (equal (point) 4))
     (When "I press \"C-a\"")
     (should (equal (point) 1)))))

(ert-deftest when-i-press-c-g-bound-to-keyboard-quit ()
  "Should quit when pressing C-g and it's bound to `keyboard-quit'."
  (let ((espuds-chain-active nil))
    (with-mock
     (stub edmacro-parse-keys => "C-g")
     (stub key-binding => 'keyboard-quit)
     (mock (espuds-quit))
     (When "I press \"C-g\""))))

(ert-deftest when-i-press-c-g-not-bound-to-keyboard-quit ()
  "Should not quit when pressing C-g but it's not bound to `keyboard-quit'."
  (let ((espuds-chain-active nil))
    (with-mock
     (stub edmacro-parse-keys => "C-g")
     (stub key-binding => 'ignore)
     (mock (execute-kbd-macro "C-g"))
     (When "I press \"C-g\""))))

(ert-deftest when-i-press-c-y ()
  "Should yank."
  (let ((kill-ring (list "bar")))
    (with-playground
     (insert "foo")
     (When "I press \"C-y\"")
     (should (equal (buffer-string) "foobar")))))

(ert-deftest when-i-press-c-y-then-m-y ()
  "Should yank pop."
  (with-playground
   (insert "foo")
   (let ((kill-ring (list "foo" "bar")))
     (When "I press \"C-y\"")
     (And  "I press \"M-y\"")
     (should (equal (buffer-string) "foofoo")))))

(ert-deftest when-i-press-previous-keyboard-input ()
  "Should set previous keyboard input."
  (let ((espuds-previous-keyboard-input))
    (When "I press \"C-a\"")
    (should (equal espuds-previous-keyboard-input "C-a"))))

(ert-deftest when-i-quit ()
  "Should quit."
  (with-mock
   (mock (espuds-quit))
   (When "I quit")))

(ert-deftest given-i-type-chain-active ()
  "Should type when chain active."
  (let ((espuds-chain-active t)
        (espuds-action-chain [1 2 3]))
    (When "I type \"456\"")
    (should (equal espuds-action-chain [1 2 3 52 53 54]))))

(ert-deftest given-i-type-chain-inactive ()
  "Should type when chain inactive."
  (let ((espuds-chain-active nil))
    (with-mock
     (mock (execute-kbd-macro [52 53 54]))
     (When "I type \"456\""))))
