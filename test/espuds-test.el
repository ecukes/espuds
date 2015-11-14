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
     (error "Expected to be in buffer '%s', but was in '%s'" "foo" "bar"))
    (Then "I should be in buffer \"foo\""))))

(ert-deftest then-i-should-be-in-file-not-visiting-file ()
  "Should not be in file when buffer not visiting file."
  (with-mock
   (stub buffer-file-name => nil)
   (mock
    (error "Expected file to be '%s', but not visiting any file." "foo"))
   (Then "I should be in file \"foo\"")))

(ert-deftest then-i-should-be-in-file-different-file ()
  "Should not be in file when different file."
  (with-mock
   (stub buffer-file-name => "/path/to/foo")
   (mock
    (error "Expected file to be '%s', but was '%s'." "bar" "/path/to/foo"))
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

(ert-deftest when-i-go-to-line-does-not-exist ()
  "Should not go to line when does not exist."
  (with-playground
   (with-mock
    (insert "foo\nbar\nbaz")
    (stub espuds-goto-line)
    (mock
     (error "Requested line '%s', but buffer only has '%d' line(s)." "4" 3))
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
     (error "Requested point '%s', but buffer only has '%d' point(s)." 11 9))
    (When "I go to point \"11\""))))

(ert-deftest when-i-go-to-point-exists ()
  "Should go to point when exists."
  (with-playground
   (insert "foobarbaz")
   (goto-char (point-min))
   (should (= (point) (point-min)))
   (When "I go to point \"5\"")
   (should (= (point) 5))))

(ert-deftest when-i-go-to-word-does-not-exist ()
  "Should not go to word when does not exist."
  (with-playground
   (insert "foo bar baz")
   (should-error
    (When "I go to word \"qux\""))))

(ert-deftest when-i-go-to-word-exists ()
  "Should go to point when exists."
  (with-playground
   (insert "foo bar baz")
   (When "I go to word \"bar\"")
   (should (equal (point) 5))))

(ert-deftest then-the-cursor-should-be-at-point-does-not-exist ()
  "Should not be at point when point does not exist."
  (with-playground
   (with-mock
    (insert "foo bar baz")
    (mock
     (error "Expected cursor to be at point '%s', but was at '%s'" "15" 12))
    (Then "the cursor should be at point \"15\""))))

(ert-deftest then-the-cursor-should-be-at-point-exists-but-not-at-point ()
  "Should not be at point when not at point."
  (with-playground
   (with-mock
    (insert "foo bar baz")
    (mock
     (error "Expected cursor to be at point '%s', but was at '%s'" "4" 3))
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
     (error "Expected '%s' to be before point but was '%s'." "bar" "o bar"))
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
     (error "Expected '%s' to be after point but was '%s'." "bar" "fo"))
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
     (error "Expected '%s' to be left of point and '%s' to be right of point, but was: '%s[CURSOR]%s'" "foo" "bar" "fo" "obarb"))
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
     (error "Expected '%s' to be before point but was '%s'." "bar" "arbaz"))
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
     (error "Expected '%s' to be after point but was '%s'." "bar" "foob"))
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
   (insert "foo")
   (should (equal (point) 4))
   (When "I go to beginning of line")
   (should (equal (point) 1))))

(ert-deftest when-i-go-to-end-of-line ()
  "Should go to end of line."
  (with-playground
   (insert "foo")
   (When "I go to end of line")
   (should (equal (point) 4))))

(ert-deftest fake-eval ()
  "Should fake eval."
  (with-playground
   (with-mock
    (stub make-temp-file => "/tmp/espuds-xyz")
    (mock (load "/tmp/espuds-xyz" nil t))
    (mock (f-write "CONTENT" 'utf-8 "/tmp/espuds-xyz"))
    (espuds-fake-eval "CONTENT"))))

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
  "Should quit, but should not signal."
  (espuds-quit))

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

(ert-deftest when-i-turn-on-major-mode ()
  "Should turn on major mode."
  (with-playground
   (When "I turn on text-mode")
   (should (equal major-mode 'text-mode))))

(ert-deftest when-i-turn-on-major-mode-multiple-times ()
  "Should turn on major mode."
  (with-playground
   (When "I turn on c-mode")
   (When "I turn on c-mode")
   (should (equal major-mode 'c-mode))))

(ert-deftest when-i-turn-on-minor-mode ()
  "Should turn on minor mode."
  (with-playground
   (with-mock
    (stub message)
    (When "I turn on abbrev-mode"))
   (should abbrev-mode)))

(ert-deftest when-i-turn-on-minor-mode-multiple-times ()
  "Should turn on (not toggle) minor mode."
  (with-playground
   (with-mock
    (stub message)
    (When "I turn on abbrev-mode")
    (When "I turn on abbrev-mode"))
   (should abbrev-mode)))

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
     (mock (error "Expected '%s' to be included in the list of printed messages, but was not." "MESSAGE"))
     (Then "I should see message \"MESSAGE\""))))

(ert-deftest then-i-should-see-message-when-exists ()
  "Should see message when exists."
  (let ((ecukes-message-log (list "foo" "bar" "MESSAGE")))
    (Then "I should see message \"MESSAGE\"")))

(ert-deftest then-i-should-see-message-when-not-last ()
  "Should see message when not last exists."
  (let ((ecukes-message-log (list "foo" "MESSAGE" "bar")))
    (Then "I should see message \"MESSAGE\"")))

(ert-deftest then-i-should-see-message-when-newline ()
  "Should see message when not last exists."
  (let ((ecukes-message-log (list "foo" "\nMESSAGE\n" "bar")))
    (Then "I should see message \"MESSAGE\"")))

(ert-deftest then-i-should-see-message-quotes ()
  (let ((ecukes-message-log (list "foo" "awesome \"MESSAGE\" dude" "bar")))
    (Then "I should see message \"awesome \\\"MESSAGE\\\" dude\"")))

(ert-deftest given-there-is-no-region-selected ()
  "Should deactivate mark."
  (should-not (region-active-p))
  (set-mark (point))
  (activate-mark)
  (should (region-active-p))
  (Given "there is no region selected")
  (should-not (region-active-p)))

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
    (error "Expected the region to be '%s', but was '%s'." "REGION" ""))
   (Then "the region should be \"REGION\"")))

(ert-deftest then-the-region-should-be-arg-wrong-region ()
  "Should not be region when wrong region."
  (with-mock
   (stub espuds-region => "REG")
   (mock
    (error "Expected the region to be '%s', but was '%s'." "REGION" "REG"))
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
    (error "Expected the region to be '%s', but was '%s'." "REGION" ""))
   (Then "the region should be:" "REGION")))

(ert-deftest then-the-region-should-be-arg-wrong-region ()
  "Should not be region when wrong region."
  (with-mock
   (stub espuds-region => "REG")
   (mock
    (error "Expected the region to be '%s', but was '%s'." "REGION" "REG"))
   (Then "the region should be:" "REGION")))

(ert-deftest then-the-region-should-not-be-active-when-active ()
  "Should have active region."
  (with-mock
   (stub region-active-p => t)
   (mock
    (error "Expected the region not to be active, but it was."))
   (Then "the region should not be active")))

(ert-deftest then-the-region-should-not-be-active-when-inactive ()
  "Should not have active region."
  (with-mock
   (stub region-active-p => nil)
   (Then "the region should not be active")))

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
     (error "Expected\n%s\nto be part of:\n%s" "qux" "foo bar baz"))
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
     (error "Expected\n%s\nto be part of:\n%s" "qux" "foo bar baz"))
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
     (error "Expected '%s' to not be part of '%s', but was." "bar" "foo bar baz"))
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
     (error "Expected '%s' to not be part of '%s', but was." "bar" "foo bar baz"))
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
     (error "Expected to see pattern '%s' in '%s', but did not." "\\(.\\)\\1\\1" "foo bar baz"))
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
     (error "Expected to see pattern '%s' in '%s', but did not." "qux" "foo bar baz"))
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
     (error "Expected to not see pattern '%s' in '%s', but did." "\\(.\\)\\1" "foo bar baz"))
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
     (error "Expected to not see pattern '%s' in '%s', but did." "\\(.\\)\\1" "foo bar baz"))
    (Then "I should not see pattern:" "\\(.\\)\\1"))))

(ert-deftest when-i-select-does-not-exist ()
  "Should not select when not exist."
  (with-playground
   (with-mock
    (insert "foo bar baz")
    (stub re-search-backward)
    (mock
     (error "The text '%s' was not found in the current buffer." "qux"))
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
     (error "Expected buffer to be empty, but had content: '%s'" "foo"))
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
     (error "Expected buffer to be empty, but had content: '%s'" "foo"))
    (Then "the buffer should be empty"))))

(ert-deftest then-the-buffer-should-be-empty-when-no-content ()
  "Should not see when no content."
  (with-playground
   (Then "the buffer should be empty")))

(ert-deftest then-current-point-should-be-in-bold ()
  (with-playground
   (lisp-mode)
   ;; make sure keyword-face is bold
   (set-face-attribute font-lock-keyword-face nil :weight 'bold)
   (insert "(defun foo ())")
   (espuds-fontify)
   (goto-char 2)
   (should (member 'font-lock-keyword-face (espuds-faces-at-point)))
   (Then "current point should be in bold")))

(ert-deftest then-current-point-should-be-in-bold-when-not-bold ()
  (with-playground
   (with-mock
    (lisp-mode)
    ;; make sure keyword-face is *not* bold
    (set-face-attribute font-lock-keyword-face nil :weight 'normal)
    (insert "(defun foo ())")
    (espuds-fontify)
    (goto-char 2)
    (should (member 'font-lock-keyword-face (espuds-faces-at-point)))
    (mock
     (error "Expected current point to be in bold"))
    (Then "current point should be in bold"))))

(ert-deftest then-current-point-should-be-in-italic ()
  (with-playground
   (lisp-mode)
   ;; make sure keyword-face is italic
   (set-face-attribute font-lock-keyword-face nil :slant 'italic)
   (insert "(defun foo ())")
   (espuds-fontify)
   (goto-char 2)
   (should (member 'font-lock-keyword-face (espuds-faces-at-point)))
   (Then "current point should be in italic")))

(ert-deftest then-current-point-should-be-in-italic-when-not-italic ()
  (with-playground
   (with-mock
    (lisp-mode)
    ;; make sure keyword-face is *not* italic
    (set-face-attribute font-lock-keyword-face nil :slant 'normal)
    (insert "(defun foo ())")
    (espuds-fontify)
    (goto-char 2)
    (should (member 'font-lock-keyword-face (espuds-faces-at-point)))
    (mock
     (error "Expected current point to be in italic"))
    (Then "current point should be in italic"))))

(ert-deftest then-current-point-should-be-in-strike-through ()
  (with-playground
   (lisp-mode)
   ;; make sure keyword-face is strike-through
   (set-face-attribute font-lock-keyword-face nil :strike-through t)
   (insert "(defun foo ())")
   (espuds-fontify)
   (goto-char 2)
   (should (member 'font-lock-keyword-face (espuds-faces-at-point)))
   (Then "current point should be in strike-through")))

(ert-deftest then-current-point-should-be-in-strike-through-when-not-strike-through ()
  (with-playground
   (with-mock
    (lisp-mode)
    ;; make sure keyword-face is *not* strike-through
    (set-face-attribute font-lock-keyword-face nil :strike-through nil)
    (insert "(defun foo ())")
    (espuds-fontify)
    (goto-char 2)
    (should (member 'font-lock-keyword-face (espuds-faces-at-point)))
    (mock
     (error "Expected current point to be in strike-through"))
    (Then "current point should be in strike-through"))))

(ert-deftest then-current-point-should-be-in-underline ()
  (with-playground
   (lisp-mode)
   ;; make sure keyword-face is underline
   (set-face-attribute font-lock-keyword-face nil :underline t)
   (insert "(defun foo ())")
   (espuds-fontify)
   (goto-char 2)
   (should (member 'font-lock-keyword-face (espuds-faces-at-point)))
   (Then "current point should be in underline")))

(ert-deftest then-current-point-should-be-in-underline-when-not-underline ()
  (with-playground
   (with-mock
    (lisp-mode)
    ;; make sure keyword-face is *not* underline
    (set-face-attribute font-lock-keyword-face nil :underline nil)
    (insert "(defun foo ())")
    (espuds-fontify)
    (goto-char 2)
    (should (member 'font-lock-keyword-face (espuds-faces-at-point)))
    (mock
     (error "Expected current point to be in underline"))
    (Then "current point should be in underline"))))

(ert-deftest then-current-point-should-have-some-face ()
  (with-playground
   (lisp-mode)
   (insert "(defun foo ())")
   (espuds-fontify)
   (goto-char 2)
   (should (member 'font-lock-keyword-face (espuds-faces-at-point)))
   (Then "current point should have the font-lock-keyword-face face")))

(ert-deftest then-current-point-should-have-some-face-when-not ()
  (with-playground
   (with-mock
    (fundamental-mode)
    (insert "foobar")
    (espuds-fontify)
    (goto-char 2)
    (should (null (espuds-faces-at-point)))
    (mock
     (error "Face '%s' was not found at point" "font-lock-keyword-face"))
    (Then "current point should have the font-lock-keyword-face face"))))

(ert-deftest then-current-point-should-have-no-face ()
  (with-playground
   (fundamental-mode)
   (insert "foobar")
   (goto-char 2)
   (Then "current point should have no face")))

(ert-deftest then-current-point-should-have-no-face-but-does ()
  (with-playground
   (with-mock
    (lisp-mode)
    (insert "(defun foo ())")
    (espuds-fontify)
    (goto-char 2)
    (should (equal '(font-lock-keyword-face) (espuds-faces-at-point)))
    (mock
     (error "Current point was expected to have no face but does have '%S'"
            '(font-lock-keyword-face)))
    (Then "current point should have no face"))))

(ert-deftest when-i-delete-other-windows ()
  "Should kill all windows except 1."
  (with-playground
   (should (equal 1 (count-windows)))
   (split-window-right)
   (should (equal 2 (count-windows)))
   (When "I delete other windows")
   (should (equal 1 (count-windows)))))

(ert-deftest when-i-call-a-command ()
  "Invoke specified command."
  (with-playground
   (When "I call \"insert-parentheses\"")
   (should (equal (buffer-string) "()"))))
