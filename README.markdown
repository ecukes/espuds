# Espuds - Ecukes step definitions

First of all. If you don't know what
[Ecukes](http://github.com/rejeep/ecukes) it, go read up about it. If
you have, you should know that in order to test with Ecukes, you need
to translate your steps so that Emacs understands them. You do that
with step definitions.

Espuds is a collection of the most commonly used step definitions.


## Usage
To use Espuds, you have load it in your `features/support.el` file.
    (add-to-list 'load-path "/path/to/espuds)
    (require 'espuds)
    
Thats it!

## Gotchas

### Action Chain
Some actions require more than one keyboard input. For example
functions that reads input via the minibuffer (not via interactive).

To handle such cases, you wrap the actions in a block. For example:

    Scenario: Activate linum mode
      Given I start an action chain
      And I press "M-x"
      And I type "linum-mode"
      And I execute the action chain
      Then linum-mode should be active

## Contributing
Contribution is much welcome!. When adding new features, please write
tests for them!

Install [carton](https://github.com/rejeep/carton) if you haven't
already, then:

    $ cd /path/to/espuds
    $ carton

Run all tests with:

    $ make

## Docs

### Asserts that there nothing to see in the current buffer.

Pattern:

    ^I should not see anything$\|^the buffer should be empty$

Docs:

    Examples:
     - Then I should not see anything
     - Then the buffer should be empty

### Selects TEXT if found. Otherwise signal an error.

Pattern:

    ^I select "\(.+\)"$

Docs:

    Examples:
     - When I select "SOME TEXT"

### Asserts that the current buffer does not match some text.

Pattern:

    ^I should not see pattern\(?: "\(.+\)"\|:\)$

Docs:

    Examples:
     - Then I should not see pattern "CONTENTS"
     - Then I should not see pattern:
         """
         CONTENTS
         """

### Asserts that the current buffer matches some text.

Pattern:

    ^I should see pattern\(?: "\(.+\)"\|:\)$

Docs:

    Examples:
     - Then I should see pattern "CONTENTS"
     - Then I should see pattern:
         """
         CONTENTS
         """

### Asserts that the current buffer does not include some text.

Pattern:

    ^I should not see\(?: "\(.+\)"\|:\)$

Docs:

    Examples:
     - Then I should not see "CONTENTS"
     - Then I should not see:
         """
         CONTENTS
         """

### Asserts that the current buffer includes some text.

Pattern:

    ^I should see\(?: "\(.+\)"\|:\)$

Docs:

    Examples:
     - Then I should see "CONTENTS"
     - Then I should see:
         """
         CONTENTS
         """

### Inserts CONTENTS into the current buffer.

Pattern:

    ^I insert\(?: "\(.+\)"\|:\)$

Docs:

    Examples:
     - When I insert "CONTENTS"
     - When I insert:
         """
         CONTENTS
         """

### Asserts that the region is not active.

Pattern:

    ^the region should not be active$

Docs:

    Examples:
     - Then the region should not be active

### Asserts that the selected region is same as EXPECTED.

Pattern:

    ^the region should be\(?: "\(.*\)"\|:\)$

Docs:

    Examples:
     - Then the region should be "REGION"
     - Then the region should be:
         """
         REGION
         """

### Pop and move point to the top position on the mark-ring

Pattern:

    ^I pop the mark$

Docs:

    Examples:
     - When I pop the mark

### Sets the mark at point.

Pattern:

    ^I set the mark$

Docs:

    Examples:
     - When I set the mark

### Activates transient mark mode.

Pattern:

    ^transient mark mode is \(active\|inactive\)$

Docs:

    Examples:
     - Given transient mark mode is active
     - Given transient mark mode is inactive

### Deactivates mark.

Pattern:

    ^there is no region selected$

Docs:

    Examples:
     - Given there is no region selected

### Places the cursor at the end of the line.

Pattern:

    ^I go to end of line$

Docs:

    Examples:
     - When I go to end of line

### Places the cursor at the beginning of the line.

Pattern:

    ^I go to beginning of line$

Docs:

    Examples:
     - When I go to beginning of line

### Places the cursor at the end of buffer.

Pattern:

    ^I go to end of buffer$

Docs:

    Examples:
     - When I go to end of buffer

### Places the cursor at the beginning of buffer.

Pattern:

    ^I go to beginning of buffer$

Docs:

    Examples:
     - When I go to beginning of buffer

### Places the cursor after first instance of text.

Pattern:

    ^I place the cursor after "\(.+\)"$

Docs:

    Examples:
     - When I place the cursor after "Foo"

### Places the cursor before first instance of text.

Pattern:

    ^I place the cursor before "\(.+\)"$

Docs:

    Examples:
     - When I place the cursor before "Foo"

### Places the cursor between text.

Pattern:

    ^I place the cursor between "\(.+\)" and "\(.+\)"$

Docs:

    Examples:
     - When I place the cursor between "Foo" and "Bar"

### Checks that the cursor is between some text.

Pattern:

    ^the cursor should be between "\(.+\)" and "\(.+\)"$

Docs:

    Examples:
     - Then the cursor should be between "Foo" and "Bar"

### Checks that the cursor is after some text.

Pattern:

    ^the cursor should be after "\(.+\)"$

Docs:

    Examples:
     - Then the cursor should be after "Foo"

### Checks that the cursor is before some text.

Pattern:

    ^the cursor should be before "\(.+\)"$

Docs:

    Examples:
     - Then the cursor should be before "Foo"

### Checks that the cursor is at a specific position.

Pattern:

    ^the cursor should be at point "\(.+\)"$

Docs:

    Examples:
     - Then the cursor should be at point "12"

### Go to WORD if it exist.

Pattern:

    ^I go to word "\(.+\)"$

Docs:

    Examples:
     - When I go to word "SOME WORD"

### Goes to POINT if it exist.

Pattern:

    ^I go to point "\([0-9]+\)"$

Docs:

    Examples:
     - When I go to point "12"

### Goes to LINE if it exist.

Pattern:

    ^I go to line "\([0-9]+\)"$

Docs:

    Examples:
     - When I go to line "12"

### Asserts that MESSAGE has been printed.

Pattern:

    ^I should see message "\(.+\)"$

Docs:

    Examples:
     - Then I should see message "MESSAGE"

### Creates a new temp file called FILE and opens it.

Pattern:

    ^I open temp file "\(.+\)"$

Docs:

    Examples:
     - When I open temp file "SOME FILE"

### Loads CONTENTS with Emacs load command.

Pattern:

    ^I load the following:$

Docs:

    Examples:
     - When I load the following:
         """
         CONTENTS
         """

### Set some variable

Pattern:

    ^I set \(.+\) to \(.+\)$

Docs:

    Examples:
     - When I set sentence-end-double-space to nil

### Turns on some mode.

Pattern:

    ^I turn on \(.+\)$

Docs:

    Examples:
     - When I turn on ruby-mode

### If action chaining is active. Add TYPING to the action

Pattern:

    ^I type "\(.+\)"$

Docs:

    chain. Otherwise simulate the TYPING.
    Examples:
     - When I type "TYPING"

### Quit without signal.

Pattern:

    ^I quit$

Docs:

    Examples:
     - When I quit

### Execute the function that KEYBINDING is bound to.

Pattern:

    ^I press "\(.+\)"$

Docs:

    Note: If action chaining is active. Add KEYBINDING to the action
    chain instead of executing.
    Examples:
     - When I press "C-h e"

### Executes the action chain.

Pattern:

    ^I execute the action chain$

Docs:

    Examples:
     - When I execute the action chain

### Starts an action chain.

Pattern:

    ^I start an action chain$

Docs:

    Examples:
     - When I start an action chain

### Clears all text in the current buffer.

Pattern:

    ^the buffer is empty$\|^I clear the buffer$

Docs:

    Examples:
     - Given the buffer is empty
     - When I clear the buffer

### Asserts that the current buffer is connected to FILE.

Pattern:

    ^I should be in file "\(.+\)"$

Docs:

    Examples:
     - Then I should be in file "/path/to/some/file"

### Asserts that the current buffer is BUFFER.

Pattern:

    ^I should be in buffer "\(.+\)"$

Docs:

    Examples:
     - Then I should be in buffer "*scratch*"

### Switches to BUFFER.

Pattern:

    ^\(?:I am in buffer\|I switch to buffer\) "\(.+\)"$

Docs:

    Examples:
     - When I switch to buffer "Foo"
     - Given I am in buffer "*scratch*"
