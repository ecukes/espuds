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


## Step Docs

**Switches to BUFFER.**

Pattern:

    (Given "^\\(?:I am in buffer\\|I switch to buffer\\) \"\\(.+\\)\"$" ...)

Examples:

    When I switch to buffer "Foo"
    Given I am in buffer "*scratch*"


**Asserts that the current buffer is BUFFER.**

Pattern:

    (Then "^I should be in buffer \"\\(.+\\)\"$" ...)

Examples:

    Then I should be in buffer "*scratch*"


**Asserts that the current buffer is connected to FILE.**

Pattern:

    (Then "^I should be in file \"\\(.+\\)\"$" ...)

Examples:

    Then I should be in file "/path/to/some/file"


**Clears all text in the current buffer.**

Pattern:

    (Given "^the buffer is empty$\\|^I clear the buffer$" ...)

Examples:

    Given the buffer is empty
    When I clear the buffer


**Starts an action chain.**

Pattern:

    (When "^I start an action chain$" ...)

Examples:

    When I start an action chain


**Executes the action chain.**

Pattern:

    (When "^I execute the action chain$" ...)

Examples:

    When I execute the action chain


**If action chaining is active. Add KEYBINDING to the actionchain. Otherwise execute the function that KEYBINDING is bound to.**

Pattern:

    (When "^I press \"\\(.+\\)\"$" ...)

Examples:

    When I press "C-h e"


**Quit without signal.**

Pattern:

    (When "^I quit$" ...)

Examples:

    When I quit


**If action chaining is active. Add TYPING to the actionchain. Otherwise simulate the TYPING.**

Pattern:

    (When "^I type \"\\(.+\\)\"$" ...)

Examples:

    When I type "TYPING"


**Turns on some mode.**

Pattern:

    (When "^I turn on \\(.+\\)$" ...)

Examples:

    When I turn on ruby-mode


**Set some variable**

Pattern:

    (When "^I set \\(.+\\) to \\(.+\\)$" ...)

Examples:

    When I set sentence-end-double-space to nil


**Loads CONTENTS with Emacs load command.**

Pattern:

    (When "^I load the following:$" ...)

Examples:

    When I load the following:
    """
    CONTENTS
    """


**Creates a new temp file called FILE and opens it.**

Pattern:

    (When "^I open temp file \"\\(.+\\)\"$" ...)

Examples:

    When I open temp file "SOME FILE"


**Asserts that MESSAGE has been printed.**

Pattern:

    (Then "^I should see message \"\\(.+\\)\"$" ...)

Examples:

    Then I should see message "MESSAGE"


**Goes to LINE if it exist.**

Pattern:

    (When "^I go to line \"\\([0-9]+\\)\"$" ...)

Examples:

    When I go to line "12"


**Goes to POINT if it exist.**

Pattern:

    (When "^I go to point \"\\([0-9]+\\)\"$" ...)

Examples:

    When I go to point "12"


**Go to WORD if it exist.**

Pattern:

    (When "^I go to word \"\\(.+\\)\"$" ...)

Examples:

    When I go to word "SOME WORD"


**Checks that the cursor is at a specific position.**

Pattern:

    (Then "^the cursor should be at point \"\\(.+\\)\"$" ...)

Examples:

    Then the cursor should be at point "12"


**Checks that the cursor is before some text.**

Pattern:

    (Then "^the cursor should be before \"\\(.+\\)\"$" ...)

Examples:

    Then the cursor should be before "Foo"


**Checks that the cursor is after some text.**

Pattern:

    (Then "^the cursor should be after \"\\(.+\\)\"$" ...)

Examples:

    Then the cursor should be after "Foo"


**Checks that the cursor is between some text.**

Pattern:

    (Then "^the cursor should be between \"\\(.+\\)\" and \"\\(.+\\)\"$" ...)

Examples:

    Then the cursor should be between "Foo" and "Bar"


**Places the cursor between text.**

Pattern:

    (When "^I place the cursor between \"\\(.+\\)\" and \"\\(.+\\)\"$" ...)

Examples:

    When I place the cursor between "Foo" and "Bar"


**Places the cursor before first instance of text.**

Pattern:

    (When "^I place the cursor before \"\\(.+\\)\"$" ...)

Examples:

    When I place the cursor before "Foo"


**Places the cursor after first instance of text.**

Pattern:

    (When "^I place the cursor after \"\\(.+\\)\"$" ...)

Examples:

    When I place the cursor after "Foo"


**Places the cursor at the beginning of buffer.**

Pattern:

    (When "^I go to beginning of buffer$" ...)

Examples:

    When I go to beginning of buffer


**Places the cursor at the end of buffer.**

Pattern:

    (When "^I go to end of buffer$" ...)

Examples:

    When I go to end of buffer


**Places the cursor at the beginning of the line.**

Pattern:

    (When "^I go to beginning of line$" ...)

Examples:

    When I go to beginning of line


**Places the cursor at the end of the line.**

Pattern:

    (When "^I go to end of line$" ...)

Examples:

    When I go to end of line


**Deactivates mark.**

Pattern:

    (Given "^there is no region selected$" ...)

Examples:

    Given there is no region selected


**Activates transient mark mode.**

Pattern:

    (Given "^transient mark mode is \\(active\\|inactive\\)$" ...)

Examples:

    Given transient mark mode is active
    Given transient mark mode is inactive


**Sets the mark at point.**

Pattern:

    (When "^I set the mark$" ...)

Examples:

    When I set the mark


**Pop and move point to the top position on the mark-ring**

Pattern:

    (When "^I pop the mark$" ...)

Examples:

    When I pop the mark


**Asserts that the selected region is same as EXPECTED.**

Pattern:

    (Then "^the region should be\\(?: \"\\(.*\\)\"\\|:\\)$" ...)

Examples:

    Then the region should be "REGION"
    Then the region should be:
    """
    REGION
    """


**Asserts that the region is not active.**

Pattern:

    (Then "^the region should not be active$" ...)

Examples:

    Then the region should not be active


**Inserts CONTENTS into the current buffer.**

Pattern:

    (When "^I insert\\(?: \"\\(.+\\)\"\\|:\\)$" ...)

Examples:

    When I insert "CONTENTS"
    When I insert:
    """
    CONTENTS
    """


**Asserts that the current buffer includes some text.**

Pattern:

    (Then "^I should see\\(?: \"\\(.+\\)\"\\|:\\)$" ...)

Examples:

    Then I should see "CONTENTS"
    Then I should see:
    """
    CONTENTS
    """


**Asserts that the current buffer does not include some text.**

Pattern:

    (Then "^I should not see\\(?: \"\\(.+\\)\"\\|:\\)$" ...)

Examples:

    Then I should not see "CONTENTS"
    Then I should not see:
    """
    CONTENTS
    """


**Asserts that the current buffer matches some text.**

Pattern:

    (Then "^I should see pattern\\(?: \"\\(.+\\)\"\\|:\\)$" ...)

Examples:

    Then I should see pattern "CONTENTS"
    Then I should see pattern:
    """
    CONTENTS
    """


**Asserts that the current buffer does not match some text.**

Pattern:

    (Then "^I should not see pattern\\(?: \"\\(.+\\)\"\\|:\\)$" ...)

Examples:

    Then I should not see pattern "CONTENTS"
    Then I should not see pattern:
    """
    CONTENTS
    """


**Selects TEXT if found. Otherwise signal an error.**

Pattern:

    (When "^I select \"\\(.+\\)\"$" ...)

Examples:

    When I select "SOME TEXT"


**Asserts that there nothing to see in the current buffer.**

Pattern:

    (Then "^I should not see anything$\\|^the buffer should be empty$" ...)

Examples:

    Then I should not see anything
    Then the buffer should be empty




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
