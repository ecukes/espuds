# Espuds - Ecukes step definitions

[![Build Status](https://api.travis-ci.org/ecukes/espuds.png?branch=master)](http://travis-ci.org/ecukes/espuds)
[![MELPA](http://melpa.org/packages/espuds-badge.svg)](http://melpa.org/#/espuds)
[![MELPA stable](http://stable.melpa.org/packages/espuds-badge.svg)](http://stable.melpa.org/#/espuds)
[![License] (http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

First of all. If you don't know what
[Ecukes](http://github.com/ecukes/ecukes) is, go read up about it
first. If you have, you should know that in order to test with Ecukes,
you need to translate your steps so that Emacs understands them. You
do that with step definitions.

Espuds is a collection of the most commonly used step definitions.

## Installation

Add `espuds` to your [Cask](https://github.com/cask/cask) file:

```lisp
(depends-on "espuds")
```

## Gotchas

### Action Chain

Some actions require more than one keyboard input. For example
functions that reads input via the minibuffer (not via
interactive). To handle such cases, you wrap the actions in a
block. For example:

    Scenario: Activate linum mode
      Given I start an action chain
      And I press "M-x"
      And I type "linum-mode"
      And I execute the action chain
      Then linum-mode should be active

## List of Steps
<!-- generated with a `grep -E "^\((Given|When|Then)" espuds.el | sed 's:^(:- :' | sed 's:"\^::' | sed 's:\$"::'` :D -->
You can retrieve them by running the `bin/docs` script.

Otherwise you can just read this list:

- Given \\(?:I am in buffer\\|I switch to buffer\\) \"\\(.+\\)\"
- Then I should be in buffer \"\\(.+\\)\"
- Then I should be in file \"\\(.+\\)\"
- Given the buffer is empty$\\|^I clear the buffer
- When I go to line \"\\([0-9]+\\)\"
- When I go to point \"\\([0-9]+\\)\"
- When I go to word \"\\(.+\\)\"
- Then the cursor should be at point \"\\(.+\\)\"
- Then the cursor should be before \"\\(.+\\)\"
- Then the cursor should be after \"\\(.+\\)\"
- Then the cursor should be between \"\\(.+\\)\" and \"\\(.+\\)\"
- When I place the cursor between \"\\(.+\\)\" and \"\\(.+\\)\"
- When I place the cursor before \"\\(.+\\)\"
- When I place the cursor after \"\\(.+\\)\"
- When I go to beginning of buffer
- When I go to end of buffer
- When I go to beginning of line
- When I go to end of line
- When I start an action chain
- When I execute the action chain
- When I call \"\\(.+\\)\"
- When I press \"\\(.+\\)\"
- When I quit
- When I type \"\\(.+\\)\"
- When I turn on \\(.+\\)
- When I set \\(.+\\) to \\(.+\\)
- When I load the following:
- When I open temp file \"\\(.+\\)\"
- Then I should see message \"\\(.+\\)\"
- Given there is no region selected
- Given transient mark mode is \\(active\\|inactive\\)
- When I set the mark
- When I pop the mark
- Then the region should be\\(?: \"\\(.*\\)\"\\|:\\)
- Then the region should not be active
- When I insert\\(?: \"\\(.+\\)\"\\|:\\)
- Then I should see\\(?: \"\\(.+\\)\"\\|:\\)
- Then I should not see\\(?: \"\\(.+\\)\"\\|:\\)
- Then I should see pattern\\(?: \"\\(.+\\)\"\\|:\\)
- Then I should not see pattern\\(?: \"\\(.+\\)\"\\|:\\)
- When I select \"\\(.+\\)\"
- Then I should not see anything$\\|^the buffer should be empty
- Then current point should be in bold
- Then current point should be in italic
- Then current point should be in strike-through
- Then current point should be in underline
- Then current point should have the \\([-[:alnum:]]+\\) face
- Then current point should have no face
- When I delete other windows

## Contributing

Contribution is much welcome! When adding new features, please write
tests for them!

Install [cask](https://github.com/cask/cask) if you haven't already,
then:

    $ cd /path/to/espuds
    $ cask

Run all tests with:

    $ make
