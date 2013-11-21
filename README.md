# Espuds - Ecukes step definitions [![Build Status](https://api.travis-ci.org/ecukes/espuds.el.png?branch=master)](http://travis-ci.org/ecukes/espuds.el)

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

## Contributing

Contribution is much welcome! When adding new features, please write
tests for them!

Install [cask](https://github.com/cask/cask) if you haven't already,
then:

    $ cd /path/to/espuds
    $ cask

Run all tests with:

    $ make
