# Espuds - Ecukes step definitions

First of all. If you don't know what
[Ecukes](http://github.com/rejeep/ecukes) it, go read up about it. If
you have, you should know that in order to test with Ecukes, you need
to translate your steps so that Emacs understands them. You do that
with step definitions.

Espuds is a collection of the most commonly used step definitions.


## Usage
To use Espuds, you have load it in your **features/support.el** file.
    (add-to-list 'load-path "/path/to/espuds)
    (require 'espuds)
    
Thats it!


## Step Definitions
See source file for description of each step definition.


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
We all test different kind of applications. It is therefore important
that you contribute the step definitions you find useful. Send me an
email, a pm, a pull request or create an issue with your improvement.
