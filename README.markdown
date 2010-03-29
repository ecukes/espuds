# Espuds - Ecukes step definitions

First of all. If you don't know what
[Ecukes](http://github.com/rejeep/ecukes)
it, go read up about it. If you have, you should know that in order to
test with Ecukes, you need to translate your feature files in some way
so that Emacs understands them. You do that by definining step
definitions.

Espuds collects the most common step definitions (or spuds) that you
almost always want when using Ecukes.

## Usage
To use Espuds, you have load it in your project file **features/support.el**.
    (add-to-list 'load-path "/path/to/espuds)
    (require 'espuds)
    
Thats it!

## Contributing
We all test different kind of applications. It is therefore important
that you contribute the step definitions you find useful. Send me an
email, a pm, a pull request or create an issue with your improvement.
