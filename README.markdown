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

There are three different kind of steps a feature can have: **regular**,
**py-string** or **table**.

You define a step definition by calling the function **Given**,
**When**, **Then**, **And** or **But** with two arguments. The first
is the match string and the second is a function (lambda or symbol).

### Regular
A regular step is a step that only consumes one line. The most basic
step looks like this:
    Given something is true
    
The step definition translation would be:
(Given "something is true"
       (lambda ()
         ;; Do something
         ))
         
The second function argument could also have been a symbol like this:
    (Given "something is true" 'do-something)
       
Steps can take arbitrary many arguments. The arguments sent to the
functions are all match groupings from the step and the match
string.

Sending a buffer name as argument
    Given I am in buffer "somebuffer"
    
Could be translated like this
    (Given "I am in buffer \"\\(.+\\)\""
       (lambda (buffer)
         ;; Do something with buffer
         ))
         
Sending more than one argument
    Given I am in buffer "somebuffer" with text "Some text"
    
Could be translated like this
    (Given "I am in buffer \"\\(.+\\)\" with text \"\\(.+\\)\""
       (lambda (buffer text)
         ;; Do something with buffer and text
         ))

### Py String
A "Py String" step looks like this
    Given the following text:
      """
      some text
      """
    
That step could be translated like this
    (Given "the following text:"
           (lambda (text)
             ;; Do something with text
             ))

It is also possible to send arguments. The py string text will then be
the last argument.

    Given the following text in buffer "some buffer":
      """
      some text
      """
    
That step could be translated like this
    (Given "the following text in buffer \"\\(.+\\)\":"
           (lambda (buffer text)
             ;; Do something with buffer and text
             ))

### Table
A table step looks like this
    Given these meals:
      | meal      | price |
      | Hamburger | $4.50 |
      | Pizza     | $5.30 |
    
That step could be translated like this
    (Given "the following meals:"
           (lambda (meals)
             ;; Do something with text
             ))
             
The argument **meals** would in this case be this list:
    (
     ("Hamburger" "$4.50")
     ("Pizza" "$5.30")
     )
     
Note that the header line is not included.


It is also possible to send arguments. The table text will then be
the last argument.

    Given these meals at "fast food":
      | meal      | price |
      | Hamburger | $4.50 |
      | Pizza     | $5.30 |
    
That step could be translated like this
    (Given "these meals at \"\\(.+\\)\":"
           (lambda (restaurant meals)
             ;; Do something with restaurant and meals
             ))


## Contributing
We all test different kind of applications. It is therefore important
that you contribute the step definitions you find useful. Send me an
email, a pm, a pull request or create an issue with your improvement.
