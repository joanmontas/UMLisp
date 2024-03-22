
# UMLisp
Simple Language With Lisp-like syntax.

## Description

This is a very simple language written in Haskell.
It contains Lisp-Like syntax. I had lots of fun implementing it; Learnt alot during the way.
Not bad for a day's work In my opinion.

The project was done via "blackbox" (and highly un-optimized) method. I ran things on Lisp's repl and saw what happened and
tried imitading it.
However this is definitely not the way to go (... fun none the less).

Cheers!
- Joan Montas
  
# Features
# List
(list 1 2 "asd" 3 )
###
(append (list 1 2 3) "appended")
###
(append (list 1 2 3) (list "tobe-appended-1" "tobe-appended-2" 5656))
###
(nth 1 (list 1 "get this value" 3) )
###
(insertAtNth 0 (list 1 3) "insert-this-value")
###
(removeAtNth 2 (list 1 3 "remove me I dare you"))
###
(replaceAtNth 2 (list 1 3 "remove me I dare you") "replacing incoming!")
# List Lazy Evaluation
(list 1 2 (+ 2 1)) -- (+ 1 2) not evaluated
###
(nth 2 (list 1 2 (+ 2 1))) -- (+ 1 2) evaluated as 3
# Introspecting
(defun foo(a) (* 45 65 234 567) )
###
foo -- prints to terminal the abstract syntax "tree" of the function
## How to
# Build the program
cabal build UMLisp.cabal
# Run the program
cabal run UMLisp
# Exit Program
λ> exit()
# Clear Program
λ> clear()
# Test Parts Individually
cabal repl or cabal repl src/Parser.hs
# Unit Test
cabal test
###
Please look at the /test/EvaluaterTest.hs as reference

## Future work
    # Research internals of Lisp and Haskell
        1. Lisp: From a layman's perspective it seems that I could have avoid writting uncessary code.
            It seems that all non-atomic structure are a sort of list '(+ 123 5645)', '(def foo (a) (+ 1 a))'...
            I guess I could have done this for many things, exept for special forms?... anyways do your research
        2. Haskell: Better understand memory management. Learn about optimization strategies.
    # Work on consistent code practice (any good reference to haskell style?)
        1. Too much nesting, break it down into smaller functions
            or use do statement, apply early exit in those do statement
        2. Use case of when "extacting" information from object (also good choice instead of classic "expectpeek" functions), otherwise use if-then-else 
        3. Better naming
            local variables should be short
            consistent naming
        4. Parse althought it returns an array of Sexpression... only one Sexpression should be return per 'parse'
            unless is an error, then keep appending the error to give better information to the user
            (do a little more research on the standard/convention).
        5. Modify to IO() this way a 'print' like function can be implemented in UMLisp
            If user call prints (no yet implemented) then wait until String is printed to the terminal.
        6. DOCUMENT!
    # Overall
        1. Re-write the entire thing
## Authors

Joan Montas

## License

    UMLisp  Copyright (C) 2023  Joan Montas
    This program comes with ABSOLUTELY NO WARRANTY: https://www.gnu.org/licenses/gpl-3.0.en.html#license-text
    This is free software, and you are welcome to redistribute it
    under certain conditions: Name the Author (Joan Montas)
    This project is licensed under the GNU v3 License - see the LICENSE.md file for details
