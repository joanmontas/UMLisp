-- /app/Main.hs

-- Copyright Joan Montas
-- All rights reserved.
-- License under GNU General Public License v3.0

module Main where
import System.IO
import Lexer
import Parser
import Evaluater
import Repl

main :: IO ()
main = do
    putStrLn "UMLisp  Copyright (C) 2023  Joan Montas\nThis program comes with ABSOLUTELY NO WARRANTY: https://www.gnu.org/licenses/gpl-3.0.en.html#license-text"
    putStrLn "This is free software, and you are welcome to redistribute it"
    putStrLn "under certain conditions: Name the Author (Joan Montas)"
    putStrLn "\n\n\nWelcome to UMLisp. type exit() to exit or clear() to clear screen"
    repl makeGlobalScope