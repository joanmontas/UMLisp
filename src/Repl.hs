-- /src/Repl.hs

-- Copyright Joan Montas
-- All rights reserved.
-- License under GNU General Public License v3.0
module Repl (
    repl
) where

import System.IO
import System.Console.ANSI

import Lexer
import Parser
import Evaluater

repl :: Scope -> IO ()
repl s = do
    putStr "λ> "
    hFlush stdout
    input <- getLine
    case input of
        "exit()"  -> do
                        putStrLn "You Have Been UMLisped. - Joan Montas"
        "clear()"  -> do
                        putStrLn "Clearing..."
                        clearScreen
                        repl s
        otherwise -> do
                        --putStrLn input
                        let t = tokenize input []
                        case t of
                            [] -> repl s
                            [LexErrorToken lexError] -> do
                                                    putStrLn (show (LexErrorToken lexError))
                                                    repl s
                            otherwise         -> do
                                                    (t', s') <- parseEval t s
                                                    repl s'

parseEval :: [Token] -> Scope -> IO ([Token], Scope)
parseEval [] s = pure ([], s)
parseEval t s = do
          let (t', ast) = parse t []
          case ast of
            [A(ParseErrorAtom parError)] -> do
                                            putStrLn (show (ParseErrorAtom parError))
                                            pure ([], s)
            otherwise                 -> do
                                            let (e, s', r)        = eval ast s    []
                                            case r of
                                                {-(A(EvalErrorAtom _)):er -> do
                                                                                putStrLn (show (EvalErrorAtom evalError))
                                                                                parseEval t' s'-}
                                                ((A(EvalErrorAtom _)):er) -> do
                                                                                putStrLn $ show (r)
                                                                                parseEval t' s'
                                                otherwise                 -> do
                                                                                putStrLn (show (head r))
                                                                                parseEval t' s'
