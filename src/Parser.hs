-- /src/Parser.hs

-- Copyright Joan Montas
-- All rights reserved.
-- License under GNU General Public License v3.0

module Parser
  ( Atom (..),
    Sexpression (..),
    parse,
    makeGlobalScope,
    makeLocalScope,
    insertSexpToScope,
    insertSexpToGlobalScope,
    findSexp,
    Scope,
    getPrev,
    expectSexpression,
    evalErrorAtom,
    identAtom,
    integerAtom,
  )
where

import qualified Data.Map as Map
import Lexer

data Sexpression
  = A Atom
  | Debug [Sexpression]
  | CallExpression {callExpressionValue :: Sexpression, callExpressionArgs :: [Sexpression]} -- ( Ident list-of-Sexpression )
  | ListExpression [Sexpression]
  | ConsExpression Sexpression Sexpression
  | IfExpression Sexpression Sexpression Sexpression -- ( if condition body else-body )
  | FunctionExpression Sexpression [Sexpression] Sexpression -- name/Ident argument (list of ident), body... own scope too when eval
  deriving (Show, Eq)

data Atom
  = IntegerAtom {integerAtomValue :: Integer}
  | StringAtom {stringAtomValue :: String}
  | IdentAtom {identAtomValue :: String}
  | NilAtom {nilAtomValue :: String}
  | TrueAtom {trueAtomValue :: String}
  | ParseErrorAtom {parseErrorAtomValue :: String}
  | EvalErrorAtom {evalErrorAtomValue :: String} -- For the future
  deriving (Show, Eq)

-- S-expression type
integerAtom :: String
integerAtom = "integerAtom"

stringAtom :: String
stringAtom = "stringAtom"

identAtom :: String
identAtom = "identAtom"

nilAtom :: String
nilAtom = "nilAtom"

trueAtom :: String
trueAtom = "trueAtom"

parseErrorAtom :: String
parseErrorAtom = "parseErrorAtom"

evalErrorAtom :: String
evalErrorAtom = "evalErrorAtom"

-- Makes sure the given token is the desired type
-- TOOO(JoanMontas) just as lexpexceptpeek.... change Sexpression to [Sexpression] this avoid having to check for empty list
--                  dry your code
expectSexpression :: Sexpression -> String -> Bool
expectSexpression (A (ParseErrorAtom _)) "parseErrorAtom" = True
expectSexpression (A (EvalErrorAtom _)) "evalErrorAtom" = True
expectSexpression (A (IntegerAtom _)) "integerAtom" = True
expectSexpression (A (StringAtom _)) "stringAtom" = True
expectSexpression (A (IdentAtom _)) "identAtom" = True
expectSexpression (A (TrueAtom _)) "trueAtom" = True
expectSexpression (A (NilAtom _)) "nilAtom" = True
expectSexpression _ _ = False

-- -- -- -- -- -- -- -- -- -- -- Parse
parse :: [Token] -> [Sexpression] -> ([Token], [Sexpression])
parse [] s =
  ( [],
    s
      ++ [ A
             ( ParseErrorAtom
                 "parse's parse Error: Received empmty token" -- TODO(JoanMontas) remove and return nil instead
             )
         ]
  )
-- parse atoms
parse ((IntegerToken v) : ts) s = (ts, s ++ [A (IntegerAtom v)])
parse ((StringToken v) : ts) s = (ts, s ++ [A (StringAtom v)])
parse ((IdentToken "T") : ts) s = (ts, s ++ [A (TrueAtom "T")])
parse ((IdentToken "NIL") : ts) s = (ts, s ++ [A (NilAtom "NIL")])
parse ((IdentToken v) : ts) s = (ts, s ++ [A (IdentAtom v)])
parse
  ((LParenToken c) : (RParenToken c') : ts)
  s = (ts, s ++ [A (NilAtom "NIL")])
-- parse S-Expression
parse ((LParenToken c) : ts) s = parseSexpression ts s
parse _ s =
  ( [],
    ( A
        ( ParseErrorAtom
            "parse's parse Error: Unkown Parser Condition"
        )
    )
      : s
  )

-- parse S-expression, if not one of the define statements(if, for, ...)
parseSexpression :: [Token] -> [Sexpression] -> ([Token], [Sexpression])
parseSexpression ((OperatorToken v) : ts) s = parseCallSexpression v ts s
parseSexpression ((IdentToken v) : ts) s = parseCallSexpression v ts s
parseSexpression _ s = ([], [A (ParseErrorAtom "parse's parseSexpression Error: Unkown Call Condition")])

parseCallSexpression :: String -> [Token] -> [Sexpression] -> ([Token], [Sexpression])
parseCallSexpression _ [] _ =
  ( [],
    [ A
        ( ParseErrorAtom
            "parse's parseCallSexpression Error: S-Expression not closed"
        )
    ]
  )
parseCallSexpression "if" t s = parseIfStatement t s
parseCallSexpression "defun" t s = parseDefunStatement t s
parseCallSexpression v ((RParenToken c) : ts) s = (ts, s ++ [CallExpression (A (IdentAtom v)) []])
parseCallSexpression v (t : ts) s =
  let (t', s', g) = parseGroupSexpression (t : ts) s []
   in if (not (null s')) && expectSexpression (head s') parseErrorAtom
        then ([], (s'))
        else (t', s' ++ [CallExpression (A (IdentAtom v)) g])

parseGroupSexpression :: [Token] -> [Sexpression] -> [Sexpression] -> ([Token], [Sexpression], [Sexpression])
parseGroupSexpression [] s g =
  ( [],
    ( ( A
          ( ParseErrorAtom
              "parse's parseGroupSexpression Error: S-Expression not closed"
          )
      )
        : []
    ),
    g
  )
parseGroupSexpression ((RParenToken c) : ts) s g = (ts, s, g)
parseGroupSexpression t s g =
  let (t', (s' : ss')) = parse t s
   in if expectSexpression s' parseErrorAtom
        then ([], ([s']), [])
        else parseGroupSexpression (t') (ss') (g ++ [s'])

parseIfStatement :: [Token] -> [Sexpression] -> ([Token], [Sexpression])
parseIfStatement [] s = ([], [A (ParseErrorAtom "parse's parseIfStatement Error: No Condition Body, or Else-Body Found")])
parseIfStatement t s =
  let (t', s') = parse t []
   in if ((not (null s')) && expectSexpression (head s') parseErrorAtom) || (length s') == 0
        then ([], [A (ParseErrorAtom "parse's parseIfStatement Error: Could Not Parse Condition-Statement")])
        else
          let (t'', s'') = parse t' []
           in if ((not (null s'')) && expectSexpression (head s'') parseErrorAtom) || (length s'') == 0
                then ([], [A (ParseErrorAtom "parse's parseIfStatement Error: Could Not Parse Body")])
                else case t'' of
                  [] -> ([], [A (ParseErrorAtom "parse's parseIfStatement Error: Did Not Close If-Statement")])
                  ((RParenToken _) : t''s) -> (t''s, [IfExpression (head s') (head s'') (A (NilAtom "NIL"))])
                  othewise ->
                    let (t''', s''') = parse t'' []
                     in if ((not (null s''')) && expectSexpression (head s''') parseErrorAtom) || (length s''') == 0
                          then ([], [A (ParseErrorAtom "parse's parseIfStatement Error: Could Not Parse Else-Body")])
                          else case t''' of
                            ((RParenToken _) : t'''s) -> (t'''s, [IfExpression (head s') (head s'') (head s''')])
                            otherwise -> ([], [A (ParseErrorAtom "parse's parseIfStatement Error: Did Not Close If-Statement")])

parseDefunStatement :: [Token] -> [Sexpression] -> ([Token], [Sexpression])
parseDefunStatement [] s = ([], [A (ParseErrorAtom "parse's parseDefunStatement Error: No Name, Arguments or Body Found")])
parseDefunStatement (t : ts) s = case t of
  (IdentToken functionname) ->
    if (not (lexExpectPeekToken ts lParenToken))
      then ([], [A (ParseErrorAtom "parse's parseDefunStatement Error: Missing Argument")])
      else
        let (t', s', arg) = parseDefunArguments (tail ts) s []
         in if (length s' > 0) && expectSexpression (head s') parseErrorAtom
              then ([], s')
              else
                let (t'', s'') = parse t' []
                 in if (length s'' > 0) && expectSexpression (head s'') parseErrorAtom
                      then ([], [A (ParseErrorAtom "parse's parseDefunStatement Error: Error Parsing Body")])
                      else
                        if not (lexExpectPeekToken t'' rParenToken)
                          then ([], [A (ParseErrorAtom "parse's parseDefunStatement Error: Did not Close defun-Statement")])
                          else ((tail t''), [FunctionExpression (A (IdentAtom functionname)) (arg) (head s'')])
  otherwise -> ([], [A (ParseErrorAtom "parse's parseDefunStatement Error: No Name Found")])

parseDefunArguments :: [Token] -> [Sexpression] -> [Sexpression] -> ([Token], [Sexpression], [Sexpression])
parseDefunArguments [] s g =
  ( [],
    ( ( A
          ( ParseErrorAtom
              "parse's parseDefunArguments Error: Arguments not closed"
          )
      )
        : []
    ),
    g
  )
parseDefunArguments ((RParenToken ')') : ts) s g = (ts, s, g)
parseDefunArguments ((IdentToken c) : ts) s g = parseDefunArguments ts s (g ++ [A (IdentAtom c)])
parseDefunArguments t s g =
  ( [],
    [ ( A
          ( ParseErrorAtom
              "parse's parseDefunArguments Error: Unexpected Value"
          )
      )
    ],
    [ ( A
          ( ParseErrorAtom
              "parse's parseDefunArguments Error: Unexpected Value"
          )
      )
    ]
  )

parseWhileStatement :: [Token] -> [Sexpression] -> ([Token], [Sexpression])
parseWhileStatement t s = undefined

-- -- -- -- -- -- -- -- -- -- -- scope -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- NOTE(JoanMontas) I need to investigate more about haskell's memory management...
data Scope
  = LocalScope {local :: (Map.Map String Sexpression), prev :: Scope}
  | EmptyScope

-- make global scope, i.e empty prev
makeGlobalScope :: Scope
makeGlobalScope = LocalScope (Map.empty) EmptyScope

-- make a local scope, i.e non-empty prev
makeLocalScope :: Scope -> Scope
makeLocalScope s = LocalScope (Map.empty) s

-- insert a Sexp to the given scope
insertSexpToScope :: Scope -> String -> Sexpression -> Scope
insertSexpToScope s k se = LocalScope (Map.insert k se (local s)) (prev s)

-- insert a Sexp to the global Scope
insertSexpToGlobalScope :: Scope -> String -> Sexpression -> Scope
insertSexpToGlobalScope s k se = case (prev s) of
  EmptyScope -> insertSexpToScope s k se
  otherwise -> LocalScope (local s) (insertSexpToGlobalScope (prev s) k se)

-- find a Sexp via lexically scoping, how I like to call it "bubble up"
findSexp :: Scope -> String -> Sexpression
findSexp EmptyScope k = A (EvalErrorAtom "EvalErrorAtom ERROR: Variable not found in any scope")
findSexp s k = case Map.lookup k (local s) of
  Just t -> t
  Nothing -> findSexp (prev s) k

getPrev :: Scope -> Scope
getPrev s = prev s

-- TODO(JoanMontas) make a helper function so you can add tabs per scope and
-- have it pretty print
instance Show Scope where
  show EmptyScope = "Empty\n"
  show (LocalScope l p) =
    "\n=======\n"
      ++ "Local: "
      ++ show l
      ++ "\nprev:"
      ++ show p
      ++ "========\n"
