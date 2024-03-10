-- Copyright Joan Montas
-- All rights reserved.
-- License under GNU General Public License v3.0
--
-- I learned Haskell Today, I also wanted to learn Lisp.
-- So, I made a sub-set of a Lisp-Like Language in Haskell.
-- Best of both world!

import qualified Data.Map as Map
import Data.Char
import Data.Typeable

-- token
data Token = IdentToken {identValue :: String}      |
             IntegerToken {integerValue :: Integer} |
             StringToken {stringValue :: String}    |
             LParenToken Char                       |
             RParenToken Char                       |
             OperatorToken { operatorValue :: Char} |
             NoneToken String                       |
             LexErrorToken String                   |
             ParseEvalErrorToken String
  deriving Show

-- execution results
emptyListOfToken :: [Token]
emptyListOfToken = []

-- -- get type
-- TODO(JoanMontas) hello from the future, could just save the type inside
--                  the token itself
-- TODO(JoanMontas) hello from the future future, not only that, we can 
--                  just pattern match ... just like I did here but
--                  simply put it inside the actual function where is needed
getTokenType :: Token -> String
getTokenType (IdentToken v)     = "IdentToken"
getTokenType (IntegerToken v)   = "IntegerToken"
getTokenType (StringToken v)    = "StringToken"
getTokenType (OperatorToken v)  = "OperatorToken"
getTokenType (LParenToken v)    = "LParenToken"
getTokenType (RParenToken v)    = "RParenToken"
getTokenType (NoneToken v)      = "NoneToken"

-- -- tokenizer/lexer, simply read ahead
tokenize :: String -> [Token] -> [Token]
tokenize s l
  | s == "" = l
  | head s == '('      = tokenize (tail s) ( l ++ [LParenToken '('])
  | head s == ')'      = tokenize (tail s) ( l ++ [RParenToken ')'])
  | head s == '+'      = tokenize (tail s) ( l ++ [OperatorToken  '+'])
  | head s == '-'      =
    if isDigit(s !! 1)                                              -- look ahead! see if its a negative number
    then lexInteger s l
    else tokenize (tail s) (l ++ [OperatorToken '-'])
  | head s == '*'      = tokenize (tail s) ( l ++ [OperatorToken  '*'])
  | head s == '"'      = lexString s l
  | isSpace(head s)    = tokenize (tail s) (l)                         -- eat white space
  | isDigit(head s)    = lexInteger s l
  | isAlphaNum(head s) = lexIdent s l
  | otherwise          = [LexErrorToken "Lexer Error: Unkown Token"]

-- lexer/tokenizer helper
lexInteger :: String -> [Token] -> [Token]
lexInteger s l = if (head s == '-')
                 then let negativeRslt = lexIntegerHelper (tail s) "-"
                      in tokenize (fst negativeRslt) (l ++ [ IntegerToken (read (snd negativeRslt) :: Integer)])
                 else let positiveRslt = lexIntegerHelper s ""
                      in tokenize (fst positiveRslt) (l ++ [ IntegerToken (read (snd positiveRslt) :: Integer)])

lexIntegerHelper :: String -> String -> (String, String)
lexIntegerHelper s n
  | s == "" = (s, n)
  | isDigit (head s) = lexIntegerHelper (tail s) (n ++ take 1 s)
  | otherwise = (s, n)                                          -- if find some non-int, leave it as is

lexIdent :: String -> [Token] -> [Token]
lexIdent s l = let rslt = lexIdentHelper s ""
               in tokenize (fst rslt) (l ++ [IdentToken (snd rslt)])

lexIdentHelper :: String -> String -> (String, String)
lexIdentHelper s n
  | s == "" = (s, n)
  | isAlphaNum (head s) = lexIdentHelper (tail s) (n ++ take 1 s)
  | otherwise = (s, n)

-- given a string, and a list of Token, parse the string and continue tokenizing
lexString :: String -> [Token] -> [Token]
lexString s l = let rslt = lexStringHelper (tail s) ""
                in if (head (fst rslt) /= '"')
                   then [LexErrorToken "Lexer-Error: Did not close String"]
                   else tokenize (tail (fst rslt)) (l ++ [StringToken (snd rslt)])

-- 
lexStringHelper :: String -> String -> (String, String)
lexStringHelper s n
  | s == "" = (" ", n)        -- reaches end of string without finding a closing quote ERROR
  | head s == '"' = (s, n)
  | otherwise = lexStringHelper (tail s) (n ++ [head s])        -- save the char, recurse and look for quote

-- -- simple implementation of lexical scoping. I know its not
-- efficient since the prev scopes are copied everytime we
-- make/go-to a new scope. Eventually I will get to that.
-- First I have to read of IORef.
-- provides variables scope
data Scope = LocalScope {local :: (Map.Map String Token), prev  ::Scope } |
             EmptyScope

-- make global scope, i.e empty prev
makeGlobalScope :: Scope
makeGlobalScope = LocalScope (Map.empty) EmptyScope

-- make a local scope, i.e non-empty prev
makeLocalScope :: Scope -> Scope
makeLocalScope s = LocalScope (Map.empty) s

-- TODO(JoanMontas) make a helper function so you can add tabs per scope and have it pretty print
instance Show Scope where
  show EmptyScope       = "Empty\n"
  show (LocalScope l p) = "\n=======\n" ++
                            "Local: " ++
                               show l ++
                            "\nprev:" ++
                               show p ++
                            "========\n"

-- insert a token to the given scope
insertTokenToScope :: Scope -> String -> Token -> Scope
insertTokenToScope s k t = LocalScope (Map.insert k t (local s)) (prev s)

-- find a token via lexically scoping, how I like to call it "bubble up"
findToken :: Scope -> String -> Token
findToken EmptyScope k = ParseEvalErrorToken "ERROR: Variable not found in any scope"
findToken s k = case Map.lookup k (local s) of
  Just t -> t
  Nothing -> findToken (prev s) k

-- -- parse-eval
-- we are just going to execute the ast directly
eval :: [Token] -> Scope -> (Token, [Token], Scope)
eval t s
  | length t == 0 = (NoneToken "NIL", t, s)
  | getTokenType (head t) == "IntegerToken" = (head t, tail t, s)
  | getTokenType (head t) == "StringToken"  = (head t, tail t, s)
  | getTokenType (head t) == "LParenToken"  = evalParen (tail t) s
  | getTokenType (head t) == "IdentToken"   = getToken t s
  | otherwise = (ParseEvalErrorToken "Parse-Eval's Otherwise Error: Unkown token", t, s)

-- eval helper
firstTriplet :: (a, b, c) -> a
firstTriplet (a, b, c) =  a
secondTriplet :: (a, b, c) -> b
secondTriplet (a, b, c) = b
thirdTriplet :: (a, b, c) -> c
thirdTriplet (a, b, c) = c

evalOperator :: [Token] -> Scope -> (Token, [Token], Scope)
evalOperator t s = let 
  x = eval (tail t) (s)
  y = eval (secondTriplet x) (thirdTriplet x)
  in
    case (head t) of
      OperatorToken '+' ->  let z = evalPlus (firstTriplet x) (firstTriplet y)
                         in (z, (secondTriplet y), thirdTriplet y)
      OperatorToken '-' ->  let z = evalMinus (firstTriplet x) (firstTriplet y)
                         in (z, (secondTriplet y), thirdTriplet y)
      OperatorToken '*' ->  let z = evalMult (firstTriplet x) (firstTriplet y)
                         in (z, (secondTriplet y), thirdTriplet y)
      otherwise      -> (ParseEvalErrorToken "Parse-Eval's evalOperator Error: Unkown Operator", t, s)

evalPlus :: Token -> Token -> Token
evalPlus (IntegerToken x) (IntegerToken y) = IntegerToken (x + y)
evalPlus _ _ = ParseEvalErrorToken "Parse-Eval's EvalPlus Error: Two Given Token Are Not Known"

evalMinus :: Token -> Token -> Token
evalMinus (IntegerToken x) (IntegerToken y) = IntegerToken (x - y)
evalMinus _ _ = ParseEvalErrorToken "Parse-Eval's EvalMinus Error: Two Given Token Are Not Known"

evalMult :: Token -> Token -> Token
evalMult (IntegerToken x) (IntegerToken y) = IntegerToken (x * y)
evalMult _ _ = ParseEvalErrorToken "Parse-Eval's EvalMult Error: Two Given Token Are Not Known"

evalParen :: [Token] -> Scope -> (Token, [Token], Scope)
evalParen t s
  | length t == 0 = ((ParseEvalErrorToken "Parse-Eval's evalParen Error: Parenthesis Not Closed"), t, s)
  | getTokenType (head t) == "RParenToken" = ((NoneToken "NIL"), (tail t), s)
  | getTokenType (head t) == "OperatorToken" = let rslt = evalOperator t s
                                            in if length (secondTriplet rslt) == 0
                                               then ((ParseEvalErrorToken "Parse-Eval's evalParen Error: Parenthesis Not Closed"), t, s)
                                               else if getTokenType (head (secondTriplet rslt)) == "RParenToken"
                                                    then ((firstTriplet rslt), (tail (secondTriplet rslt)), (thirdTriplet rslt))
                                                    else ((ParseEvalErrorToken "Parse-Eval's evalParen Error: Parenthesis Not Closed"), t, s)
  | getTokenType (head t) == "IdentToken" = let rslt = evalIdent t s
                                            in if length (secondTriplet rslt) == 0
                                               then ((ParseEvalErrorToken "Parse-Eval's evalParen Error: Parenthesis Not Closed"), t, s)
                                               else if getTokenType (head (secondTriplet rslt)) == "RParenToken"
                                                    then ((firstTriplet rslt), (tail (secondTriplet rslt)), (thirdTriplet rslt))
                                                    else ((ParseEvalErrorToken "Parse-Eval's evalParen Error: Parenthesis Not Closed"), t, s)
  | otherwise = ((ParseEvalErrorToken "Parse-Eval's evalParen Error: Unknown condition"), t, s)

evalIdent :: [Token] -> Scope -> (Token, [Token], Scope)
evalIdent t s = case (head t) of
  IdentToken "defvar" -> defVarBuiltIn (tail t) s
  otherwise           -> ((ParseEvalErrorToken "Parse-Eval's evalIdent Error: Unkown ident"), t, s)

defVarBuiltIn :: [Token] -> Scope -> (Token, [Token], Scope)
defVarBuiltIn t s = if getTokenType (head t) /= "IdentToken"
                    then ((ParseEvalErrorToken "Parse-Eval's defVarBuiltIn Error: Expected an Ident") , t, s)
                    else let rslt = eval (tail t) s
                             newScope = insertTokenToScope (thirdTriplet rslt) (identValue (head t)) (firstTriplet rslt)
                         in ( (NoneToken "NIL"), (secondTriplet rslt),newScope)

getToken :: [Token] -> Scope -> (Token, [Token], Scope)
getToken t s = let rslt = findToken s (identValue (head t))
               in (rslt, (tail t), s)

-- repl components
main :: IO ()
main = do
  -- let inputs = "(+ 321 (+ 0 (* 1000 4)))"
  -- let inputs = "\"Hello, World!\""
  -- let inputs = "(+ 99 3     )"
  let inputsZERO = "(defvar myVar (+ 1 123))"
  let outputsZERO = eval (tokenize inputsZERO emptyListOfToken) makeGlobalScope
  print outputsZERO

  print "----------------------- now getting value"

  -- let inputsONE = "(+ 1 myVar)"
  let inputsONE = "myVarr"
  let outputsONE = eval (tokenize inputsONE emptyListOfToken) (thirdTriplet outputsZERO)
  print outputsONE


