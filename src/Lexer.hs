-- /src/Lexer.hs

module Lexer (
  Token(..),
  emptyListOfToken,
  tokenize,
  lexExpectPeekToken,
  lParenToken,
  rParenToken
) where

import Data.Char
import Data.Typeable



data Token = IdentToken      {identValue     :: String }            |
             IntegerToken    {integerValue   :: Integer}            |
             StringToken     {stringValue    :: String }            |
             LParenToken     { lparenValue   :: Char   }            |
             RParenToken     { rparenValue   :: Char   }            |
             OperatorToken   {operatorValue  :: String }            |
             NoneToken       {nonValue       :: String }            | -- TODO(JoanMontas) Irrelevant, delete
             BooleanToken    {booleanValue   :: String }            | -- TODO(JoanMontas) irrelevant, delete
             LexErrorToken    String
  deriving (Show, Eq)

-- token types
identToken     :: String
identToken      = "IdentToken"
integerToken   :: String
integerToken    = "IntegerToken"
stringToken    :: String
stringToken     = "StringToken"
operatorToken  :: String
operatorToken  = "OperatorToken"
lParenToken    :: String
lParenToken    = "LParenToken"
rParenToken    :: String
rParenToken    = "RParenToken"
noneToken      :: String
noneToken      = "NoneToken"
booleanToken   :: String
booleanToken   = "BooleanToken"
lexErrorToken  :: String
lexErrorToken  = "LexErrorToken"

-- execution results
emptyListOfToken :: [Token]
emptyListOfToken =  []

-- -- -- -- -- -- -- -- -- -- -- tokenizer/lexer, simply read ahead
tokenize :: String -> [Token] -> [Token]
tokenize s l
  | s == "" = l
  | head s == '('      = tokenize (tail s) ( l ++ [LParenToken '('])
  | head s == ')'      = tokenize (tail s) ( l ++ [RParenToken ')'])
  | head s == '+'      = tokenize (tail s) ( l ++ [OperatorToken  "+"])
  | head s == '-'      =
                          if   ((length (tail s))> 0) && (isDigit(s !! 1))  -- look ahead! see if its a negative number
                          then lexInteger s l
                          else tokenize (tail s) (l ++ [OperatorToken "-"])
  | head s == '*'      = tokenize (tail s) ( l ++ [OperatorToken  "*"])
  | head s == '>'      =
                          if   ((length (tail s)) > 0) && ((s !! 1) == '=') -- look ahead
                          then tokenize (tail (tail s)) (l ++ [OperatorToken ">="])
                          else tokenize (tail s) ( l ++ [OperatorToken  ">"])
  | head s == '<'      =
                          if   ((length (tail s))> 0) && ((s !! 1) == '=')  -- look ahead
                          then tokenize (tail (tail s)) (l ++ [OperatorToken "<="])
                          else tokenize (tail s) ( l ++ [OperatorToken  "<"])
  | head s == '"'      = lexString s l
  | isSpace(head s)    = tokenize (tail s) (l)      -- eat white space
  | isDigit(head s)    = lexInteger s l
  | isAlphaNum(head s) = lexIdent s l
  | otherwise          = [LexErrorToken "Lexer Error: Unkown Token"]

-- lexer/tokenizer helper
lexInteger :: String -> [Token] -> [Token]
lexInteger s l = if (head s == '-')
                 then let negativeRslt = lexIntegerHelper (tail s) "-"
                      in tokenize (fst negativeRslt)
                        (l ++ [ IntegerToken (read (snd negativeRslt) :: Integer)])
                 else let positiveRslt = lexIntegerHelper s ""
                      in tokenize (fst positiveRslt)
                        (l ++ [ IntegerToken (read (snd positiveRslt) :: Integer)])

lexIntegerHelper :: String -> String -> (String, String)
lexIntegerHelper s n
  | s == "" = (s, n)
  | isDigit (head s) = lexIntegerHelper (tail s) (n ++ take 1 s)
  | otherwise = (s, n)      -- if find some non-int, leave it as is

-- an ident starts with a alpha and ends with space... this is my definition atleast
lexIdent :: String -> [Token] -> [Token]
lexIdent s l = let rslt = lexIdentHelper s ""
               in tokenize (fst rslt) (l ++ [IdentToken (snd rslt)])

lexIdentHelper :: String -> String -> (String, String)
lexIdentHelper s n
  |  s       == ""           = (s, n)
  | (head s) == ' '          = (s, n)
  | not (isAlphaNum(head s)) = (s, n)
  | otherwise                = lexIdentHelper (tail s) (n ++ take 1 s)

-- given a string, and a list of Token, parse the string and continue tokenizing
lexString :: String -> [Token] -> [Token]
lexString s l = let rslt = lexStringHelper (tail s) ""
                in if   (head (fst rslt) /= '"')
                   then [LexErrorToken "Lexer-Error: Did not close String"]
                   else tokenize (tail (fst rslt)) (l ++ [StringToken (snd rslt)])

--
lexStringHelper :: String -> String -> (String, String)
lexStringHelper s n
  | s      == ""  = (" ", n)  -- reaches end of string without finding a closing quote ERROR
  | head s == '"' = (s  , n)
  | otherwise     =  lexStringHelper (tail s)
                    (n ++ [head s])     -- save the char, recurse and look for quote

-- to be used by the parse to look ahead L(1)
lexExpectPeekToken :: [Token] -> String -> Bool
lexExpectPeekToken ( ( IdentToken    v   ) : _ ) "IdenToken"     = True
lexExpectPeekToken ( ( IntegerToken  v   ) : _ ) "IntegerToken"  = True
lexExpectPeekToken ( ( StringToken   v   ) : _ ) "StringToken"   = True
lexExpectPeekToken ( ( OperatorToken v   ) : _ ) "OperatorToken" = True
lexExpectPeekToken ( ( LParenToken   '(' ) : _ ) "LParenToken"   = True
lexExpectPeekToken ( ( RParenToken   ')' ) : _ ) "RParenToken"   = True
lexExpectPeekToken ( ( NoneToken     v   ) : _ ) "NoneToken"     = True
lexExpectPeekToken ( ( BooleanToken  v   ) : _ ) "BooleanToken"  = True
lexExpectPeekToken ( ( LexErrorToken v   ) : _ ) "LexErrorToken" = True
lexExpectPeekToken _                       _                     = False

