-- /src/Evaluater.hs

-- Copyright Joan Montas
-- All rights reserved.
-- License under GNU General Public License v3.0

module Evaluater
  ( eval,
  )
where

import Data.Bool -- bool for isTruthy
import Lexer
import Parser

firstTriplet :: (a, b, c) -> a -- TODO(JoanMontas) delete this... we can just list-decompose. Leave it as a momento.
firstTriplet (a, b, c) = a

secondTriplet :: (a, b, c) -> b
secondTriplet (a, b, c) = b

thirdTriplet :: (a, b, c) -> c
thirdTriplet (a, b, c) = c

-- -- -- -- -- -- -- -- -- -- -- eval
lazyEval :: [Sexpression] -> Scope -> [Sexpression] -> ([Sexpression], Scope, [Sexpression]) -- TODO(JoanMontas) research about lazy eval
lazyEval [] s r = ([], s, [A (NilAtom "NIL")]) -- I am thinking I can call lazyEval at key moment
-- Computational Thinking Chapter 13 explains it very well, apperantly I need to save the environment
-- that way if I lazy eval inside a function, onced I exit, the Sexpression/Ident/Atoms that are
-- depended on it are not destroy
-- https://www.cs.virginia.edu/~evans/cs150/book/ch13-laziness.pdf
lazyEval (a : as) s r = undefined

eval :: [Sexpression] -> Scope -> [Sexpression] -> ([Sexpression], Scope, [Sexpression])
eval [] s results = ([], s, [A (NilAtom "NIL")])
-- eval atoms
eval ((A (IntegerAtom v)) : sp) s results = (sp, s, results ++ [A (IntegerAtom v)]) -- TODO(JoanMontas) fix things dependent on +.-, *
eval ((A (StringAtom v)) : sp) s results = (sp, s, [(A (StringAtom v))])
eval ((A (NilAtom v)) : sp) s results = (sp, s, [(A (NilAtom v))])
eval ((A (TrueAtom v)) : sp) s results = (sp, s, [(A (TrueAtom v))])
eval ((A (IdentAtom v)) : sp) s results =
  let r' = findSexp s v
   in if expectSexpression r' evalErrorAtom
        then ([], s, [r'])
        else (sp, s, [r'])
-- eval S-expression
eval ((IfExpression c b e) : sp) s results = evalIfExpression (IfExpression c b e) sp s results
eval ((CallExpression id arg) : sp) s results = evalCallExpression (CallExpression id arg) sp s results
eval ((FunctionExpression id arg body) : sp) s results = evalDefunExpression (FunctionExpression id arg body) sp s results
eval ((ListExpression v) : sp) s results = (sp, s, [ListExpression v])
eval _ s results = ([], s, [(A (EvalErrorAtom "eval's eval Error: Unkown Sexpression Nor Atom"))])

-- TODO(JoanMontas) this arithmetic expression are horrible... I need to re-do this asap
-- also standarize the coding in such arithmetic and logical expression
evalCallExpression :: Sexpression -> [Sexpression] -> Scope -> [Sexpression] -> ([Sexpression], Scope, [Sexpression])
evalCallExpression (CallExpression (A (IdentAtom "+")) arg) a s results = evalSum (A (IntegerAtom 0)) arg s [] a
evalCallExpression (CallExpression (A (IdentAtom "-")) arg) a s results = evalSub arg s []
evalCallExpression (CallExpression (A (IdentAtom "*")) arg) a s results = evalMult (A (IntegerAtom 1)) arg s [] a
evalCallExpression (CallExpression (A (IdentAtom ">")) arg) a s results =
  let (oldArg, s', evaledGroup) = evalGroupExpression arg s []
   in if (length evaledGroup > 0) && expectSexpression (head evaledGroup) evalErrorAtom
        then ([], s, evaledGroup)
        else (a, s', (evalGreater evaledGroup) : results)
evalCallExpression (CallExpression (A (IdentAtom "<")) arg) a s results =
  let (oldArg, s', evaledGroup) = evalGroupExpression arg s []
   in if (length evaledGroup > 0) && expectSexpression (head evaledGroup) evalErrorAtom
        then ([], s, evaledGroup)
        else (a, s', (evalLesser evaledGroup) : results)
evalCallExpression (CallExpression (A (IdentAtom ">=")) arg) a s results =
  let (oldArg, s', evaledGroup) = evalGroupExpression arg s []
   in if (length evaledGroup > 0) && expectSexpression (head evaledGroup) evalErrorAtom
        then ([], s, evaledGroup)
        else (a, s', (evalGreaterEqual evaledGroup) : results)
evalCallExpression (CallExpression (A (IdentAtom "<=")) arg) a s results =
  let (oldArg, s', evaledGroup) = evalGroupExpression arg s []
   in if (length evaledGroup > 0) && expectSexpression (head evaledGroup) evalErrorAtom
        then ([], s, evaledGroup)
        else (a, s', (evalLesserEqual evaledGroup) : results)
evalCallExpression (CallExpression (A (IdentAtom "not")) arg) a s results =
  if (length arg == 0) || (length arg > 1)
    then ([], s, [A (EvalErrorAtom "eval's not Error: Wrong parameter")])
    else
      let (a', s', r') = eval arg s []
       in case (head r') of
            (A (EvalErrorAtom v)) -> ([], s, r')
            otherwise ->
              if (isTruthy r')
                then (a, s', [A (NilAtom "NIL")])
                else (a, s', [A (TrueAtom "T")])
evalCallExpression (CallExpression (A (IdentAtom "and")) arg) a s results =
  let (a', s', r') = evalLogicalAnd arg s [] -- TODO(JoanMontas) finish this logical and
   in (a, s', r')
evalCallExpression (CallExpression (A (IdentAtom "or")) arg) a s results =
  let (a', s', r') = evalLogicalOr arg s [] -- TODO(JoanMontas) finish this logical or
   in (a, s', r')
evalCallExpression (CallExpression (A (IdentAtom "defvar")) arg) a s results =
  if not (length arg >= 1 && length arg < 3) || not (expectSexpression (head arg) identAtom)
    then ([], s, [A (EvalErrorAtom "eval's defvar Error: Wrong parameter")])
    else -- NOTE(JoanMontas) I do not have to evaluate, due to potential lazy evaluation features (more research is needed)

      let (oldArg, s', evaledGroup) = evalGroupExpression (tail arg) s []
       in if (length evaledGroup > 0) && expectSexpression (head evaledGroup) evalErrorAtom
            then ([], s, evaledGroup)
            else case (head arg) of
              (A (IdentAtom v)) ->
                if length evaledGroup > 0
                  then
                    let newScope = insertSexpToGlobalScope s' v (head evaledGroup)
                     in (a, newScope, [A (StringAtom v)])
                  else
                    let newScope = insertSexpToGlobalScope s' v (A (NilAtom "NIL"))
                     in (a, newScope, [A (StringAtom v)])
evalCallExpression (CallExpression (A (IdentAtom "setq")) arg) a s results =
  if (length arg < 2) || (length arg > 2) || (not (expectSexpression (head arg) identAtom))
    then ([], s, [A (EvalErrorAtom "eval's setq Error: Wrong parameter")])
    else case (head arg) of
      A (IdentAtom v) ->
        let originalValue = findSexp s v
         in if expectSexpression originalValue evalErrorAtom
              then ([], s, [originalValue])
              else
                let (a', s', r') = eval (tail arg) s []
                 in if expectSexpression (head r') evalErrorAtom
                      then ([], s, r')
                      else
                        let s'' = insertSexpToScope s' v (head r')
                         in (a, s'', r')
      otherwise -> ([], s, [A (EvalErrorAtom "eval's setq Error: Expected an Ident as argument")])
evalCallExpression (CallExpression (A (IdentAtom "progn")) arg) a s results =
  if length arg == 0
    then (a, s, [A (NilAtom "NIL")])
    else evalPrognExpression arg a s []
evalCallExpression (CallExpression (A (IdentAtom "list")) arg) a s results =
  let (oldArg, s', evaledGroup) = evalGroupExpression arg s []
   in if (length evaledGroup > 0) && expectSexpression (head evaledGroup) evalErrorAtom
        then ([], s, evaledGroup)
        else -- else ( a, s', [ (ListExpression evaledGroup)])         -- NOTE(JoanMontas) just lazily evaluate them... lets mess around

          if length evaledGroup == 0
            then (a, s, [ListExpression []])
            else -- else (a, s, [ (ListExpression arg)])       -- NOTE(JoanMontas) hmmmmm lets remove lazy eval until I research more
              (a, s, [(ListExpression evaledGroup)])
evalCallExpression (CallExpression (A (IdentAtom "nth")) arg) a s results =
  if (length arg /= 2)
    then ([], s, [A (EvalErrorAtom "eval's nth Error: Wrong parameter")])
    else
      let (a', s', r') = eval [head arg] s []
       in if (length r' == 0) || expectSexpression (head r') evalErrorAtom
            then ([], s, [A (EvalErrorAtom "eval's nth Error: Error evaluating nth's first argument")] ++ r')
            else
              if not (expectSexpression (head r') integerAtom)
                then ([], s, [A (EvalErrorAtom "eval's nth Error: nth's first argument expects an integer")])
                else
                  let (A (IntegerAtom v)) = head r'
                      (a'', s'', r'') = eval (tail arg) s' []
                   in case (head r'') of
                        -- NOTE(JoanMontas) here we can match other iterable... also eventually can create their own file (for other iterable data type)
                        A (EvalErrorAtom v) -> ([], s, r'' ++ [A (EvalErrorAtom "eval's nth Error: Error while evaluating iterable")])
                        (ListExpression v') ->
                          if (fromInteger v) >= length v' || (fromInteger v < 0)
                            then ([], s, [A (EvalErrorAtom "eval's nth Error: ListExpression out of range error")])
                            else
                              let (a''', s''', r''') = eval [v' !! (fromInteger v)] s'' []
                               in -- TODO(JoanMontas) after we eval the nth S-expression and place its evaluated
                                  -- value inside the list
                                  case (head r''') of
                                    A (EvalErrorAtom v) -> ([], s, r''' ++ [A (EvalErrorAtom "eval's nth Error: Error while evaluating nth element")])
                                    otherwise -> (a, s''', [head r'''])
                        otherwise -> ([], s, [A (EvalErrorAtom "eval's nth Error: nth's second item not an iterable")])
evalCallExpression (CallExpression (A (IdentAtom "append")) arg) a s results =
  if (length arg /= 2)
    then ([], s, [A (EvalErrorAtom "eval's nth Error: Wrong parameter")])
    else
      let (a', s', r') = eval [head arg] s []
       in case (head r') of
            A (EvalErrorAtom v) -> ([], s, r' ++ [A (EvalErrorAtom "eval's append Error: Error while evaluating iterable")])
            (ListExpression v) ->
              let (a'', s'', r'') = eval [arg !! 1] s' []
               in -- TODO(JoanMontas) see how much easier it would have been if I used "cons".
                  --                  pays to know lisp internal.
                  case (head r'') of
                    (ListExpression v') -> (a, s'', [ListExpression (v ++ v')])
                    otherwise -> (a, s'', [ListExpression (v ++ [head r''])])
            otherwise -> ([], s, [A (EvalErrorAtom "eval's append Error: append's second item not an iterable")])
evalCallExpression (CallExpression (A (IdentAtom "insertAtNth")) arg) a s results =
  if (length arg /= 3)
    then ([], s, [A (EvalErrorAtom "eval's insertAtNth Error: Wrong parameter")])
    else
      let (a', s', r') = eval [head arg] s []
       in if (length r' == 0) || expectSexpression (head r') evalErrorAtom
            then ([], s, r' ++ [A (EvalErrorAtom "eval's nth Error: Error evaluating insertAtNth's first argument")])
            else
              if not (expectSexpression (head r') integerAtom)
                then ([], s, [A (EvalErrorAtom "eval's insertAtNth Error: nth's first argument expects an integer")])
                else
                  let (A (IntegerAtom v)) = head r'
                      (a'', s'', r'') = eval [arg !! 1] s' []
                   in case (head r'') of
                        A (EvalErrorAtom v) -> ([], s, r'' ++ [A (EvalErrorAtom "eval's insertAtNth Error: Error while evaluating iterable")])
                        (ListExpression v') ->
                          if (fromInteger v) >= length v' || (fromInteger v < 0)
                            then ([], s, [A (EvalErrorAtom "eval's insertAtNth Error: ListExpression out of range error")])
                            else (a, s'', [ListExpression (evalInsertAtNth v' v (arg !! 2))])
                        otherwise -> ([], s, [A (EvalErrorAtom "eval's nth Error: nth's second item not an iterable")])
evalCallExpression (CallExpression (A (IdentAtom "removeNth")) arg) a s r =
  if (length arg /= 1) && (length arg /= 2)
    then ([], s, [A (EvalErrorAtom "eval's nth Error: Wrong parameter")])
    else
      let (a', s', r') = eval [head arg] s []
       in case (head r') of
            A (EvalErrorAtom v) -> ([], s, r' ++ [A (EvalErrorAtom "eval's removeNth Error: Error while evaluating iterable")])
            (ListExpression v) ->
              if length arg == 1
                then (a, s', [ListExpression (init v)])
                else ([], s, [A (EvalErrorAtom "eval's removeNth Error: Error iterable followed by an additional argument")])
            A (IntegerAtom v) ->
              if length arg /= 2
                then ([], s, [A (EvalErrorAtom "eval's removeNth Error: Integer Should be followed by an iterable")])
                else
                  let (a'', s'', r'') = eval (tail arg) s' []
                   in case (head r'') of
                        A (EvalErrorAtom v') -> ([], s, r ++ [A (EvalErrorAtom "eval's removeNth Error: Error while evaluating iterable")])
                        (ListExpression v') ->
                          if v < 0 || (fromInteger v) >= (length v')
                            then ([], s, r ++ [A (EvalErrorAtom "eval's removeNth Error: Out Of Range Error")])
                            else (a, s'', [ListExpression (evalRemoveNth (v') v)])
                        otherwise -> ([], s, [A (EvalErrorAtom "eval's removeNth Error: Unkown iterable")])
            otherwise -> ([], s, [A (EvalErrorAtom "eval's removeNth Error: Not known iterable nor Integer provided")])
evalCallExpression (CallExpression (A (IdentAtom "replaceAtNth")) arg) a s results =
  if (length arg /= 3)
    then ([], s, [A (EvalErrorAtom "eval's replaceAtNth Error: Wrong parameter")])
    else
      let (a', s', r') = eval [head arg] s []
       in if (length r' == 0) || expectSexpression (head r') evalErrorAtom
            then ([], s, r' ++ [A (EvalErrorAtom "eval's nth Error: Error evaluating replaceAtNth's first argument")])
            else
              if not (expectSexpression (head r') integerAtom)
                then ([], s, [A (EvalErrorAtom "eval's replaceAtNth Error: nth's first argument expects an integer")])
                else
                  let (A (IntegerAtom v)) = head r'
                      (a'', s'', r'') = eval [arg !! 1] s' []
                   in case (head r'') of
                        A (EvalErrorAtom v) -> ([], s, r'' ++ [A (EvalErrorAtom "eval's replaceAtNth Error: Error while evaluating iterable")])
                        (ListExpression v') ->
                          if (fromInteger v) >= length v' || (fromInteger v < 0)
                            then ([], s, [A (EvalErrorAtom "eval's replaceAtNth Error: ListExpression out of range error")])
                            else
                              let (a''', s''', r''') = eval [arg !! 2] s'' []
                               in case (head r''') of
                                    A (EvalErrorAtom v'') -> ([], s, r''' ++ [A (EvalErrorAtom "eval's replaceAtNth Error: Error while evaluating element to be inserted")])
                                    otherwise -> (a, s''', [ListExpression (evalReplaceAtNth v' v (arg !! 2))])
                        -- (a, s'', [ListExpression (evalReplaceAtNth v' v (arg !! 2))])   -- TODO(JoanMontas) eval the agument... this is too lazy lol
                        otherwise -> ([], s, [A (EvalErrorAtom "eval's nth Error: nth's second item not an iterable")])
evalCallExpression (CallExpression (A (IdentAtom nme)) arg) a s results = evalCustomCallExpression nme arg a s []

-- NOTE(JoanMontas) lol
-- revisited
evalGroupExpression :: [Sexpression] -> Scope -> [Sexpression] -> ([Sexpression], Scope, [Sexpression])
evalGroupExpression [] s r = ([], s, r)
evalGroupExpression (a : as) s r =
  let (a', s', r') = eval [a] s []
   in if (length r' == 0) || (expectSexpression (head r') evalErrorAtom)
        then ([], s, r' ++ [A (EvalErrorAtom "eval's evalGroupExpression Error: Error while evaluating Arguments")])
        else evalGroupExpression as s' (r ++ r')

evalSum :: Sexpression -> [Sexpression] -> Scope -> [Sexpression] -> [Sexpression] -> ([Sexpression], Scope, [Sexpression])
evalSum (A (IntegerAtom v)) [] s r a = ([], s, [A (IntegerAtom v)])
evalSum (A (IntegerAtom v)) (as : ass) s r a =
  let (a', s', r') = eval [as] s []
   in if (length r' == 0) || expectSexpression (head r') evalErrorAtom
        then ([], s, r' ++ [A (EvalErrorAtom "eval's + Error: Error evaluating argument")])
        else case (head r') of
          A (IntegerAtom v') -> evalSum (A (IntegerAtom (v + v'))) ass s' [] a
          otherwise -> (a, s, r' ++ [A (EvalErrorAtom "eval's evalSum Error: Given non-numerical value to evalSum '+'")])

evalSub :: [Sexpression] -> Scope -> [Sexpression] -> ([Sexpression], Scope, [Sexpression])
evalSub [] s r = ([], s, [A (EvalErrorAtom "eval's evalSub Error: Given zero value value to evalSub '-'")])
evalSub [a] s r =
  let (a', s', r') = eval [a] s []
   in if (length r' == 0) || expectSexpression (head r') evalErrorAtom
        then ([], s, r' ++ [A (EvalErrorAtom "eval's - Error: Error evaluating argument")])
        else case (head r') of
          A (IntegerAtom v') -> ([], s', [A (IntegerAtom ((-1) * v'))])
          otherwise -> ([], s, r' ++ [A (EvalErrorAtom "eval's evalSub Error: Given non-numerical value to evalSum '+'")])
evalSub (a : as) s r =
  let (a', s', r') = eval [a] s []
   in case (head r') of
        A (IntegerAtom v) ->
          let (a'', s'', r'') = evalSubHelper as s' []
           in case (head r'') of
                A (IntegerAtom v') -> ([], s'', [A (IntegerAtom (v + v'))])
                otherwise -> ([], s, r'')
        otherwise -> ([], s, r' ++ [A (EvalErrorAtom "eval's evalSub Error: Given non-numerical value to evalSum '+'")])

evalSubHelper :: [Sexpression] -> Scope -> [Sexpression] -> ([Sexpression], Scope, [Sexpression])
evalSubHelper [] s r = ([], s, [A (IntegerAtom 0)])
evalSubHelper [a] s r =
  let (a', s', r') = eval [a] s []
   in if (length r' == 0) || expectSexpression (head r') evalErrorAtom
        then ([], s, r' ++ [A (EvalErrorAtom "eval's - Error: Error evaluating argument")])
        else case (head r') of
          A (IntegerAtom v') -> ([], s', [A (IntegerAtom ((-1) * v'))])
          otherwise -> ([], s, r' ++ [A (EvalErrorAtom "eval's evalSub Error: Given non-numerical value to evalSum '+'")])
evalSubHelper (a : as) s r =
  let (a', s', r') = eval [a] s []
   in if (length r' == 0) || expectSexpression (head r') evalErrorAtom
        then ([], s, r' ++ [A (EvalErrorAtom "eval's - Error: Error evaluating argument")])
        else case (head r') of
          A (IntegerAtom v) ->
            let (a'', s'', r'') = evalSubHelper as s' []
             in if (length r'' == 0) || expectSexpression (head r'') evalErrorAtom
                  then ([], s, r'')
                  else case (head r'') of
                    A (IntegerAtom v') -> ([], s', [A (IntegerAtom (((-1) * (v)) + (v')))])
                    otherwise -> ([], s, r' ++ [A (EvalErrorAtom "eval's evalSubHelper Error: Given non-numerical value to evalSum '+'")])
          otherwise -> ([], s, r' ++ [A (EvalErrorAtom "eval's evalSubHelper Error: Given non-numerical value to evalSum '+'")])

evalMult :: Sexpression -> [Sexpression] -> Scope -> [Sexpression] -> [Sexpression] -> ([Sexpression], Scope, [Sexpression])
evalMult (A (IntegerAtom v)) [] s r a = ([], s, [A (IntegerAtom v)])
evalMult (A (IntegerAtom v)) (as : ass) s r a =
  let (a', s', r') = eval [as] s []
   in if (length r' == 0) || expectSexpression (head r') evalErrorAtom
        then ([], s, r' ++ [A (EvalErrorAtom "eval's * Error: Error evaluating argument")])
        else case (head r') of
          A (IntegerAtom v') -> evalMult (A (IntegerAtom (v * v'))) ass s' [] a
          otherwise -> (a, s, r' ++ [A (EvalErrorAtom "eval's evalMult Error: Given non-numerical value to evalMult '*'")])

-- note eventually having evalLesser type check and execute another helper function based on type
evalLesser :: [Sexpression] -> Sexpression
evalLesser [sp] = A (TrueAtom "T")
evalLesser ((A (IntegerAtom v)) : ys) = evalLesserHelper (A (IntegerAtom v)) ys
evalLesser otherwise = A (EvalErrorAtom "eval's evaLesser Error: Given non-numerical value to evalLesser '<'")

evalLesserHelper :: Sexpression -> [Sexpression] -> Sexpression
evalLesserHelper (A (IntegerAtom v)) [] = A (TrueAtom "T")
evalLesserHelper (A (IntegerAtom v)) ((A (IntegerAtom v')) : ys) =
  if v < v'
    then evalLesserHelper (A (IntegerAtom v)) ys
    else A (NilAtom "NIL")
evalLesserHelper _ _ = A (EvalErrorAtom "eval's evaLesser Error: Given non-numerical value to evalLesser '<'")

evalLesserEqual :: [Sexpression] -> Sexpression
evalLesserEqual [sp] = A (TrueAtom "T")
evalLesserEqual ((A (IntegerAtom v)) : ys) = evalLesserEqualHelper (A (IntegerAtom v)) ys
evalLesserEqual otherwise = A (EvalErrorAtom "eval's evaLesserEqual Error: Given non-numerical value to evalLesserEqual '<='")

evalLesserEqualHelper :: Sexpression -> [Sexpression] -> Sexpression
evalLesserEqualHelper (A (IntegerAtom v)) [] = A (TrueAtom "T")
evalLesserEqualHelper (A (IntegerAtom v)) ((A (IntegerAtom v')) : ys) =
  if v <= v'
    then evalLesserEqualHelper (A (IntegerAtom v)) ys
    else A (NilAtom "NIL")
evalLesserEqualHelper _ _ = A (EvalErrorAtom "eval's evaLesserEqual Error: Given non-numerical value to evalLesserEqual '<='")

evalGreater :: [Sexpression] -> Sexpression
evalGreater [sp] = A (TrueAtom "T")
evalGreater ((A (IntegerAtom v)) : ys) = evalGreaterHelper (A (IntegerAtom v)) ys
evalGreater otherwise = A (EvalErrorAtom "eval's evalGreater Error: Given non-numerical value to evalGreater '>'")

evalGreaterHelper :: Sexpression -> [Sexpression] -> Sexpression
evalGreaterHelper (A (IntegerAtom v)) [] = A (TrueAtom "T")
evalGreaterHelper (A (IntegerAtom v)) ((A (IntegerAtom v')) : ys) =
  if v > v'
    then evalGreaterHelper (A (IntegerAtom v)) ys
    else A (NilAtom "NIL")
evalGreaterHelper _ _ = A (EvalErrorAtom "eval's evalGreaterHelper Error: Given non-numerical value to evalGreater '>'")

evalGreaterEqual :: [Sexpression] -> Sexpression
evalGreaterEqual [sp] = A (TrueAtom "T")
evalGreaterEqual ((A (IntegerAtom v)) : ys) = evalGreaterEqualHelper (A (IntegerAtom v)) ys
evalGreaterEqual otherwise = A (EvalErrorAtom "eval's evalGreaterEqual Error: Given non-numerical value to evalGreaterEqual '>='")

evalGreaterEqualHelper :: Sexpression -> [Sexpression] -> Sexpression
evalGreaterEqualHelper (A (IntegerAtom v)) [] = A (TrueAtom "T")
evalGreaterEqualHelper (A (IntegerAtom v)) ((A (IntegerAtom v')) : ys) =
  if v >= v'
    then evalGreaterEqualHelper (A (IntegerAtom v)) ys
    else A (NilAtom "NIL")
evalGreaterEqualHelper _ _ = A (EvalErrorAtom "eval's evalGreaterEqualHelper Error: Given non-numerical value to evalGreaterEqual '>'")

evalIfExpression :: Sexpression -> [Sexpression] -> Scope -> [Sexpression] -> ([Sexpression], Scope, [Sexpression])
evalIfExpression (IfExpression c b e) a s r =
  let (a', s', r') = eval [c] s []
   in if (length r' > 0) && expectSexpression (head r') evalErrorAtom
        then ([], s, r' ++ [A (EvalErrorAtom "eval's if Error: Error Evaluating Condition")])
        else
          if isTruthy (r')
            then eval [b] s' r
            else eval [e] s' r

evalPrognExpression :: [Sexpression] -> [Sexpression] -> Scope -> [Sexpression] -> ([Sexpression], Scope, [Sexpression])
evalPrognExpression [] as s r = (as, s, [A (NilAtom "NIL")])
evalPrognExpression [a] as s r =
  let (a', s', r') = eval [a] s []
   in if (length r' == 0) || expectSexpression (head r') evalErrorAtom
        then ([], s, r' ++ [A (EvalErrorAtom "eval's progn Error: Error evaluating bodies 1")])
        else (as, s', r')
evalPrognExpression (a : ass) as s r =
  let (a', s', r') = eval [a] s []
   in if (length r' == 0) || expectSexpression (head r') evalErrorAtom
        then ([], s, r' ++ [A (EvalErrorAtom "eval's progn Error: Error evaluating bodies 2")])
        else evalPrognExpression ass as s' []

evalDefunExpression :: Sexpression -> [Sexpression] -> Scope -> [Sexpression] -> ([Sexpression], Scope, [Sexpression])
evalDefunExpression (FunctionExpression (A (IdentAtom n)) arg bdy) a s r =
  let newScope = insertSexpToScope s n (FunctionExpression (A (IdentAtom n)) arg bdy)
   in (a, newScope, [(A (StringAtom n))])

evalCustomCallExpression :: String -> [Sexpression] -> [Sexpression] -> Scope -> [Sexpression] -> ([Sexpression], Scope, [Sexpression])
evalCustomCallExpression nme arg a s r =
  let func = findSexp s nme
   in case func of
        (FunctionExpression nme' arg' bdy) -> evalCustomCallExpressionHelper func arg a s []
        A (EvalErrorAtom v) -> ([], s, [A (EvalErrorAtom v)])
        otherwise -> ([], s, [A (EvalErrorAtom "eval's evalCustomCallExpression Error: Given Ident Is Not A Function")])

evalCustomCallExpressionHelper :: Sexpression -> [Sexpression] -> [Sexpression] -> Scope -> [Sexpression] -> ([Sexpression], Scope, [Sexpression])
evalCustomCallExpressionHelper (FunctionExpression nme' arg' bdy) arg a s r =
  if (length arg') /= (length arg)
    then ([], s, [A (EvalErrorAtom "eval's evalCustomCallExpressionHelper Error: Given Wrong Amount Of Argument")])
    else
      let (a', s', evaledGroup) = evalGroupExpression arg s []
       in if (length evaledGroup == 0) || expectSexpression (head evaledGroup) evalErrorAtom
            then ([], s, evaledGroup ++ [A (EvalErrorAtom "eval's evalCustomCallExpressionHelper Error: Error While Evaluating Arguments")])
            else
              let funcScope = makeLocalScope s'
                  funcScope' = applyArgsToScope arg' evaledGroup funcScope
               in let (a'', funcScope'', r') = eval [bdy] funcScope' []
                   in if (length r' > 0) && expectSexpression (head r') evalErrorAtom
                        then ([], s, r')
                        else (a, (getPrev funcScope''), r')

-- define whats true whats false
isTruthy :: [Sexpression] -> Bool
isTruthy [] = False
isTruthy [A (NilAtom "NIL")] = False
isTruthy ((A (NilAtom "NIL")) : _) = False
isTruthy otherwise = True

applyArgsToScope :: [Sexpression] -> [Sexpression] -> Scope -> Scope
applyArgsToScope [] [] s = s
applyArgsToScope [A (IdentAtom nme)] [val] s = insertSexpToScope s nme val
applyArgsToScope ((A (IdentAtom nme)) : args) (val : vals) s =
  let s' = insertSexpToScope s nme val
   in applyArgsToScope args vals s'
applyArgsToScope _ _ s = s

evalLogicalAnd :: [Sexpression] -> Scope -> [Sexpression] -> ([Sexpression], Scope, [Sexpression])
evalLogicalAnd [] s r = ([], s, [A (TrueAtom "T")])
evalLogicalAnd [a] s r =
  let (a', s', r') = eval [a] s []
   in if (length r') == 0 || expectSexpression (head r') evalErrorAtom
        then ([], s, r' ++ [A (EvalErrorAtom "eval's evalLogicalAnd Error: Error while evaluating arguments")])
        else
          if isTruthy r'
            then ([], s', r')
            else ([], s', [A (NilAtom "NIL")])
evalLogicalAnd (a : as) s r =
  let (a', s', r') = eval [a] s []
   in if (length r') == 0 || expectSexpression (head r') evalErrorAtom
        then ([], s, r' ++ [A (EvalErrorAtom "eval's evalLogicalAnd Error: Error while evaluating arguments")])
        else
          if isTruthy r'
            then evalLogicalAnd as s' []
            else ([], s', [A (NilAtom "NIL")])

evalLogicalOr :: [Sexpression] -> Scope -> [Sexpression] -> ([Sexpression], Scope, [Sexpression])
evalLogicalOr [] s r = ([], s, [A (NilAtom "NIL")])
evalLogicalOr (a : as) s r =
  let (a', s', r') = eval [a] s []
   in if (length r') == 0 || expectSexpression (head r') evalErrorAtom
        then ([], s, r' ++ [A (EvalErrorAtom "eval's evalLogicalOr Error: Error while evaluating arguments")])
        else
          if isTruthy r'
            then ([], s', r')
            else evalLogicalOr as s' r'

evalRemoveNth :: [a] -> Integer -> [a]
evalRemoveNth [] _ = []
evalRemoveNth (a : as) 0 = as
evalRemoveNth (a : as) i = [a] ++ evalRemoveNth as (i - 1)

evalInsertAtNth :: [a] -> Integer -> a -> [a]
evalInsertAtNth [] _ e = [e]
evalInsertAtNth (a : as) 0 e = [e, a] ++ as
evalInsertAtNth (a : as) i e = [a] ++ evalInsertAtNth as (i - 1) e

evalReplaceAtNth :: [a] -> Integer -> a -> [a]
evalReplaceAtNth [] i e = [e]
evalReplaceAtNth (a : as) 0 e = [e] ++ as
evalReplaceAtNth (a : as) i e = [a] ++ evalReplaceAtNth as (i - 1) e
