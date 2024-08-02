-- test/EvaluaterText.hs

-- Copyright Joan Montas
-- All rights reserved.
-- License under GNU General Public License v3.0

import Test.Hspec
import Lexer
import Parser
import Evaluater

main :: IO ()
main = hspec $ do
    describe "Eval Single Atoms Integer" $ do
        it "should Eval '1' into IntegerAtom 1" $ do
            let input            = "1"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 1)]
            r `shouldBe` expectedOutput

        it "should Eval '-1' into IntegerAtom (-1)" $ do
            let input            = "-1"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom (-1))]
            r `shouldBe` expectedOutput
        
        it "should Eval '567345456' into IntegerAtom 567345456" $ do
            let input            = "567345456"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 567345456)]
            r `shouldBe` expectedOutput
      
        it "should Eval '-567345456' into IntegerAtom (-567345456)" $ do
            let input            = "-567345456"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom (-567345456))]
            r `shouldBe` expectedOutput

    describe "Eval Single Atoms String" $ do
        it "should Eval '\"h\"' into StringAtom \"h\"" $ do
            let input            = "\"h\""
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (StringAtom "h")]
            r `shouldBe` expectedOutput

        it "should Eval '\"ffgh345zddf-345gxfggb\"' into StringAtom \"ffgh345zddf-345gxfggb\"" $ do
            let input            = "\"ffgh345zddf-345gxfggb\""
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (StringAtom "ffgh345zddf-345gxfggb")]
            r `shouldBe` expectedOutput

    describe "Eval Single Atoms NIL/()" $ do
        it "should Eval '()' into NilAtom 'NIL'" $ do
            let input            = "()"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (NilAtom "NIL")]
            r `shouldBe` expectedOutput

        it "should Eval 'NIL' into NilAtom 'NIL')" $ do
            let input            = "NIL"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (NilAtom "NIL")]
            r `shouldBe` expectedOutput

    describe "Eval Single Atoms T" $ do
        it "should Eval 'T' into TrueAtom 'T'" $ do
            let input            = "T"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (TrueAtom "T")]
            r `shouldBe` expectedOutput

    describe "Eval Mulltiple Single Atoms" $ do
        -- like inside a repl
        it "should Eval '1 2 3' into IntegerAtom '1', IntegerAtom '2' and IntegerAtom '3'" $ do
            let input                = "1 2 3"
            let tokens               = tokenize input       []
            let (tokens', ast)       = parse    tokens      []
            let scpe                 = makeGlobalScope
            let (e, s, r)            = eval ast scpe        []
            let expectedOutput       = [A (IntegerAtom 1)]
            r `shouldBe` expectedOutput
            let (tokens'', ast')     = parse    tokens'     []
            let (e', s', r')         = eval ast' s          []
            let expectedOutput'      = [A (IntegerAtom 2)]
            r' `shouldBe` expectedOutput'
            let (tokens''', ast'')   = parse    tokens''    []
            let (e'', s'', r'')      = eval ast'' s'        []
            let expectedOutput''     = [A (IntegerAtom 3)]
            r'' `shouldBe` expectedOutput''
    
    describe "Eval Arithmetic +" $ do
        -- edge case no input
        it "should Eval '(+)' into IntegerAtom 0" $ do
            let input            = "(+)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 0)]
            r `shouldBe` expectedOutput

        -- edge case single number (small)
        it "should Eval '(+ 4)' into IntegerAtom 4" $ do
            let input            = "(+ 4)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 4)]
            r `shouldBe` expectedOutput

        -- ege case single number (many digits)
        it "should Eval '(+ 456456)' into IntegerAtom 456456" $ do
            let input            = "(+ 456456)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 456456)]
            r `shouldBe` expectedOutput

        -- edge case single negative number (small)
        it "should Eval '(+ -7)' into IntegerAtom (-7)" $ do
            let input            = "(+ -7)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom (-7))]
            r `shouldBe` expectedOutput

        -- edge case single negative number (many digits)
        it "should Eval '(+ -62355423445)' into IntegerAtom (-62355423445)" $ do
            let input            = "(+ -62355423445)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom (-62355423445))]
            r `shouldBe` expectedOutput

        it "should Eval '(+ 7 6)' into IntegerAtom 13" $ do
            let input            = "(+ 7 6)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 13)]
            r `shouldBe` expectedOutput

        it "should Eval '(+ 7 6 4)' into IntegerAtom 17" $ do
            let input            = "(+ 7 6 4)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 17 )]
            r `shouldBe` expectedOutput
            
        it "should Eval '(+ 3876345345 93459863345692834)' into IntegerAtom 93459867222038179" $ do
            let input            = "(+ 3876345345 93459863345692834)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 93459867222038179)]
            r `shouldBe` expectedOutput

        it "should Eval '(+ 445677456457 4566456 345823463535)' into IntegerAtom 17" $ do
            let input            = "(+ 445677456457 4566456 345823463535)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 791505486448 )]
            r `shouldBe` expectedOutput

    describe "Eval Arithmetic -" $ do
        -- edge case no input
        it "should Eval '(-)' into IntegerAtom 0" $ do
            let input            = "(-)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (EvalErrorAtom "eval's evalSub Error: Given zero value value to evalSub '-'")]
            r `shouldBe` expectedOutput

        -- edge case single number (small)
        it "should Eval '(- 4)' into IntegerAtom (-4)" $ do
            let input            = "(- 4)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom (-4))]
            r `shouldBe` expectedOutput

        -- ege case single number (many digits)
        it "should Eval '(- 456456)' into IntegerAtom (-456456)" $ do
            let input            = "(- 456456)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom (-456456))]
            r `shouldBe` expectedOutput

        -- edge case single negative number (small)
        it "should Eval '(- -7)' into IntegerAtom 7)" $ do
            let input            = "(- -7)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 7)]
            r `shouldBe` expectedOutput

        -- edge case single negative number (many digits)
        it "should Eval '(- -62355423445)' into IntegerAtom 62355423445" $ do
            let input            = "(- -62355423445)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 62355423445)]
            r `shouldBe` expectedOutput

        it "should Eval '(- 7 6)' into IntegerAtom 1" $ do
            let input            = "(- 7 6)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 1)]
            r `shouldBe` expectedOutput

        it "should Eval '(- 7 6 5)' into IntegerAtom (-4)" $ do
            let input            = "(- 7 6 5)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom (-4) )]
            r `shouldBe` expectedOutput
            
        it "should Eval '(- 3876345345 93459863345692834)' into IntegerAtom (- 93459859469347489)" $ do
            let input            = "(- 3876345345 93459863345692834)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom (-93459859469347489))]
            r `shouldBe` expectedOutput

        it "should Eval '(- 93459863345692834 3876345345 )' into IntegerAtom 93459859469347489" $ do
            let input            = "(- 93459863345692834 3876345345)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 93459859469347489)]
            r `shouldBe` expectedOutput

        it "should Eval '(- 445677456457 4566456 345823463535)' into IntegerAtom 99849426466" $ do
            let input            = "(- 445677456457 4566456 345823463535)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 99849426466 )]
            r `shouldBe` expectedOutput

    describe "Eval Arithmetic *" $ do
        -- edge case no input
        it "should Eval '(*)' into IntegerAtom 1" $ do
            let input            = "(*)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 1)]
            r `shouldBe` expectedOutput

        -- edge case single number (small)
        it "should Eval '(* 4)' into IntegerAtom 4" $ do
            let input            = "(* 4)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 4)]
            r `shouldBe` expectedOutput

        -- ege case single number (many digits)
        it "should Eval '(* 456456)' into IntegerAtom 456456" $ do
            let input            = "(* 456456)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 456456)]
            r `shouldBe` expectedOutput

        -- edge case single negative number (small)
        it "should Eval '(* -7)' into IntegerAtom (-7)" $ do
            let input            = "(* -7)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom (-7))]
            r `shouldBe` expectedOutput

        -- edge case single negative number (many digits)
        it "should Eval '(* -62355423445)' into IntegerAtom (-62355423445)" $ do
            let input            = "(* -62355423445)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom (-62355423445))]
            r `shouldBe` expectedOutput

        it "should Eval '(* 7 6)' into IntegerAtom 42" $ do
            let input            = "(* 7 6)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 42)]
            r `shouldBe` expectedOutput

        it "should Eval '(* -7 6)' into IntegerAtom (-42)" $ do
            let input            = "(* -7 6)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom (-42))]
            r `shouldBe` expectedOutput
        
        it "should Eval '(* -7 6)' into IntegerAtom (-42)" $ do
            let input            = "(* -7 6)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom (-42))]
            r `shouldBe` expectedOutput

        it "should Eval '(* 7 6 5)' into IntegerAtom 210" $ do
            let input            = "(* 7 6 5)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 210 )]
            r `shouldBe` expectedOutput
            
        it "should Eval '(* 3876345345 93459863345692834)' into IntegerAtom 362282706224412542875757730" $ do
            let input            = "(* 3876345345 93459863345692834)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 362282706224412542875757730)]
            r `shouldBe` expectedOutput

        it "should Eval '(* 445677456457 -4566456 345823463535)' into IntegerAtom (-703808326206839122379976915720)" $ do
            let input            = "(* 445677456457 -4566456 345823463535)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom (-703808326206839122379976915720) )]
            r `shouldBe` expectedOutput

    describe "Eval Arithmetic >" $ do
        it "should Eval '(>)' into  EvalErrorAtom 'eval's evalGreater Error: Given non-numerical value to evalGreater '>'" $ do
            let input            = "(>)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (EvalErrorAtom "eval's evalGreater Error: Given non-numerical value to evalGreater '>'")]
            r `shouldBe` expectedOutput

        it "should Eval '(> 4576345)' into  TrueAtom 'T'" $ do
            let input            = "(> 4576345)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (TrueAtom "T")]
            r `shouldBe` expectedOutput

        it "should Eval '(> -934875749)' into  TrueAtom 'T'" $ do
            let input            = "(> -934875749)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (TrueAtom "T")]
            r `shouldBe` expectedOutput

        it "should Eval '(> 4576345)' into  TrueAtom 'T'" $ do
            let input            = "(> 4576345)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (TrueAtom "T")]
            r `shouldBe` expectedOutput
        
        it "should Eval '(> 9 1)' into  TrueAtom 'T'" $ do
            let input            = "(> 9 1)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (TrueAtom "T")]
            r `shouldBe` expectedOutput
        
        it "should Eval '(> 3 7)' into  NilAtom 'NIL'" $ do
            let input            = "(> 3 7)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (NilAtom "NIL")]
            r `shouldBe` expectedOutput

        it "should Eval '(> 9 7 5 6 1)' into  TrueAtom 'T'" $ do
            let input            = "(> 9 7 5 6 1)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (TrueAtom "T")]
            r `shouldBe` expectedOutput
        
        it "should Eval '(> 3 1 7 0)' into  NilAtom 'NIL'" $ do
            let input            = "(> 3 1 7 0)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (NilAtom "NIL")]
            r `shouldBe` expectedOutput

    describe "Eval logical and" $ do
        it "should Eval '(and T)' into TrueAtom 'T'" $ do
            let input            = "(and T)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (TrueAtom "T")]
            r `shouldBe` expectedOutput
        it "should Eval '(and 345)' into TrueAtom 'T'" $ do
            let input            = "(and 345)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 345)]
            r `shouldBe` expectedOutput
        it "should Eval '(and 'fgDfg')' into StringAtom 'fgDfg'" $ do
            let input            = "(and \"fgDfg\")"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (StringAtom "fgDfg")]
            r `shouldBe` expectedOutput
        it "should Eval '(and (> 123 9) 'blakghgs')' into StringAtom 'T'" $ do
            let input            = "(and (> 123 9) \"blakghgs\")"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (StringAtom "blakghgs")]
            r `shouldBe` expectedOutput
        it "should Eval '(and 234 (> 123 9))' into TrueAtom 'T'" $ do
            let input            = "(and 234 (> 123 9))"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (TrueAtom "T")]
            r `shouldBe` expectedOutput
        it "should Eval '(and 'lbjhfgd' (>= (* 99 99) 1) (- 234 43) )' into TrueAtom 'T'" $ do
            let input            = "(and \"lbjhfgd\" (>= (* 99 99) 1) (- 234 43))"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 191)]
            r `shouldBe` expectedOutput
        it "should Eval '(and NIL)' into NilAtom 'NIL'" $ do
            let input            = "(and NIL)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (NilAtom "NIL")]
            r `shouldBe` expectedOutput
        it "should Eval '(and (> -345 99))' into NilAtom 'NIL'" $ do
            let input            = "(and (> -345 99))"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (NilAtom "NIL")]
            r `shouldBe` expectedOutput
        it "should Eval '(and (> -345 99) (<= 11 -10) )' into NilAtom 'NIL'" $ do
            let input            = "(and (> -345 99) (<= 11 -10) )"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (NilAtom "NIL")]
            r `shouldBe` expectedOutput
        it "should Eval '(and (> -345 99) (<= 11 -10) (> 86739 86739) )' into NilAtom 'NIL'" $ do
            let input            = "(and (> -345 99) (<= 11 -10) (> 86739 86739) )"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (NilAtom "NIL")]
            r `shouldBe` expectedOutput
        it "should Eval '(and (< -345 99) (>= 11 -10) (> 86739 86739) )' into NilAtom 'NIL'" $ do
            let input            = "(and (> -345 99) (<= 11 -10) (> 86739 86739) )"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (NilAtom "NIL")]
            r `shouldBe` expectedOutput
    describe "Eval logical or" $ do
        it "should Eval '(or)' into NilAtom 'NIL'" $ do
            let input            = "(or)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (NilAtom "NIL")]
            r `shouldBe` expectedOutput
        it "should Eval '(or T)' into TrueAtom 'T'" $ do
            let input            = "(or T)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (TrueAtom "T")]
            r `shouldBe` expectedOutput
        it "should Eval '(or NIL)' into NilAtom 'NIL'" $ do
            let input            = "(or NIL)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (NilAtom "NIL")]
            r `shouldBe` expectedOutput
        it "should Eval '(or NIL NIL)' into NilAtom 'NIL'" $ do
            let input            = "(or NIL NIL)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (NilAtom "NIL")]
            r `shouldBe` expectedOutput
        it "should Eval '(or NIL T)' into NilAtom 'NIL'" $ do
            let input            = "(or NIL T)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (TrueAtom "T")]
            r `shouldBe` expectedOutput
        it "should Eval '(or NIL T NIL)' into NilAtom 'NIL'" $ do
            let input            = "(or NIL T NIL)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (TrueAtom "T")]
            r `shouldBe` expectedOutput
    describe "Eval IfExpression" $ do
        it "should Eval '(if 1 2)' into IntegerAtom 2" $ do
            let input            = "(if 1 2)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 2)]
            r `shouldBe` expectedOutput
        
        it "should Eval '(if () 2)' into NilAtom 'NIL'" $ do
            let input            = "(if () 2)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (NilAtom "NIL")]
            r `shouldBe` expectedOutput

        it "should Eval '(if 1 2 3)' into IntegerAtom 2" $ do
            let input            = "(if 1 2 3)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 2)]
            r `shouldBe` expectedOutput

        it "should Eval '(if () 2 3)' into NilAtom 'NIL'" $ do
            let input            = "(if 1 () 3)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (NilAtom "NIL")]
            r `shouldBe` expectedOutput

        it "should Eval '(if () 2 3)' into IntegerAtom 3" $ do
            let input            = "(if () 2 3)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 3)]
            r `shouldBe` expectedOutput

        it "should Eval '(if (+ 23 34) 2)' into IntegerAtom 2" $ do
            let input            = "(if (+ 23 34) 2)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 2)]
            r `shouldBe` expectedOutput

        it "should Eval '(if (> 9 1) 2)' into IntegerAtom 2" $ do
            let input            = "(if (> 9 1) 2)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 2)]
            r `shouldBe` expectedOutput

        it "should Eval '(if (> 1 9) 2)' into NilAtom 'NIL" $ do
            let input            = "(if (> 1 9) 2)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (NilAtom "NIL")]
            r `shouldBe` expectedOutput
        
        it "should Eval '(if (> 1 9) 2 3)' into IntegerAtom 3" $ do
            let input            = "(if (> 1 9) 2 3)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 3)]
            r `shouldBe` expectedOutput

        it "should Eval '(if (> 1 9) 2 (+ -56 36))' into IntegerAtom (-20)" $ do
            let input            = "(if (> 1 9) 2 (+ -56 36))"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom (-20))]
            r `shouldBe` expectedOutput

        it "should Eval '(if (> 9 1 0) (- 100 1) (+ -56 36))' into IntegerAtom 99" $ do
            let input            = "(if (> 9 1 0) (- 100 1) (+ -56 36))"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 99)]
            r `shouldBe` expectedOutput


    describe "Eval Single Basic Ident via defvar" $ do
        it "should Eval '(defvar x)' into StringAtom 'x'" $ do
            let input            = "(defvar x)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (StringAtom "x")]
            r `shouldBe` expectedOutput

        it "should Eval '(defvar sfgfd123 98345)' into StringAtom 'x'" $ do
            let input            = "(defvar sfgfd123 98345)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (StringAtom "sfgfd123")]
            r `shouldBe` expectedOutput

        it "should Eval '(defvar p9487541 \"UMLisped.\")' into StringAtom 'x'" $ do
            let input            = "(defvar p9487541 \"UMLisped.\")"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (StringAtom "p9487541")]
            r `shouldBe` expectedOutput

    describe "Eval Ident that has been 'defvar'" $ do
        it "should Eval '(defvar a 8) x' into  IntegerAtom 8 and EvalErrorAtom 'Variable not found in any scope'" $ do
            let input            = "(defvar a 8) a b"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (StringAtom "a")]
            r `shouldBe` expectedOutput
            let (tokens'', ast')     = parse    tokens'     []
            let (e', s', r')         = eval ast' s          []
            let expectedOutput'      = [A (IntegerAtom 8)]
            r' `shouldBe` expectedOutput'
            let (tokens''', ast'')     = parse    tokens''     []
            let (e'', s'', r'')         = eval ast'' s          []
            let expectedOutput''      = [A (EvalErrorAtom "EvalErrorAtom ERROR: Variable not found in any scope")]
            r'' `shouldBe` expectedOutput''

    describe "Eval Single Basic via setq" $ do
        it "should Eval '(defvar a ) (setq a 123) a' into  'IntegerAtom 123'" $ do
            let input            = "(defvar a) (setq a 123) a"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (StringAtom "a")]
            r `shouldBe` expectedOutput
            let (tokens'', ast')     = parse    tokens'     []
            let (e', s', r')         = eval ast' s          []
            let expectedOutput'      = [A (IntegerAtom 123)]
            r' `shouldBe` expectedOutput'
            let (tokens''', ast'')     = parse    tokens''     []
            let (e'', s'', r'')         = eval ast'' s'          []
            let expectedOutput''      = [A (IntegerAtom 123)]
            r'' `shouldBe` expectedOutput''

    describe "Eval setq in Non-valid Ident" $ do
        it "should Eval '(defvar a ) (setq x 123)' into  'EvalErrorAtom ERROR: Variable not found in any scope'" $ do
            let input            = "(defvar a) (setq x 123)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (StringAtom "a")]
            r `shouldBe` expectedOutput
            let (tokens'', ast')     = parse    tokens'     []
            let (e', s', r')         = eval ast' s          []
            let expectedOutput'      = [A (EvalErrorAtom "EvalErrorAtom ERROR: Variable not found in any scope")]
            r' `shouldBe` expectedOutput'
   

    describe "Eval progn as block statement" $ do
        it "should Eval '(progn (defvar x) (setq x 1) (setq x (+ 1 x)) (setq x (+ x 3)) (setq x (* 3 x)))' into  'EvalErrorAtom ERROR: Variable not found in any scope'" $ do
            let input            = "(progn (defvar x) (setq x 1) (setq x (+ 1 x)) (setq x (+ x 3)) (setq x (* 3 x))) x"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (IntegerAtom 15)]
            r `shouldBe` expectedOutput
            let (tokens'', ast')     = parse    tokens'     []
            let (e', s', r')         = eval ast' s          []
            let expectedOutput'      = [A (IntegerAtom 15)]
            r' `shouldBe` expectedOutput'

    describe "Eval defun" $ do
        it "should Eval '(defun foo(a) a) (foo\"Hello, World!\")' into  StringAtom 'Hello, World!' " $ do
            let input            = "(defun foo(a) a) (foo \"Hello, World!\") (foo 135454))"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (StringAtom "foo")]
            r `shouldBe` expectedOutput
            let (tokens'', ast')     = parse    tokens'     []
            let (e', s', r')         = eval ast' s          []
            let expectedOutput'      = [A (StringAtom "Hello, World!")]
            r' `shouldBe` expectedOutput'
            let (tokens''', ast'')     = parse    tokens''     []
            let (e'', s'', r'')         = eval ast'' s'          []
            let expectedOutput''      = [A (IntegerAtom 135454)]
            r'' `shouldBe` expectedOutput''

        it "should Eval a recursive function" $ do
            let input            = "( defun foo(a) ( if (<= a 10) ( foo (+ 1 a) ) a ) ) (foo -56)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (StringAtom "foo")]
            r `shouldBe` expectedOutput
            let (tokens'', ast')     = parse    tokens'     []
            let (e', s', r')         = eval ast' s          []
            let expectedOutput'      = [A (IntegerAtom 11)]
            r' `shouldBe` expectedOutput'

        it "should Eval first class func y which comes from foo " $ do
            let input            = "(defun foo(x) (* 3 x)) (defvar y foo) (foo 12)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (StringAtom "foo")]
            r `shouldBe` expectedOutput
            let (tokens'', ast')     = parse    tokens'     []
            let (e', s', r')         = eval ast' s          []
            let expectedOutput'      = [A (StringAtom "y")]
            r' `shouldBe` expectedOutput'
            let (tokens''', ast'')     = parse    tokens''     []
            let (e'', s'', r'')         = eval ast'' s'          []
            let expectedOutput''      = [A (IntegerAtom (12 * 3))]
            r'' `shouldBe` expectedOutput''

        it "should closure?" $ do
            let input            = "( defun foo(a) ( progn  \"hi\" (defvar x 54345) 3)) (foo \"hmmm\") x"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A (StringAtom "foo")]
            r `shouldBe` expectedOutput
            let (tokens'', ast')     = parse    tokens'     []
            let (e', s', r')         = eval ast' s          []
            let expectedOutput'      = [A (IntegerAtom 3)]
            r' `shouldBe` expectedOutput'
            let (tokens''', ast'')     = parse    tokens''     []
            let (e'', s'', r'')         = eval ast'' s'          []
            let expectedOutput''      = [A (IntegerAtom 54345)]
            r'' `shouldBe` expectedOutput''

    describe "Eval list" $ do
        it "should Eval '(list) into ListExpression []" $ do
            let input            = "(list)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [(ListExpression [])]
            r `shouldBe` expectedOutput
        it "should Eval '(list 1) into ListExpression [IntegerAtom 1]" $ do
            let input            = "(list 1)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [(ListExpression [A(IntegerAtom 1)])]
            r `shouldBe` expectedOutput
        it "should Eval '(list 1) into ListExpression [IntegerAtom 1]" $ do
            let input            = "(list 1)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [(ListExpression [A(IntegerAtom 1)])]
            r `shouldBe` expectedOutput
        it "should Eval '(list 'hello-world!...') into ListExpression [StringAtom 'hello-world!...']" $ do
            let input            = "(list \"hello-world!...\")"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [(ListExpression [A(StringAtom "hello-world!...")])]
            r `shouldBe` expectedOutput
        it "should Eval '(list NIL) into ListExpression [NilAtom 'NIL']" $ do
            let input            = "(list NIL)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [(ListExpression [A(NilAtom "NIL")])]
            r `shouldBe` expectedOutput
        it "should Eval '(list (* 34 -6575)) into ListExpression [IntegerAtom -223550]" $ do
            let input            = "(list (* 34 -6575))"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [(ListExpression [A(IntegerAtom (-223550))])]
            r `shouldBe` expectedOutput
        it "should Eval '(list (< -4563485 9945)) into ListExpression [TrueAtom T]" $ do
            let input            = "(list (< -4563485 9945))"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [(ListExpression [A(TrueAtom "T")])]
            r `shouldBe` expectedOutput
        it "should Eval '(list 'h3r3 1s s0m3t41ng' 45 (>= -592745 94863)) into ListExpression [StringAtom 'h3r3 1s s0m3t41ng', IntegerAtom 45, NilAtom 'NIL']" $ do
            let input            = "(list \"h3r3 1s s0m3t41ng\" 45 (>= -592745 94863))"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [(ListExpression [A(StringAtom "h3r3 1s s0m3t41ng"), A(IntegerAtom 45), A(NilAtom "NIL")])]
            r `shouldBe` expectedOutput

    describe "Eval list nth" $ do
        it "should Eval '(nth 0 (list)) into EvalErrorAtom 'eval's nth Error: ListExpression out of range error'" $ do
            let input            = "(nth 0 (list))"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A(EvalErrorAtom "eval's nth Error: ListExpression out of range error")]
            r `shouldBe` expectedOutput
        it "should Eval '(nth 0 87345) into EvalErrorAtom 'eval's nth Error: nth's second item not an iterable'" $ do
            let input            = "(nth 0 87345)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A(EvalErrorAtom "eval's nth Error: nth's second item not an iterable")]
            r `shouldBe` expectedOutput

        it "should Eval '(nth -456 (list)) into EvalErrorAtom 'eval's nth Error: ListExpression out of range error'" $ do
            let input            = "(nth -456 (list))"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A(EvalErrorAtom "eval's nth Error: ListExpression out of range error")]
            r `shouldBe` expectedOutput

        it "should Eval '(nth 0 (list 1)) into IntegerAtom 1" $ do
            let input            = "(nth 0 (list 1))"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A(IntegerAtom 1)]
            r `shouldBe` expectedOutput
        it "should Eval '(nth 345345 (list 1)) into EvalErrorAtom 'eval's nth Error: ListExpression out of range error'" $ do
            let input            = "(nth 345345 (list 1))"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A(EvalErrorAtom "eval's nth Error: ListExpression out of range error")]
            r `shouldBe` expectedOutput
        it "should Eval '(nth -9574 (list 45645)) into EvalErrorAtom 'eval's nth Error: ListExpression out of range error'" $ do
            let input            = "(nth -9574 (list 45645))"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A(EvalErrorAtom "eval's nth Error: ListExpression out of range error")]
            r `shouldBe` expectedOutput

        it "should Eval '(nth 0 (list 'giknwa')) into StringAtom 'giknwa'" $ do
            let input            = "(nth 0 (list \"giknwa\"))"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A(StringAtom "giknwa")]
            r `shouldBe` expectedOutput
        
        it "should Eval '(nth 0 (list NIL)) into NilAtom 'NIL'" $ do
            let input            = "(nth 0 (list NIL))"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A(NilAtom "NIL")]
            r `shouldBe` expectedOutput

        it "should Eval '(nth 0 (list T)) into TrueAtom 'T'" $ do
            let input            = "(nth 0 (list T))"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A(TrueAtom "T")]
            r `shouldBe` expectedOutput

        it "should Eval '(nth 0 (list  (+ 45 566) 'lfsmgk45#55dfg' (>= 4857 4857) )) (nth 1 (list  (+ 45 566) 'lfsmgk45#55dfg' (>= 4857 4857) )) (nth 2 (list  (+ 45 566) 'lfsmgk45#55dfg' (>= 4857 4857) )) into IntegerAtom 611 , StringAtom 'lfsmgk45#55dfg' , TrueAtom 'T'" $ do
            let input            = "(nth 0 (list  (+ 45 566) \"lfsmgk45#55dfg\" (>= 4857 4857) )) (nth 1 (list  (+ 45 566) \"lfsmgk45#55dfg\" (>= 4857 4857) )) (nth 2 (list  (+ 45 566) \"lfsmgk45#55dfg\" (>= 4857 4857) ))"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A(IntegerAtom 611)]
            r `shouldBe` expectedOutput
            let (tokens'', ast')     = parse    tokens'     []
            let (e', s', r')         = eval ast' s          []
            let expectedOutput'      = [A (StringAtom "lfsmgk45#55dfg")]
            r' `shouldBe` expectedOutput'
            let (tokens''', ast'')     = parse    tokens''     []
            let (e'', s'', r'')         = eval ast'' s'        []
            let expectedOutput''      = [A(TrueAtom "T")]
            r'' `shouldBe` expectedOutput''

    describe "Eval list append" $ do
        it "should Eval '(append (list) 9476) into ListExpression [IntegerAtom 9476]'" $ do
            let input            = "(append (list) 9476)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [ListExpression [A(IntegerAtom (9476))]]
            r `shouldBe` expectedOutput

        it "should Eval '(append (list) -9476) into ListExpression [IntegerAtom -9476]'" $ do
            let input            = "(append (list) -9476)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [ListExpression [A(IntegerAtom (-9476))]]
            r `shouldBe` expectedOutput

        it "should Eval '(append (list) 's[o]dksj') into ListExpression [StringAtom 's[o]dksj']'" $ do
            let input            = "(append (list) \"s[o]dksj\")"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [ListExpression [A(StringAtom "s[o]dksj")]]
            r `shouldBe` expectedOutput

    it "should Eval '(append (list) T) into ListExpression [TrueAtom 'T']'" $ do
            let input            = "(append (list) T)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [ListExpression [A(TrueAtom "T")]]
            r `shouldBe` expectedOutput

    it "should Eval '(append (list) NIL) into ListExpression [NilAtom 'NIL']'" $ do
            let input            = "(append (list) NIL)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [ListExpression [A(NilAtom "NIL")]]
            r `shouldBe` expectedOutput

    it "should Eval '(append (list) NIL) into ListExpression [NilAtom 'NIL']'" $ do
            let input            = "(append (list) NIL)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [ListExpression [A(NilAtom "NIL")]]
            r `shouldBe` expectedOutput

    it "should Eval '(append (list 8475) 38) into ListExpression [IntegerAtom 8475, IntegerAtom 38]'" $ do
            let input            = "(append (list 8475) 38)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [ListExpression [A(IntegerAtom 8475), A(IntegerAtom 38)]]
            r `shouldBe` expectedOutput
    it "should Eval '(append (list 8475) 38) into ListExpression [IntegerAtom 8475, IntegerAtom 38]'" $ do
            let input            = "(append (list 8475) 38)"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [ListExpression [A(IntegerAtom 8475), A(IntegerAtom 38)]]
            r `shouldBe` expectedOutput
    it "should Eval '(defun lfkgSfg(als rhs)(+ als rhs)) (append (list 8475) (lfkgSfg 455 -345)) into ListExpression [IntegerAtom 8475, IntegerAtom 38]'" $ do
            let input            = "(defun lfkgSfg(als rhs)(+ als rhs)) (append (list 8475) (lfkgSfg 455 -345))"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A(StringAtom "lfkgSfg")]
            r `shouldBe` expectedOutput
            let (tokens'', ast')     = parse    tokens'     []
            let (e', s', r')         = eval ast' s          []
            let expectedOutput'      = [ListExpression [A(IntegerAtom 8475), A(IntegerAtom 110)]]
            r' `shouldBe` expectedOutput'

    it "should Eval '(defun lfkgSfg(als rhs)(+ als rhs)) (append (list 8475) (lfkgSfg 455 -345)) into ListExpression [IntegerAtom 8475, IntegerAtom 38]'" $ do
            let input            = "(defun lfkgSfg(als rhs)(* als rhs)) (defvar djdhgmFhr (list 8475))  (append djdhgmFhr (lfkgSfg -4566345 3453)) (setq djdhgmFhr (append djdhgmFhr (lfkgSfg -4566345 3453))) djdhgmFhr"
            let tokens           = tokenize input   []
            let (tokens', ast)   = parse    tokens  []
            let scpe             = makeGlobalScope
            let (e, s, r)        = eval ast scpe    []
            let expectedOutput   = [A(StringAtom "lfkgSfg")]
            r `shouldBe` expectedOutput
            let (tokens'', ast')     = parse    tokens'     []
            let (e', s', r')         = eval ast' s          []
            let expectedOutput'      = [A(StringAtom "djdhgmFhr")]
            r' `shouldBe` expectedOutput'
            let (tokens''', ast'')   = parse    tokens''     []
            let (e'', s'', r'')      = eval ast'' s'          []
            let expectedOutput''     = [ListExpression [A(IntegerAtom 8475), A(IntegerAtom (-15767589285))]]
            r'' `shouldBe` expectedOutput''
            let (tokens'''', ast''') = parse    tokens'''     []
            let (e''', s''', r''')   = eval ast''' s''          []
            let expectedOutput'''    = [ListExpression [A(IntegerAtom 8475), A(IntegerAtom (-15767589285))]]
            r''' `shouldBe` expectedOutput'''
            let (tokens''''', ast'''')     = parse    tokens''''     []
            let (e'''', s'''', r'''')      = eval ast'''' s'''          []
            let expectedOutput''''         = [ListExpression [A(IntegerAtom 8475), A(IntegerAtom (-15767589285))]]
            r'''' `shouldBe` expectedOutput''''

-- describe "Eval list replaceAtNth" $ do
--     it "should Eval (replaceAtNth 0 (list) 'lkdjffgh%65g234') as EvalErrorAtom 'eval's replaceAtNth Error: ListExpression out of range error'" $ do
--             let input            = "(replaceAt 0 (list) \"lkdjffgh%65g234\")"
--             let tokens           = tokenize input   []
--             let (tokens', ast)   = parse    tokens  []
--             let scpe             = makeGlobalScope
--             let (e, s, r)        = eval ast scpe    []
--             let expectedOutput   = [A(EvalErrorAtom "eval's replaceAtNth Error: ListExpression out of range error")]
--             r `shouldBe` expectedOutput
-- -- TODO(JoanMontas) ... Someone has been naughty not unit-testing... replaceAtNth insertAtNth and removeNth
