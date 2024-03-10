-- test/ParserText.hs

-- Copyright Joan Montas
-- All rights reserved.
-- License under GNU General Public License v3.0

import Test.Hspec
import Lexer
import Parser
main :: IO ()
main = hspec $ do
  describe "Parse Single Atoms Integer" $ do
    it "should parse '1' into IntegerAtom 1" $ do
      let input            = "1"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (IntegerAtom 1)]
      ast `shouldBe` expectedOutput

    it "should parse '45645765' into IntegerAtom 45645765" $ do
      let input            = "45645765"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (IntegerAtom 45645765)]
      ast `shouldBe` expectedOutput

    it "should parse '-1' into IntegerAtom (-1)" $ do
      let input            = "1"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (IntegerAtom 1)]
      ast `shouldBe` expectedOutput

    it "should parse '-45645765' into IntegerAtom (-45645765)" $ do
      let input            = "-45645765"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (IntegerAtom (-45645765))]
      ast `shouldBe` expectedOutput

  describe "Parse Single Atoms Ident" $ do
    it "should parse 'H' into IdentAtom \"H\"" $ do
      let input            = "H"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (IdentAtom "H")]
      ast `shouldBe` expectedOutput

    it "should parse 'HelloWorld' into IdentAtom \"HelloWorld\"" $ do
      let input            = "HelloWorld"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (IdentAtom "HelloWorld")]
      ast `shouldBe` expectedOutput

  describe "Parse Single Atoms String" $ do
    it "should parse '\"H\"' into StringAtom \"H\"" $ do
      let input            = "\"H\""
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (StringAtom "H")]
      ast `shouldBe` expectedOutput

    it "should parse '\"HelloWorld\"' into StringAtom \"HelloWorld\"" $ do
      let input            = "\"HelloWorld\""
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (StringAtom "HelloWorld")]
      ast `shouldBe` expectedOutput

  describe "Parse Single Atom Nil" $ do
    it "should parse 'NIL' into NilAtom NIL" $ do
      let input            = "NIL"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (NilAtom "NIL")]
      ast `shouldBe` expectedOutput

    it "should parse '()' into NilAtom NIL" $ do
      let input            = "()"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (NilAtom "NIL")]
      ast `shouldBe` expectedOutput
  
  describe "Parse Single Atom T" $ do
    it "should parse 'T' into TrueAtom T" $ do
      let input            = "T"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (TrueAtom "T")]
      ast `shouldBe` expectedOutput

  describe "Parse Basic CallExpression" $ do
    it "should parse (+) into CallExpression (+) " $ do
      let input            = "(+)"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [CallExpression ( A ( IdentAtom "+" ) ) []]
      ast `shouldBe` expectedOutput

    it "should parse (+ 1 2) into CallExpression + [1 2]" $ do
      let input            = "(+ 1 2)"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [CallExpression ( A ( IdentAtom "+" ) ) [A(IntegerAtom 1), A(IntegerAtom 2)]]
      ast `shouldBe` expectedOutput

  describe "Parse CallExpression Error" $ do
    it "should parse '(' into ParseError" $ do
      let input            = "("
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (ParseErrorAtom "parse's parseSexpression Error: Unkown Call Condition")]
      ast `shouldBe` expectedOutput

    it "should parse '(someIdent' into ParseError" $ do
      let input            = "(someIdent"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (ParseErrorAtom "parse's parseCallSexpression Error: S-Expression not closed")]
      ast `shouldBe` expectedOutput

    it "should parse '(someIdent 1' into ParseError" $ do
      let input            = "(someIdent 1"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (ParseErrorAtom "parse's parseGroupSexpression Error: S-Expression not closed")]
      ast `shouldBe` expectedOutput

    it "should parse '(someIdent otherIdent 1' into ParseError" $ do
      let input            = "(someIdent otherIdent 1"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (ParseErrorAtom "parse's parseGroupSexpression Error: S-Expression not closed")]
      ast `shouldBe` expectedOutput

  describe "Parse IfExpression" $ do
    it "should parse '(if 1 2)' into IfExpression 1 2 NIL" $ do
      let input            = "(if 1 2)"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [IfExpression (A(IntegerAtom 1)) (A(IntegerAtom 2)) (A(NilAtom "NIL"))]
      ast `shouldBe` expectedOutput

    it "should parse '(if 1 2)' into IfExpression 1 2 3" $ do
      let input            = "(if 1 2 3)"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [IfExpression (A(IntegerAtom 1)) (A(IntegerAtom 2)) (A(IntegerAtom 3))]
      ast `shouldBe` expectedOutput

    it "should parse '(if (+ 44 3456) 968396 454523)' into IfExpression (CallExpression + [44, 3456]) 968396 454523" $ do
      let input            = "(if (+ 44 3456) 968396 454523)"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [IfExpression (CallExpression ( A ( IdentAtom "+" ) ) [A(IntegerAtom 44), A(IntegerAtom 3456)]) (A(IntegerAtom 968396)) (A(IntegerAtom 454523))]
      ast `shouldBe` expectedOutput
      
    it "should parse '(if (+ 44 1) 2 (+ 65256 \"sfffssdfg\"))' into IfExpression (CallExpression + [44, 1]) 2 (CallExpression [65256, \"sfffssdfg\"])" $ do
      let input            = "(if (+ 44 1) 2 (+ 65256 \"sfffssdfg\"))"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [IfExpression (CallExpression ( A ( IdentAtom "+" ) ) [A(IntegerAtom 44), A(IntegerAtom 1)]) (A(IntegerAtom 2)) (CallExpression ( A ( IdentAtom "+" ) ) [A(IntegerAtom 65256), A(StringAtom "sfffssdfg")])]
      ast `shouldBe` expectedOutput

  describe "Parse IfExpression Error" $ do
    it "should parse '(if' into ParseError" $ do
      let input            = "(if"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (ParseErrorAtom "parse's parseCallSexpression Error: S-Expression not closed")]
      ast `shouldBe` expectedOutput
    it "should parse '(if 1' into ParseError" $ do
      let input            = "(if 1"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (ParseErrorAtom "parse's parseIfStatement Error: Could Not Parse Body")]
      ast `shouldBe` expectedOutput
    it "should parse '(if 1 2' into ParseError" $ do
      let input            = "(if 1 2"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (ParseErrorAtom "parse's parseIfStatement Error: Did Not Close If-Statement")]
      ast `shouldBe` expectedOutput

    it "should parse '(if - 2)' into ParseError" $ do
      let input            = "(if - 2)"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (ParseErrorAtom "parse's parseIfStatement Error: Could Not Parse Condition-Statement")]
      ast `shouldBe` expectedOutput

    it "should parse '(if - -)' into ParseError" $ do
      let input            = "(if - -)"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (ParseErrorAtom "parse's parseIfStatement Error: Could Not Parse Condition-Statement")]
      ast `shouldBe` expectedOutput

    it "should parse '(if 1 -)' into ParseError" $ do
      let input            = "(if 1 -)"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (ParseErrorAtom "parse's parseIfStatement Error: Could Not Parse Body")]
      ast `shouldBe` expectedOutput

    it "should parse '(if 1 1 -)' into ParseError" $ do
      let input            = "(if 1 1 -)"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (ParseErrorAtom "parse's parseIfStatement Error: Could Not Parse Else-Body")]
      ast `shouldBe` expectedOutput
    it "should parse '(if 1 1 1' into ParseError" $ do
      let input            = "(if 1 1 1"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A (ParseErrorAtom "parse's parseIfStatement Error: Did Not Close If-Statement")]
      ast `shouldBe` expectedOutput
      
  describe "Parse FunctionExpression" $ do
    it "should parse '(defun foo(a) a)' into FunctionExpression (A(IdentAtom'foo') [A(IdentAtom 'a')] (A(IdentAtom 'a')) " $ do
      let input            = "(defun foo(a) a)"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [FunctionExpression (A (IdentAtom "foo")) [A (IdentAtom "a")] (A (IdentAtom "a"))]
      ast `shouldBe` expectedOutput

    it "should parse '(defun f(a b) (+ a b)' into FunctionExpression (A(IdentAtom'foo') [A(IdentAtom 'a'), A(IdentAtom 'b')] (CallExpression '+' [IdentAtom 'a', IdentAtom 'b'])) " $ do
      let input            = "(defun f(a b) (+ a b))"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [FunctionExpression (A (IdentAtom "f")) [A (IdentAtom "a"), A(IdentAtom "b")] (CallExpression (A(IdentAtom "+")) [A (IdentAtom "a"), A(IdentAtom "b")] )]
      ast `shouldBe` expectedOutput
    
    it "should parse '(defun lkm647() (+ a b)' into FunctionExpression (A(IdentAtom'foo') [A(IdentAtom 'a'), A(IdentAtom 'b')] (CallExpression '+' [IdentAtom 'a', IdentAtom 'b'])) " $ do
      let input            = "(defun lkm647() (* 45 b))"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [FunctionExpression (A (IdentAtom "lkm647")) [] (CallExpression (A(IdentAtom "*")) [A (IntegerAtom 45), A(IdentAtom "b")] )]
      ast `shouldBe` expectedOutput

    it "should parse '(defun ())' into FunctionExpression (A(IdentAtom'foo') [A(IdentAtom 'a')] (A(IdentAtom 'a')) " $ do
      let input            = "(defun foo(a) a)"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [FunctionExpression (A (IdentAtom "foo")) [A (IdentAtom "a")] (A (IdentAtom "a"))]
      ast `shouldBe` expectedOutput

  describe "Parse FunctionExpression Error" $ do
    it "should parse '(defun (a) a)' into ParseErrorAtom 'parse's parseDefunStatement Error: No Name Found'" $ do
      let input            = "(defun (a) a)"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A(ParseErrorAtom "parse's parseDefunStatement Error: No Name Found")]
      ast `shouldBe` expectedOutput

    it "should parse '(defun (a) a)' into ParseErrorAtom 'parse's parseDefunStatement Error: No Name Found'" $ do
      let input            = "(defun lfkj2 bodyBlockStatementHere"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A(ParseErrorAtom "parse's parseDefunStatement Error: Missing Argument")]
      ast `shouldBe` expectedOutput


    it "should parse '(defun gdhd2(b 34) a)' into ParseErrorAtom 'parse's parseDefunArguments Error: Unexpected Value'" $ do
      let input            = "(defun gdhd2(b 34) a)"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A(ParseErrorAtom "parse's parseDefunArguments Error: Unexpected Value")]
      ast `shouldBe` expectedOutput


    it "should parse '(defun gdhd2(fdkfm4 dkjad3 IAmTheBody)' into ParseErrorAtom 'parse's parseDefunStatement Error: Error Parsing Body'" $ do
      let input            = "(defun gdhd2(fdkfm4  IAmTheBody3 a)"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A(ParseErrorAtom "parse's parseDefunStatement Error: Error Parsing Body")]
      ast `shouldBe` expectedOutput
      

    it "should parse '(defun IAmValidButNotClose(fdkfm4 dkjad3) IAmTheBody' into ParseErrorAtom 'parse's parseDefunStatement Error: Error Parsing Body'" $ do
      let input            = "(defun IAmValidButNotClose(fdkfm4 dkjad3) IAmTheBody"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A(ParseErrorAtom "parse's parseDefunStatement Error: Did not Close defun-Statement")]
      ast `shouldBe` expectedOutput

    it "should parse '(defun IAmValidButNotClose(fdkfm4 dkjad3) IAmTheBody I4mExtra6' into ParseErrorAtom 'parse's parseDefunStatement Error: Error Parsing Body'" $ do
      let input            = "(defun IAmValidButNotClose(fdkfm4 dkjad3) IAmTheBody I4mExtra6)"
      let tokens           = tokenize input   []
      let (tokens', ast)   = parse    tokens  [] 
      let expectedOutput   = [A(ParseErrorAtom "parse's parseDefunStatement Error: Did not Close defun-Statement")]
      ast `shouldBe` expectedOutput
    