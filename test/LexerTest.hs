-- test/LexerTest.hs

import Lexer
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Lexer Single Token" $ do
    it "should tokenize '' as Nothing" $ do
      let inputNothing = ""
      let actualNothing = []
      (tokenize inputNothing emptyListOfToken) `shouldBe` actualNothing
    it "should tokenize '1' as an IntegerToken" $ do
      let input1 = "1"
      let actual1 = [IntegerToken 1]
      (tokenize input1 emptyListOfToken) `shouldBe` actual1
    it "should tokenize '1337' as an IntegerToken" $ do
      let input1337 = "1337"
      let actual1337 = [IntegerToken 1337]
      (tokenize input1337 emptyListOfToken) `shouldBe` actual1337
    it "should tokenize '-9' as an IntegerToken" $ do
      let inputNegativeNine = "-9"
      let actualNegativeNine = [IntegerToken (-9)]
      (tokenize inputNegativeNine emptyListOfToken) `shouldBe` actualNegativeNine
    it "should tokenize '-9' as an IntegerToken" $ do
      let inputNegativeThreeOneFourOneFive = "-31415"
      let actualNegativeThreeOneFourOneFive = [IntegerToken (-31415)]
      (tokenize inputNegativeThreeOneFourOneFive emptyListOfToken) `shouldBe` actualNegativeThreeOneFourOneFive
    it "should tokenize 'someIdent' as an IndentToken" $ do
      let inputSomeIdent = "someIdent"
      let actualSomeIdent = [IdentToken "someIdent"]
      (tokenize inputSomeIdent emptyListOfToken) `shouldBe` actualSomeIdent
    it "should tokenize '\"someIdent\"' as an IndentToken" $ do
      let inputSomeString = "\"someString\""
      let actualSomeString = [StringToken "someString"]
      (tokenize inputSomeString emptyListOfToken) `shouldBe` actualSomeString
    it "should tokenize '(' as a LParenToken" $ do
      let inputLparenToken = "("
      let actualLparenToken = [LParenToken '(']
      (tokenize inputLparenToken emptyListOfToken) `shouldBe` actualLparenToken
    it "should tokenize ')' as a TParenToken" $ do
      let inputRparenToken = ")"
      let actualRparenToken = [RParenToken ')']
      (tokenize inputRparenToken emptyListOfToken) `shouldBe` actualRparenToken
    it "should tokenize ')' as a TParenToken" $ do
      let inputRparenToken = ")"
      let actualRparenToken = [RParenToken ')']
      (tokenize inputRparenToken emptyListOfToken) `shouldBe` actualRparenToken
    it "should tokenize '+' as an OperatorToken" $ do
      let inputOperatorTokenPlus = "+"
      let actualOperatorTokenPlus = [OperatorToken "+"]
      (tokenize inputOperatorTokenPlus emptyListOfToken) `shouldBe` actualOperatorTokenPlus
    it "should tokenize '-' as an OperatorToken" $ do
      let inputOperatorTokenMinus = "-"
      let actualOperatorTokenMinus = [OperatorToken "-"]
      (tokenize inputOperatorTokenMinus emptyListOfToken) `shouldBe` actualOperatorTokenMinus
    it "should tokenize '*' as an OperatorToken" $ do
      let inputOperatorTokenMult = "*"
      let actualOperatorTokenMult = [OperatorToken "*"]
      (tokenize inputOperatorTokenMult emptyListOfToken) `shouldBe` actualOperatorTokenMult
    it "should tokenize '>' as an OperatorToken" $ do
      let inputOperatorTokenGreater = ">"
      let actualOperatorTokenGreater = [OperatorToken ">"]
      (tokenize inputOperatorTokenGreater emptyListOfToken) `shouldBe` actualOperatorTokenGreater
    it "should tokenize '>=' as an OperatorToken" $ do
      let inputOperatorTokenGreaterEqual = ">="
      let actualOperatorTokenGreaterEqual = [OperatorToken ">="]
      (tokenize inputOperatorTokenGreaterEqual emptyListOfToken) `shouldBe` actualOperatorTokenGreaterEqual
    it "should tokenize '<' as an OperatorToken" $ do
      let inputOperatorTokenLesser = "<"
      let actualOperatorTokenLesser = [OperatorToken "<"]
      (tokenize inputOperatorTokenLesser emptyListOfToken) `shouldBe` actualOperatorTokenLesser
    it "should tokenize '<=' as an OperatorToken" $ do
      let inputOperatorTokenLesserEqual = "<="
      let actualOperatorTokenLesserEqual = [OperatorToken "<="]
      (tokenize inputOperatorTokenLesserEqual emptyListOfToken) `shouldBe` actualOperatorTokenLesserEqual

  describe "Lexer Single Token Error" $ do
    it "should tokenize '&' as \"Lexer Error: Unkown Token\"" $ do
      let inputAsterisk = "&"
      let actualAsteriskUnkownTokenError = [LexErrorToken "Lexer Error: Unkown Token"]
      (tokenize inputAsterisk emptyListOfToken) `shouldBe` actualAsteriskUnkownTokenError
    it "should tokenize \"Look, I am not closing the string as \"Lexer-Error: Did not close String\"" $ do
      let inputUnclosedString = "\"Look, I am not closing the string"
      let actualUnclosedString = [LexErrorToken "Lexer-Error: Did not close String"]
      (tokenize inputUnclosedString emptyListOfToken) `shouldBe` actualUnclosedString
    it "should tokenize 'Look, I forgot to open the string' as \"LexErrorToken \"Lexer Error: Unkown Token\"" $ do
      let inputUnOpenString = "Look, I forgot to open the string\""
      let actualUnOpenString = [LexErrorToken "Lexer Error: Unkown Token"]
      (tokenize inputUnOpenString emptyListOfToken) `shouldBe` actualUnOpenString
    it "should tokenize 'Look I forgot to open the string' as \"Lexer-Error: Did not close String\"" $ do
      let inputUnOpenString = "Look I forgot to open the string\""
      let actualUnOpenString = [LexErrorToken "Lexer-Error: Did not close String"]
      (tokenize inputUnOpenString emptyListOfToken) `shouldBe` actualUnOpenString

  describe "Lexer Eat White Space" $ do
    it "should tokenize '      ' as Nothing" $ do
      let inputNothing = "      "
      let actualNothing = []
      (tokenize inputNothing emptyListOfToken) `shouldBe` actualNothing
    it "should tokenize '  1     ' as an IntegerToken" $ do
      let input1 = "  1     "
      let actual1 = [IntegerToken 1]
      (tokenize input1 emptyListOfToken) `shouldBe` actual1
    it "should tokenize '\"      \"' as an StringToken" $ do
      let inputStringOfFiveEmpty = "\"     \""
      let actualStringOfFiveEmpty = [StringToken "     "]
      (tokenize inputStringOfFiveEmpty emptyListOfToken) `shouldBe` actualStringOfFiveEmpty

  describe "Lexer Should Separate Ident From Non-AlphaNumerical" $ do
    it "should tokenize 'someIdent123+3'" $ do
      let inputSomeIdentSummedWithThree = "someIdent123+3"
      let actualSomeIdentSummedWithThree = [IdentToken "someIdent123", OperatorToken "+", IntegerToken 3]
      (tokenize inputSomeIdentSummedWithThree emptyListOfToken) `shouldBe` actualSomeIdentSummedWithThree
