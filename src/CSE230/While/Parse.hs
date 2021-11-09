module CSE230.While.Parse where

import Text.Parsec hiding (State, between)
import Text.Parsec.String
import qualified CSE230.While.Types as H

{- As you can see, it is rather tedious to write the above tests! 
   They correspond to the code in the files `test.imp` and `fact.imp`. 
   It is rather tedious to have to specify individual programs as Haskell
   values. For this problem, you will use parser combinators to build a parser
   for the WHILE language from the previous problem.
-}

parseFromString :: Parser a -> String -> Either ParseError a 
parseFromString p s = runParser p () "DUMMY" s 

-- >>> parseFromString varP "X45"
-- Right "X"
--

-------------------------------------------------------------------------------
-- | Parsing Constants
-------------------------------------------------------------------------------

-- First, we will write parsers for the `Value` type

valueP :: Parser H.Value
valueP = try intP <|> try boolP

-- First, fill in the implementation of `intP`. You can assume that the numbers
-- in our language are non-negative.

intP :: Parser H.Value
intP = do 
      rd <- many1 digit 
      return $ H.IntVal $ read rd

-- Next, define a parser that will accept a particular string `s` as a given value `x`

constP :: String -> a -> Parser a
constP s x = string s >> notFollowedBy alphaNum >> return x

-- Use the above to define a parser for boolean values 
-- where `"true"` and `"false"` should be parsed appropriately.

boolP :: Parser H.Value
boolP = try (constP "true" (H.BoolVal True)) 
      <|> try (constP "false" (H.BoolVal False))

-- Continue to use the above to parse the binary operators

opP :: Parser H.Bop
opP = (constP "+" H.Plus)
   <|> (constP "-" H.Minus)
   <|> (constP "*" H.Times)
   <|> (constP "/" H.Divide)
   <|> (constP ">" H.Gt)
   <|> (constP ">=" H.Ge)
   <|> (constP "<" H.Lt)
   <|> (constP "<=" H.Le)

-------------------------------------------------------------------------------
-- | Parsing Expressions 
-------------------------------------------------------------------------------

-- The following is a parser for variables, which are one-or-more uppercase letters. 

varP :: Parser H.Variable
varP = many1 upper

-- Use the above to write a parser for `Expression` values. Assume that
-- operators are right associative, and they all have the same precedence.

exprP :: Parser H.Expression
exprP   = try opExprP <|> try parentheseP <|> try varOrValP

-- Helpers
makeExpr :: (Monad m) => (a1 -> r) -> m a1 -> m r
makeExpr f m1 = m1 >>= \m1 -> return $ f m1

parentheseP :: Parser H.Expression
parentheseP = do char '(' >> exprP >>= \e -> char ')' >> return e

-- make val and var as Expr
varOrValP :: Parser H.Expression
varOrValP = (makeExpr H.Var varP) <|> (makeExpr H.Val valueP)

-- ((1 + 2) - 3) + (1 + 3)
-- (1 + 3) | 1 + 3
-- Par,Val,Space,Op,Space,Val,Par
-- ((1 + 2) - 3)
-- Par,Expr,Space,Op,Space,Val,Par
opExprP :: Parser H.Expression
opExprP = do
            r <- try varOrValP <|> try parentheseP
            spaces
            cop <- opP
            spaces
            res <- exprP
            return $ H.Op cop r res

-- >>> parseFromString exprP "((1 + 2) - 3) + (1 + 3)"
-- Right (Op Plus (Op Minus (Op Plus (Val (IntVal 1)) (Val (IntVal 2))) (Val (IntVal 3))) (Op Plus (Val (IntVal 1)) (Val (IntVal 3))))
--

-------------------------------------------------------------------------------
-- | Parsing Statements 
-------------------------------------------------------------------------------

-- Next, use the expression parsers to build a statement parser

statementP :: Parser H.Statement
statementP = try sequenceP <|> try assignmentP <|> try ifP <|> try whileP <|> try skipP

-- Helpers

-- skip
skipP :: Parser H.Statement
skipP = string "skip" >> return H.Skip

{-
  while X > 1 do
    F := Z + F;
    X := X - 1
  endwhile; 
-}
whileP :: Parser H.Statement
whileP = do string "while"
            spaces
            cond <- exprP 
            spaces
            string "do"
            spaces
            stateP <- statementP
            spaces
            string "endwhile"
            return $ H.While cond stateP

{- 
   F := Z + F
-}
assignmentP :: Parser H.Statement
assignmentP = do 
               var <- varP
               spaces
               string ":="
               spaces
               res <- exprP
               return $ H.Assign var res

{- 
if X < 0 then
  X := 0 - X
else
  skip
endif
-}
ifP :: Parser H.Statement
ifP = do string "if"
         spaces
         cond <- exprP
         spaces
         string "then"
         spaces
         std <- statementP
         spaces
         string "else"
         spaces
         stdE <- statementP
         spaces
         string "endif"
         return $ H.If cond std stdE

{-
   F := Z + F;
   X := X - 1
-}
sequenceP :: Parser H.Statement
sequenceP = do s1 <- try assignmentP <|> try whileP <|> try ifP <|> try skipP
               string ";"
               spaces
               s2 <- statementP
               return $ H.Sequence s1 s2


-- When you are done, we can put the parser and evaluator together 
-- in the end-to-end interpreter function `runFile` in `Main.hs`

-- | Parsing Files 

-------------------------------------------------------------------------------
parseFile :: FilePath -> IO (Either ParseError H.Statement)
-------------------------------------------------------------------------------
--- >>> parseFile "test/in/self-assign.imp"
--- Right (Sequence (Assign "N" (Val (IntVal 2))) (Sequence (Assign "F" (Val (IntVal 1))) (Sequence (Assign "N" (Val (IntVal 2))) (Assign "F" (Val (IntVal 1))))))
---
--- >>> parseFile "test/in/self-while.imp"
--- Right (While (Op Gt (Var "X") (Val (IntVal 1))) (Sequence (Assign "F" (Op Plus (Var "Z") (Var "F"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1))))))
---
--- >>> parseFile "test/in/self-if.imp"
--- Right (If (Op Lt (Var "X") (Val (IntVal 0))) (Assign "X" (Op Minus (Val (IntVal 0)) (Var "X"))) Skip)
---
-- >>> ((Right H.w_fact) ==) <$> parseFile "test/in/fact.imp"
-- True
-- >>> ((Right H.w_test) == ) <$> parseFile "test/in/test.imp"
-- True
-- >>> ((Right H.w_abs) == ) <$> parseFile "test/in/abs.imp"
-- True
-- >>> ((Right H.w_times) == ) <$> parseFile "test/in/times.imp"
-- True


parseFile f = parseFromFile statementP f
