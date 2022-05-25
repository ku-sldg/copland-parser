
{-# LANGUAGE GADTs, FlexibleContexts #-}

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Function
import Test.QuickCheck.Monadic

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.Parsec.Char
import CoplandLang
--import Data.List
import PrettyPrinter
import CoplandQC

--Takes a String to a Haskell AST
languageDef =
  javaStyle { identStart = lower
            , identLetter = (alphaNum <|> char '_')
            , reservedNames = [ "_"
                              , "!"
                              , "#"
                              , "+"
                              , "-"
                              , "@"
                              , "{}"
                              , "["
                              , "]"
                              ]
            , reservedOpNames = ["->"]
            , commentLine = "%"
            }

expr  = buildExpressionParser table term <?> "phrase"

term    = parenz
          <|> gen_atExpr
          <|> asptExpr
          <?> "unknown input"

table   :: OperatorTable Char () T
table   = [ 
  [ inFix "->" LN AssocRight ],
  [
    inFix "-<-" (BRS (NONE,NONE)) AssocRight, 
    inFix "-<+" (BRS (NONE, ALL)) AssocNone,
    inFix "+<-" (BRS (ALL, NONE)) AssocNone,
    inFix "+<+" (BRS (ALL, ALL)) AssocNone,
    inFix "-~-" (BRP (NONE, NONE)) AssocNone,
    inFix "-~+" (BRP (NONE, ALL)) AssocNone,
    inFix "+~-" (BRP (ALL, NONE)) AssocNone,
    inFix "+~+" (BRP (ALL, ALL)) AssocNone]]

lexer = makeTokenParser languageDef

inFix o c assoc = Infix (do { reservedOp lexer o; return c }) assoc

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

--ASPT Parser
chooseASP :: Parser ASP
chooseASP = cpyExpr <|> sigExpr <|> hshExpr <|> nullExpr <|> aspcExpr 


asptExpr :: Parser T
asptExpr = try $ do i <- chooseASP
                    void spaces
                    return (ASPT i)                    

cpyExpr :: Parser ASP
cpyExpr = do char '_'
             return CPY

sigExpr :: Parser ASP
sigExpr = do char '!'
             return SIG

hshExpr :: Parser ASP
hshExpr = do char '#'
             return HSH

nullExpr :: Parser ASP
nullExpr = do string "{}"
              return NULL

aspcExpr :: Parser ASP
aspcExpr = do sym1 <- symbolExpr
              void spaces 
              place <- placeExpr
              void spaces 
              sym2 <- symbolExpr
              void spaces 
              -- No args in def -- arg <- sepBy (many (noneOf " ,\"()-+@_!#")) (char ',')
              return (SPS sym1 place sym2)

-- When a place is a sequence of digits, it is equivalent to the symbol that results from adding the letter p to the beginning of the digits. 
digitsExpr :: Parser SYMBOL
digitsExpr = do dig <- natural lexer
                return ("p" ++ (show dig))

symbolExpr :: Parser SYMBOL
symbolExpr = do sym <- (identifier lexer <?> "valid identifier starting with lowercase, remainder alpha-numeric + '_'")
                return sym 

placeExpr :: Parser PLACE
placeExpr = do plc <- symbolExpr <|> digitsExpr <?> "place"
               return (PLC plc)

-- AT Parser
atExpr :: PLACE -> Parser T
atExpr pl = do ph <- expr
               return (AT pl ph)

-- AT_S Parser (with square brackets)
at_SExpr :: PLACE -> Parser T
at_SExpr pl = do char '['
                 ph <- expr
                 char ']' <?> "closing square bracket"
                 return (AT_S pl ph)

gen_atExpr :: Parser T
gen_atExpr = do char '@'
                place <- placeExpr
                void spaces
                ph <- at_SExpr place <|> (atExpr place)
                return ph

--Parentheses Parser
parenz :: Parser T
parenz = do void $ char '('
            e <-  expr 
            void $ char ')'
            void spaces
            return e

parseExpr   :: Parser T
parseExpr = do phr <- expr
               (do {spaces; skipMany1 newline}) <|> eof <?> "end of phrase"
               return phr

--total parser
parsePhrase :: String -> T
parsePhrase str = case parse parseExpr "" str of
                    Left e -> error $ show e
                    Right r -> r

coplandStarExpr :: Parser COPLAND
coplandStarExpr = do char '*'
                     void spaces
                     place <- placeExpr
                     void spaces
                     char ':'
                     void spaces
                     phr <- expr
                     return (STAR place phr)

coplandS_expr :: Parser COPLAND
coplandS_expr = do phr <- expr
                   return (COP_PHRASE phr)

coplandExpr :: Parser COPLAND
coplandExpr = do cop <- (coplandStarExpr <|> coplandS_expr)
                 return cop

--total parser
parseCop :: String -> COPLAND
parseCop str = case parse coplandExpr "" str of
                    Left e -> error $ show e
                    Right r -> r

checkSame :: T -> T -> Bool 
checkSame t1 t2 = pprint t1 == pprint t2