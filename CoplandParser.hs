
{-# LANGUAGE GADTs, FlexibleContexts #-}

module CoplandParser where

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
import PrettyPrinter

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

expr  = buildExpressionParser table term-- <?> "phrase"

term    = parenz
          <|> gen_atExpr
          <|> asptExpr
          -- <?> "unknown input"

table   :: OperatorTable Char () T
table   = [ 
  [ inFix "->" LN AssocRight ],
  -- TODO: Arbitrary introduction of right associativity here, for branch operators
  [
  inFix "-<-" (BRS (NONE,NONE)) AssocRight,
  inFix "-<+" (BRS (NONE, ALL)) AssocRight,
  inFix "+<-" (BRS (ALL, NONE)) AssocRight,
  inFix "+<+" (BRS (ALL, ALL)) AssocRight,
  inFix "-~-" (BRP (NONE, NONE)) AssocRight,
  inFix "-~+" (BRP (NONE, ALL)) AssocRight,
  inFix "+~-" (BRP (ALL, NONE)) AssocRight,
  inFix "+~+" (BRP (ALL, ALL)) AssocRight]]

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
symbolExpr = do sym <- (identifier lexer <?> "(valid identifier starting with lowercase, remainder alpha-numeric + '_')")
                return sym 

placeExpr :: Parser PLACE
placeExpr = do plc <- symbolExpr <|> digitsExpr
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
            return (PAREN e)

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
                     place <- placeExpr <?> "missing initial place"
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
checkSame t1 t2 = transAST_T_Cop t1 == transAST_T_Cop t2

-- Need quickCheck to generate good copland string for these to be valuable
-- quickCheck_parsable     :: String -> Bool
-- quickCheck_parsable t   = case  parse coplandExpr "" t of
--                                 Left e -> False
--                                 Right r -> True

-- involutiveParse :: String -> Bool
-- involutiveParse s = case transAST_T_Cop (parsePhrase s) == s of
--                         True -> True
--                         False -> error $ show (parsePhrase s, (transAST_T_Cop (parsePhrase s)))

involutiveAST :: T -> Bool
involutiveAST s = case parsePhrase (transAST_T_Cop s) == s of
                        True -> True
                        False -> error $ show (transAST_T_Cop s, (parsePhrase (transAST_T_Cop s)))


