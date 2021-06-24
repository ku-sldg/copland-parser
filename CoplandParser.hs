
{-# LANGUAGE GADTs, FlexibleContexts #-}

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

--Takes a String to a Haskell AST
languageDef =
  javaStyle { identStart = letter
            , identLetter = alphaNum
            , reservedNames = [ "_"
                              , "!"
                              , "#"
                              , "ASPC"
                              , "+"
                              , "-"
                              , "@"
                              , "{}"
                              ]
            , reservedOpNames = ["->"]
            }

lexer = makeTokenParser languageDef

inFix :: String -> (a -> a -> a) -> Assoc -> Operator Char st a
inFix o c  = Infix (reservedOp lexer o >> return c) 
preFix :: String -> (a -> a) -> Operator Char st a
preFix o c = Prefix (reservedOp lexer o >> return c)

postFix :: String -> (a -> a) -> Operator Char st a
postFix o c = Postfix (reservedOp lexer o >> return c)

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

expr :: Parser T
expr = buildExpressionParser operators term

operators :: [[Operator Char st T]]
operators = [
   -- [ inFix "->" LN AssocRight ]
  ]


--ASPT Parser
asptExpr :: Parser T
asptExpr = try $ do i <- chooseASP
                    void spaces
                    parseAllWInput (ASPT i) <|> return (ASPT i)
                    

cpyExpr :: Parser ASP
cpyExpr = do i <- reserved lexer "_"
             return CPY

sigExpr :: Parser ASP
sigExpr = do i <- reserved lexer "!"
             return SIG

hshExpr :: Parser ASP
hshExpr = do i <- reserved lexer "#"
             return HSH

aspcExpr :: Parser ASP
aspcExpr = do i <- reserved lexer "ASPC"
              id <- read <$> many1 digit 
              void spaces 
              arg <- sepBy (many (noneOf " ,\")")) (char ',')
              return (ASPC id arg)


chooseASP :: Parser ASP
chooseASP = cpyExpr <|> sigExpr <|> hshExpr <|> aspcExpr


-- AT Parser
atExpr :: Parser T
atExpr = do i <- reserved lexer "@"
            pl <- read <$> many1 digit 
            void spaces
            ph <- try parseAll <|> expr
            return (AT pl ph)


--SEQUENTIAL BRANCHING Parser
brsExpr :: T -> Parser T
brsExpr p1 = try $ do
                   b <- bothBranchesS
                   p2 <- try parseAll <|> expr
                   return (BRS b p1 p2)

--PARALLEL BRANCHING Parser
brpExpr :: T -> Parser T
brpExpr p1 = try $ do
                   b <- bothBranchesP
                   p2 <- try parseAll <|> expr
                   return (BRP b p1 p2)

--Branch helper parsers
allBranch :: Parser SP
allBranch = do i <- reserved lexer "+"
               return ALL

noneBranch :: Parser SP
noneBranch = do i <- reserved lexer "-"
                return NONE

branches :: Parser SP
branches = allBranch <|> noneBranch

bothBranchesS :: Parser (SP, SP)
bothBranchesS = do b1 <- branches
                   void $ char '<'
                   b2 <- branches
                   return (b1, b2)

bothBranchesP :: Parser (SP, SP)
bothBranchesP = do b1 <- branches
                   void $ char '~'
                   b2 <- branches
                   return (b1, b2)

--LINEAR Parser
lnExpr :: T -> Parser T
lnExpr p1 = try $ do i <- reserved lexer "->"
                     p2 <- try parseLn <|> expr
                     try (parseAllWInput (LN p1 p2)) <|> return (LN p1 p2)

--Parentheses Parser
parenz :: Parser T
parenz = do void $ char '('
            e <- try parseAll <|> expr 
            void $ char ')'
            void spaces
            try (parseAllWInput e) <|> return e

term = parenz
    <|> atExpr
    <|> asptExpr
    <?> "unknown input"

--helper parsers
parseAll :: Parser T
parseAll = do i <- expr
              try (brsExpr i) <|> try (brpExpr i) <|> try (lnExpr i) <|> return i

parseAllWInput :: T -> Parser T
parseAllWInput i = try (brsExpr i) <|> try (brpExpr i) <|> try (lnExpr i) <|> return i

parseLn :: Parser T
parseLn = do i <- expr
             try (lnExpr i) <|> return i

parseBranches :: Parser T 
parseBranches = do i <- expr
                   try (brsExpr i) <|> try (brpExpr i) <|> return i

--total parser
parsePhrase :: String -> T
parsePhrase = parseString expr
