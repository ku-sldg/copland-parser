--{-# LANGUAGE GADTs, FlexibleContexts #-}

module PrettyPrinter where
import CoplandLang
--import Data.List
import Data.String

--Haskell AST to String
pprint :: T -> String 
pprint (ASPT n)  
          | show n == "CPY" = "_"
          | show n == "SIG" = "!"
          | show n == "HSH" = "#"
          | show n == "MT"  = "{}"
pprint (ASPT (ASPC n x)) = "ASPC " ++ show n ++ " " ++ toStr x
pprint (LN n m) = pprint n ++ " -> " ++ pprint m
pprint (AT n m) = "@ " ++ show n ++ " " ++ "(" ++ pprint m ++ ")"
pprint (BRS (b1,b2) n m) 
          | show b1 == "NONE" && show b2 == "NONE" =  "(" ++ pprint n ++ ")" ++ " -<- "  ++ pprint m 
          | show b1 == "ALL"  && show b2 == "NONE" =  "(" ++ pprint n ++ ")" ++ " +<- " ++ pprint m 
          | show b1 == "NONE" && show b2 == "ALL"  =  "(" ++ pprint n ++ ")" ++ " -<+ " ++ pprint m 
          | otherwise                              =  "(" ++ pprint n ++ ")" ++ " +<+ " ++ pprint m 
pprint (BRP (b1,b2) n m) 
          | show b1 == "NONE" && show b2 == "NONE" =  "(" ++ pprint n ++ ")" ++ " -~- " ++ pprint m 
          | show b1 == "ALL"  && show b2 == "NONE" =  "(" ++ pprint n ++ ")" ++ " +~- " ++ pprint m 
          | show b1 == "NONE" && show b2 == "ALL"  =  "(" ++ pprint n ++ ")" ++ " -~+ " ++ pprint m 
          | otherwise                              =  "(" ++ pprint n ++ ")" ++ " +~+ " ++ pprint m 
pprint _ = error "not a datatype"


toStr :: [[Char]] -> String
toStr []      = ""
toStr [x]     = str x
toStr (x:xs)  = str x ++ "," ++ toStr xs

str :: [Char] -> String 
str []     = ""
str [x]    = [x]
str (x:xs) = [x] ++ str xs


-- Haskell AST to string of Coq, needs a little more work

toStrCoq :: [[Char]] -> String
toStrCoq []      = ""
toStrCoq [x]     = "" ++ str x ++ ""
toStrCoq (x:xs)  = "" ++ str x ++ "," ++ toStr xs ++ ""

strCoq :: [Char] -> String 
strCoq []     = ""
strCoq [x]    = [x]
strCoq (x:xs) = [x] ++ str xs

charToString :: Char -> String
charToString c = [c]

quotes :: [Char]
quotes = charToString '"'

quote :: Char
quote = '"'

pprintCoq :: T -> String 
pprintCoq (ASPT n)  
          | show n == "CPY" = "ASPT CPY"
          | show n == "SIG" = "ASPT SIG"
          | show n == "HSH" = "ASPT HSH"
-- string needs quotes for coq, but cant have back slashes. currently does
pprintCoq (ASPT (ASPC n x)) = "ASPT (ASPC " ++ show n ++ " " ++ toStrCoq x ++ ")"
pprintCoq (LN n m) = "LN " ++ "(" ++ pprintCoq n ++ ") (" ++ pprintCoq m ++ ")"
pprintCoq (AT n m) = "AT " ++ show n ++ " (" ++ pprintCoq m ++ ")"
pprintCoq (BRS (b1,b2) n m) 
          | show b1 == "NONE" && show b2 == "NONE" =  "BRS NONE NONE (" ++ pprintCoq n ++ ") ("  ++ pprintCoq m ++ ")"
          | show b1 == "ALL"  && show b2 == "NONE" =  "BRS ALL NONE (" ++ pprintCoq n ++ ") (" ++ pprintCoq m ++ ")"
          | show b1 == "NONE" && show b2 == "ALL"  =  "BRS NONE ALL (" ++ pprintCoq n ++ ") (" ++ pprintCoq m ++ ")"
          | otherwise                              =  "BRS ALL ALL (" ++ pprintCoq n ++ ") (" ++ pprintCoq m ++ ")"
pprintCoq (BRP (b1,b2) n m) 
          | show b1 == "NONE" && show b2 == "NONE" =  "BRP NONE NONE (" ++ pprintCoq n ++ ") (" ++ pprintCoq m ++ ")"
          | show b1 == "ALL"  && show b2 == "NONE" =  "BRP ALL NONE (" ++ pprintCoq n ++ ") (" ++ pprintCoq m ++ ")"
          | show b1 == "NONE" && show b2 == "ALL"  =  "BRP NONE ALL (" ++ pprintCoq n ++ ") (" ++ pprintCoq m ++ ")"
          | otherwise                              =  "BRP ALL ALL (" ++ pprintCoq n ++ ") (" ++ pprintCoq m ++ ")"
pprintCoq _ = error "not a datatype"