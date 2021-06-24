--{-# LANGUAGE GADTs, FlexibleContexts #-}

module PrettyPrinter where
import CoplandLang
--import Data.List
import Data.String

--Haskell AST to String
pprintCop :: T -> String 
pprintCop (ASPT n)  
          | show n == "CPY" = "_"
          | show n == "SIG" = "!"
          | show n == "HSH" = "#"
          | show n == "MT"  = "{}"
pprintCop (ASPT (ASPC n x)) = "ASPC " ++ show n ++ " " ++ toStr x
pprintCop (LN n m) = pprintCop n ++ " -> " ++ pprintCop m
pprintCop (AT n m) = "@ " ++ show n ++ " " ++ pprintCop m
pprintCop (BRS (b1,b2) n m) 
          | show b1 == "NONE" && show b2 == "NONE" = "(" ++ pprintCop n ++ " -<- "  ++ pprintCop m ++ ")"
          | show b1 == "ALL"  && show b2 == "NONE" = "(" ++ pprintCop n ++ " +<- " ++ pprintCop m ++ ")"
          | show b1 == "NONE" && show b2 == "ALL"  = "(" ++ pprintCop n ++ " -<+ " ++ pprintCop m ++ ")"
          | otherwise                              = "(" ++ pprintCop n ++ " +<+ " ++ pprintCop m ++ ")"
pprintCop (BRP (b1,b2) n m) 
          | show b1 == "NONE" && show b2 == "NONE" = "(" ++ pprintCop n ++ " -~- " ++ pprintCop m ++ ")"
          | show b1 == "ALL"  && show b2 == "NONE" = "(" ++ pprintCop n ++ " +~- " ++ pprintCop m ++ ")"
          | show b1 == "NONE" && show b2 == "ALL"  = "(" ++ pprintCop n ++ " -~+ " ++ pprintCop m ++ ")"
          | otherwise                              = "(" ++ pprintCop n ++ " +~+ " ++ pprintCop m ++ ")"
pprintCop _ = error "not a datatype"


toStr :: [[Char]] -> String
toStr []      = ""
toStr [x]     = str x
toStr (x:xs)  = str x ++ "," ++ toStr xs

str :: [Char] -> String 
str []     = ""
str [x]    = [x]
str (x:xs) = [x] ++ str xs
