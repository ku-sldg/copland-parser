--{-# LANGUAGE GADTs, FlexibleContexts #-}

module PrettyPrinter where
import CoplandLang
--import Data.List
import Data.String

transAST_SP_Cop :: SP -> String
transAST_SP_Cop ALL = "+"
transAST_SP_Cop NONE = "-"

transAST_PLC_Cop :: PLACE -> String
transAST_PLC_Cop (PLC sym) = sym

--Haskell AST to String
transAST_T_Cop :: T -> String 
transAST_T_Cop (ASPT n)  
          | show n == "CPY" = "_"
          | show n == "SIG" = "!"
          | show n == "HSH" = "#"
          | show n == "NULL"  = "{}"
transAST_T_Cop (ASPT (SPS s1 plc s2)) = s1 ++ " " ++ transAST_PLC_Cop plc ++ " " ++ s2
transAST_T_Cop (LN n m) = transAST_T_Cop n ++ " -> " ++ transAST_T_Cop m
transAST_T_Cop (AT n m) = "@" ++ transAST_PLC_Cop n ++ " " ++ transAST_T_Cop m
transAST_T_Cop (AT_S n m) = "@" ++ transAST_PLC_Cop n ++ " [" ++ transAST_T_Cop m ++ "]"
transAST_T_Cop (BRS (b1,b2) n m) = transAST_T_Cop n ++ " " ++ transAST_SP_Cop b1 ++ "<" ++                                   transAST_SP_Cop b2 ++ " " ++ transAST_T_Cop m
transAST_T_Cop (BRP (b1,b2) n m) = transAST_T_Cop n ++ " " ++ transAST_SP_Cop b1 ++ "~" ++                                   transAST_SP_Cop b2 ++ " " ++ transAST_T_Cop m
transAST_T_Cop (PAREN phr)  = "(" ++ transAST_T_Cop phr ++ ")"

