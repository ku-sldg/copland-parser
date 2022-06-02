module CoplandTypeChecker where
import CoplandLang
import PrettyPrinter

-- Sequences the type checking operations so the first fail halts
(-->) :: (Bool, String) -> (Bool, String) -> (Bool, String)
x --> y = case x of
             (True, _) -> case y of
                            (True, _) -> (True, "")
                            (False, s) -> (False, s)
             (False, s) -> (False,s)

typeCheckPlc :: PLACE -> (Bool,String)
typeCheckPlc (PLC sym) = (True, "")

typeCheckASP :: ASP -> (Bool,String)
typeCheckASP _ = (True, "")

-- TODO: Can obfuscate a TERMINAL using parens
typeCheckT :: T -> (Bool, String)
typeCheckT (LN f s)     = case f of
                            (ASPT x) -> typeCheckT f --> (False, "Cannot have sequence starting with '" ++ transAST_T_Cop (ASPT x) ++ "' ")
                            _ -> typeCheckT f --> (case s of
                                                    (ASPT x) -> typeCheckT s --> (True, "")
                                                    (LN f' s') -> typeCheckT s
                                                    s' -> (False, "Sequence must end with an ASPT terminal, not '" ++ transAST_T_Cop s' ++ "' "))
typeCheckT (AT plc t)   = typeCheckPlc plc --> typeCheckT t
typeCheckT (AT_S plc t) = typeCheckPlc plc --> typeCheckT t
typeCheckT (BRS (s1,s2) t1 t2)
                        = typeCheckT t1 --> typeCheckT t2
typeCheckT (BRP (s1,s2) t1 t2)
                        = typeCheckT t1 --> typeCheckT t2
typeCheckT (PAREN t)    = typeCheckT t
typeCheckT (ASPT asp)   = typeCheckASP asp

typeCheckCop :: COPLAND -> (Bool,String)
typeCheckCop (STAR plc t) = typeCheckPlc plc --> typeCheckT t
typeCheckCop (COP_PHRASE t) = typeCheckT t
typeCheckCop (COMMENT s) = (True, "")

tc = typeCheckCop