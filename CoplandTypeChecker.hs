module CoplandTypeChecker where
import CoplandLang
import PrettyPrinter

-- If we have a        (Sign, Hash)
checkSign_Hash                      :: (Bool, Bool) -> T -> (Bool, Bool)
                                    -- If we have not               or possibly already failed
checkSign_Hash (s,h) (ASPT SIG)     = if s == False then (True, False) else (s, h)
checkSign_Hash (s,h) (ASPT HSH)     = (s, True)
checkSign_Hash (s,h) (ASPT _)       = (s,h)

checkSign_Hash (s,h) (AT plc t)     = checkSign_Hash (s,h) t
checkSign_Hash (s,h) (AT_S plc t)   = checkSign_Hash (s,h) t

checkSign_Hash (s,h) (LN f sec)     = do let (sf, hf) = (checkSign_Hash (s,h) f)
                                         if (sf == True && hf == True)
                                             then (True, True)
                                             else checkSign_Hash (sf,hf) sec

checkSign_Hash (s,h) (BRS (sp1,sp2) f sec)
                                                -- Only check if evidence passed
                                    = case sp1 of
                                        ALL -> case sp2 of
                                                ALL -> -- Check both
                                                    case (checkSign_Hash (s,h) f) of
                                                        (True, True) -> (True,True)
                                                        (s',h') -> checkSign_Hash (s,h) sec
                                                NONE -> -- Check only sp1 with above, reset below
                                                    case (checkSign_Hash (s,h) f) of
                                                        (True, True) -> (True,True)
                                                        (s',h') -> checkSign_Hash (False,False) sec
                                        NONE -> case sp2 of
                                                ALL -> -- Check only sp2 with above, reset below
                                                    case (checkSign_Hash (False,False) f) of
                                                        (True, True) -> (True,True)
                                                        (s',h') -> checkSign_Hash (s,h) sec
                                                NONE -> -- Reset for both
                                                    case (checkSign_Hash (False,False) f) of
                                                        (True, True) -> (True,True)
                                                        (s',h') -> checkSign_Hash (False,False) sec

checkSign_Hash (s,h) (BRP (sp1,sp2) f sec)
                                                -- Only check if evidence passed
                                    = case sp1 of
                                        ALL -> case sp2 of
                                                ALL -> -- Check both
                                                    case (checkSign_Hash (s,h) f) of
                                                        (True, True) -> (True,True)
                                                        (s',h') -> checkSign_Hash (s,h) sec
                                                NONE -> -- Check only sp1 with above, reset below
                                                    case (checkSign_Hash (s,h) f) of
                                                        (True, True) -> (True,True)
                                                        (s',h') -> checkSign_Hash (False,False) sec
                                        NONE -> case sp2 of
                                                ALL -> -- Check only sp2 with above, reset below
                                                    case (checkSign_Hash (False,False) f) of
                                                        (True, True) -> (True,True)
                                                        (s',h') -> checkSign_Hash (s,h) sec
                                                NONE -> -- Reset for both
                                                    case (checkSign_Hash (False,False) f) of
                                                        (True, True) -> (True,True)
                                                        (s',h') -> checkSign_Hash (False,False) sec

checkSign_Hash (s,h) (PAREN t)      = checkSign_Hash (s,h) t

checkSH :: COPLAND -> (Bool,String)
checkSH (COP_PHRASE t) = case (checkSign_Hash (False, False) t) of
                            (True, True) -> (False,"Failed - SIG irrecoverable due to HSH")
                            (_, _) -> (True,"Passed")
                            -- TODO: Implement the equivalent in Coq someday
checkSH (STAR plc t) = (False, "'* place : phrases' are not allowed currently \n\t\t\tPlease just specify the phrase")

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
typeCheckASP NULL = (False, "Coq equivalent of NULL does not exist")
typeCheckASP _ = (True, "")

typeCheckT :: T -> (Bool, String)
typeCheckT (LN f s)     = typeCheckT f --> typeCheckT s
typeCheckT (AT plc t)   = typeCheckPlc plc --> typeCheckT t
typeCheckT (AT_S plc t) = typeCheckPlc plc --> typeCheckT t
typeCheckT (BRS (s1,s2) t1 t2)
                        = (not ((s1 == NONE) && (s2 == NONE)), "'-<-' Causes evidence erasure") --> typeCheckT t1 --> typeCheckT t2
typeCheckT (BRP (s1,s2) t1 t2)
                        = (not ((s1 == NONE) && (s2 == NONE)), "'-~-' Causes evidence erasure") --> typeCheckT t1 --> typeCheckT t2
typeCheckT (PAREN t)    = typeCheckT t
typeCheckT (ASPT asp)   = typeCheckASP asp

typeCheckCop :: COPLAND -> (Bool,String)
typeCheckCop (STAR plc t) = typeCheckPlc plc --> typeCheckT t
typeCheckCop (COP_PHRASE t) = typeCheckT t
typeCheckCop (COMMENT s) = (True, "")

tc = typeCheckCop

quickTC         :: T -> Bool
quickTC s       = case typeCheckT (s) of
                        (True,_) -> True
                        (False,mess) -> error $ show (transAST_T_Cop s, mess)

quickSIG_HSH    :: T -> Bool
quickSIG_HSH s  = case (checkSH (COP_PHRASE s)) of
                    (False,s') -> error $  show (transAST_T_Cop s, s')
                    (True, s') -> True