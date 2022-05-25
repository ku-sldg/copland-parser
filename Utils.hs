module Utils where
import CoplandLang
import CoplandParser

-- Translates each seen symbol String -> Nat representing that symbol
type PlaceMap = [(String, Int)]


printASPT_Coq           :: PlaceMap -> ASP -> Maybe (String, PlaceMap)
printASPT_Coq e n
            | show n == "CPY" = return ("CPY", e)
            | show n == "SIG" = return ("SIG", e)
            | show n == "HSH" = return ("HSH", e)
            | show n == "NULL" = Nothing
-- string needs quotes for coq, but cant have back slashes. currently does
-- TODO: This Coq translation is certainly not correct yet
printASPT_Coq e (SPS s1 plc s2) = Nothing -- "(ASPC " ++ show s1 ++ " " ++ show plc ++ " " ++ show s2 ++ ")"



printPlace_Coq          :: PlaceMap -> PLACE -> Maybe (String, PlaceMap)
printPlace_Coq e (PLC sym)  = case lookup sym e of
                                                -- new index is length, add at spot
                                    Nothing -> return (show (length e), [(sym, (length e))])
                                    Just x -> return (show x, e)

-- Translates Copland Phrase AST -> Coq String
printPhrase_Coq         :: PlaceMap -> T -> Maybe (String, PlaceMap)
printPhrase_Coq env (ASPT asp)      = do    (asp',e') <- printASPT_Coq env asp
                                            return ("(asp " ++ asp' ++ ")", e')
printPhrase_Coq env (AT plc phr)    = do    (plc',e') <- printPlace_Coq env plc
                                            (r,e'') <- printPhrase_Coq e' phr
                                            return ("(att " ++ plc' ++ " " ++ r ++ ")", e'')
printPhrase_Coq env (AT_S plc phr)  = do    (plc',e') <- printPlace_Coq env plc
                                            (r,e'') <- printPhrase_Coq e' phr
                                            return ("(att " ++ plc' ++ " " ++ r ++ ")", e'')
printPhrase_Coq env (LN phr1 phr2)  = do    (phr1',e') <- printPhrase_Coq env phr1
                                            (phr2', e'') <- printPhrase_Coq e' phr2
                                            return ("(lseq " ++ phr1' ++ " " ++ phr2' ++ ")", e'')
printPhrase_Coq env (BRS s phr1 phr2)
                                    = do    (phr1',e') <- printPhrase_Coq env phr1
                                            (phr2', e'') <- printPhrase_Coq e' phr2
                                            return ("(bseq " ++ show s ++ " " ++ phr1' ++ " " ++ phr2' ++ ")", e'')
printPhrase_Coq env (BRP s phr1 phr2)
                                    = do    (phr1',e') <- printPhrase_Coq env phr1
                                            (phr2', e'') <- printPhrase_Coq e' phr2
                                            return ("(bseq " ++ show s ++ " " ++ phr1' ++ " " ++ phr2' ++ ")", e'')
printPhrase_Coq env (PAREN phr)     = do    (phr1', e') <- printPhrase_Coq env phr
                                            return ("(" ++ phr1' ++ ")", e')

-- Translates Copland AST -> Coq String
printCopland_Coq        :: PlaceMap -> COPLAND -> Maybe (String, PlaceMap)
printCopland_Coq env (STAR plc term) = Nothing -- This is a bad case
printCopland_Coq env (COP_PHRASE phr) = do  (r,e'') <- printPhrase_Coq env phr
                                            return (r,e'')

printCoq            :: COPLAND -> Maybe (String, PlaceMap)
printCoq            = printCopland_Coq []

transCop_Coq        :: String -> Maybe (String, PlaceMap)
transCop_Coq str    = printCoq (parseCop str)
