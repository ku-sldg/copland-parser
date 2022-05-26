module Utils where
import CoplandLang
import CoplandParser

-- Translates each seen symbol String -> Nat representing that symbol
type PlaceMap   = [(String, Int)]

type ASP_SymMap     = [(String, Int)]
type TARG_SymMap     = [(String, Int)]

type Env        = (PlaceMap, ASP_SymMap, TARG_SymMap, Int)

printASPT_Coq           :: Env -> ASP -> Maybe (String, Env)
printASPT_Coq e n
            | show n == "CPY" = return ("CPY", e)
            | show n == "SIG" = return ("SIG", e)
            | show n == "HSH" = return ("HSH", e)
            | show n == "NULL" = Nothing -- TODO: Actually implement NULL in Coq def
printASPT_Coq e (SPS s1 plc s2) = do    (s1', e') <- printASym_Coq e s1
                                        (plc', e'') <- printPlace_Coq e' plc
                                        (s2', e''') <- printTSym_Coq e'' s2
                                        -- Default args here are 'nil' = empty arg list
                                        return ("(ASPC (asp_paramsC " ++ s1' ++ " nil " ++ plc' ++ " " ++ s2' ++ "))", e''')

printASym_Coq                    :: Env -> SYMBOL -> Maybe (String, Env)
printASym_Coq (pM,asM,tsM,i) sym        = case lookup sym asM of
                                                -- new index is length, add at spot
                                                Nothing -> return ("asp_" ++ show i, (pM,[(sym, i)] ++ asM,tsM, i+1))
                                                Just x -> return ("asp_" ++ show x, (pM,asM, tsM,i))

printTSym_Coq                    :: Env -> SYMBOL -> Maybe (String, Env)
printTSym_Coq (pM,asM,tsM,i) sym        = case lookup sym tsM of
                                                -- new index is length, add at spot
                                                Nothing -> return ("asp_" ++ show i, (pM,asM,[(sym, i)] ++ tsM, i+1))
                                                Just x -> return ("asp_" ++ show x, (pM,asM,tsM,i))


printPlace_Coq                          :: Env -> PLACE -> Maybe (String, Env)
printPlace_Coq (pM,asM,tsM,i) (PLC sym)  = case lookup sym pM of
                                                -- new index is length, add at spot
                                                Nothing -> return ("asp_" ++ show i, ([(sym, i)] ++ pM, asM, tsM, i+1))
                                                Just x -> return ("asp_" ++ show x, (pM,asM,tsM,i))

-- Translates Copland Phrase AST -> Coq String
printPhrase_Coq         :: Env -> T -> Maybe (String, Env)
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

mapFn :: (String, Int) -> String
mapFn (x,y) = "asp_" ++ show y

-- Translates Copland AST -> Coq String
printCopland_Coq        :: Env -> COPLAND -> Maybe String
printCopland_Coq env (STAR plc term) = Nothing -- This is a bad case - No Coq def yet
printCopland_Coq env (COP_PHRASE phr) = do      (r,(pM,asM,tsM,i)) <- printPhrase_Coq env phr
                                                let tsStr = (foldl (\x -> \a -> x ++ "Definition " ++ a ++ " : TARG_ID. Admitted.\n") "" (map mapFn tsM))
                                                let asStr = (foldl (\x -> \a -> x ++ "Definition " ++ a ++ " : ASP_ID. Admitted.\n") "" (map mapFn asM))
                                                let pStr = (foldl (\x -> \(a,b) -> x ++ "Definition asp_" ++ show b ++ " : Plc := " ++ show b ++ ".\n") "" pM)
                                                return (asStr ++ tsStr ++ pStr ++ "Definition cop_phrase : Term := " ++ r ++ ".\n")

printCoq            :: COPLAND -> Maybe String -- Only adds default empty env
printCoq            = printCopland_Coq ([],[],[],0)

transCop_Coq        :: String -> String
transCop_Coq str    = case printCoq (parseCop str) of
                           Just x -> x
                           Nothing -> error $ "error failed to print"

transCop_Coq_IO str             = putStrLn (transCop_Coq str)