--quickCheck (\t -> checkSame (parsePhrase (pprint t)) t)

module CoplandQC where

import CoplandParser
import CoplandLang
import PrettyPrinter
import Utils
import CoplandTypeChecker

--import QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Function
import Test.QuickCheck.Monadic
import Data.Maybe

genCpy :: Gen T
genCpy = return (ASPT CPY)
genHsh :: Gen T
genHsh = return (ASPT HSH)
genSig :: Gen T
genSig = return (ASPT SIG)
genNull :: Gen T
genNull = return (ASPT NULL)

genInt :: Int -> Gen Int
genInt max = chooseInt (0,max)

genChar :: Gen Char
genChar = elements ['a'..'z']

genString :: Gen String 
genString = listOf genChar

genDig :: Gen SYMBOL
genDig = do dig <- chooseInt (0, 1000000)
            return ("p" ++ (show dig))

genSym :: Gen SYMBOL
genSym = do first <- genChar
            rem <- listOf (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['_']))
            return ([first] ++ rem)

genPlace :: Gen PLACE
genPlace = do plc <- oneof [genSym, genDig]
              return (PLC plc)

genAspc :: Gen T
genAspc = 
  do sym1 <- genSym
     place <- genPlace
     sym2 <- genSym
     return (ASPT (SPS sym1 place sym2))

genAllB :: Gen SP
genAllB = return ALL 
genNoneB :: Gen SP 
genNoneB = return NONE

genASPT :: Gen T
genASPT =
  do oneof [genCpy, genHsh, genSig, genAspc] -- TODO: Null not in Coq yet genNull] 

genParen :: Int -> Gen T
genParen n = do ph <- genT n
                return (PAREN ph)

genAt :: Int -> Gen T
genAt n =
  do pl <- genPlace
     ph <- genT n
     return (AT pl ph)

genAt_S :: Int -> Gen T
genAt_S n =
  do pl <- genPlace
     ph <- genT n
     return (AT_S pl ph)

genLn :: Int -> Gen T
genLn n =
  do p1 <- genT3 n -- Cannot have recursive left arrow
     p2 <- genT2 n
     return (LN p1 p2)

genBrs :: Int -> Gen T
genBrs n =
  do b1 <- do oneof [genAllB, genNoneB]
     b2 <- do oneof [genAllB, genNoneB]
     p1 <- genT2 n
     p2 <- genT2 n
     return (BRS (b1, b2) p1 p2)

genBrp :: Int -> Gen T
genBrp n =
  do b1 <- do oneof [genAllB, genNoneB]
     b2 <- do oneof [genAllB, genNoneB]
     p1 <- genT2 n -- need to not have recursive left branch
     p2 <- genT n
     return (BRP (b1, b2) p1 p2)


genT :: Int -> Gen T
genT 0 =
  do genASPT
genT n = 
  do oneof [genASPT, genParen (n-1), genLn (n-1), genAt (n-1), genAt_S (n-1), genBrs (n-1), genBrp (n-1)]

genT2 :: Int -> Gen T
genT2 0 =
  do genASPT
genT2 n = 
  do oneof [genASPT, genParen (n-1), genLn (n-1)]

genT3 :: Int -> Gen T
genT3 0 =
  do genASPT
genT3 n = 
  do oneof [genASPT, genParen (n-1)]

instance Arbitrary T where
  arbitrary = sized $ \n -> genT (rem n 10)

testCoq_Trans :: Int -> IO String
testCoq_Trans n = do  let x = generate (genT n)
                      x' <- x
                      case printCoq (COP_PHRASE x') of
                        Nothing -> error $ "failure to print " ++ show x'
                        Just x -> return x

-- Creates an example copland phrase and translates it to Coq
--      f controls output file
--      n controls the complexity of the generated phrase
out_test f n = do out <- testCoq_Trans n
                  writeFile f (out_test_coq_defs ++ out)


checkParser :: Int -> IO ()
checkParser n = quickCheckWith stdArgs {maxSuccess = n} involutiveAST

checkTC :: Int -> IO ()
checkTC n = quickCheckWith stdArgs {maxSuccess = n} quickTC

checkSIG_HSH :: Int -> IO ()
checkSIG_HSH n = quickCheckWith stdArgs {maxSuccess = n} quickSIG_HSH

out_test_coq_defs :: String
out_test_coq_defs = "Definition Plc: Set := nat.\nDefinition N_ID: Set := nat.\nDefinition Event_ID: Set := nat.\nDefinition ASP_ID: Set. Admitted.\nDefinition TARG_ID: Set. Admitted.\nDefinition Arg: Set. Admitted.\nInductive ASP_PARAMS: Set :=\n| asp_paramsC: ASP_ID -> (list Arg) -> Plc -> TARG_ID -> ASP_PARAMS.\nInductive Evidence: Set :=\n| mt: Evidence\n| uu: (*ASP_PARAMS ->*) ASP_PARAMS ->\n      (*Evidence ->*) Plc -> Evidence -> Evidence\n| gg: Plc -> Evidence -> Evidence\n| hh: Plc -> Evidence -> Evidence\n| nn: N_ID -> Evidence\n| ss: Evidence -> Evidence -> Evidence\n| pp: Evidence -> Evidence -> Evidence.\nInductive ASP: Set :=\n| CPY: ASP\n| ASPC: ASP_PARAMS -> ASP\n| SIG: ASP\n| HSH: ASP.\nInductive SP: Set :=\n| ALL\n| NONE.\nDefinition Split: Set := (SP * SP).\nInductive Term: Set :=\n| asp: ASP -> Term\n| att: Plc -> Term -> Term\n| lseq: Term -> Term -> Term\n| bseq: Split -> Term -> Term -> Term\n| bpar: Split -> Term -> Term -> Term.\n"