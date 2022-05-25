--quickCheck (\t -> checkSame (parsePhrase (pprint t)) t)

module CoplandQC where

import CoplandParser
import CoplandLang
import PrettyPrinter
import Utils

--import QuickCheck
--import System.Random
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

genInt :: Gen Int
genInt = chooseInt (0,100)

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
  do oneof [genCpy, genHsh, genSig, genNull, genAspc]

genParen :: Int -> Gen T
genParen n = do ph <- genT n
                return (PAREN ph)

genAt :: Int -> Gen T
genAt n =
  do pl <- genPlace
     ph <- genT n
     return (AT pl ph)

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
  do oneof [genASPT, genParen (n-1), genLn (n-1), genAt (n-1), genBrs (n-1), genBrp (n-1)]

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

checkTerms :: IO ()
checkTerms = quickCheckWith stdArgs {maxSuccess = 100} quickCheck_parsable

testCoq_Trans :: IO [String]
testCoq_Trans = do  let x = sample' (genT 10)
                    x' <- x
                    let x'' = map (\x' -> printCoq x') (map (\x' -> (COP_PHRASE x')) (x'))
                    let filt = filter (\x -> if x == Nothing then False else True) x''
                    let mapped = map (\x -> do (s,e) <- x 
                                               return s) filt
                    let mapped2 = map (\x -> fromJust x) mapped
                    return mapped2

out_test = do let file = "coq_test.v"
              out <- testCoq_Trans
              let out' = foldl (\x -> \a -> x ++ "\nCompute " ++ a ++ ".") "" out
              writeFile file (out')


checkParser :: Int -> IO ()
checkParser n = quickCheckWith stdArgs {maxSuccess = n} involutiveAST