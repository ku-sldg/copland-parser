

module CoplandQC where

import CoplandLang
import PrettyPrinter

--import QuickCheck
--import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Function
import Test.QuickCheck.Monadic



instance Arbitrary T where
  arbitrary = sized $ \n -> genT (rem n 10)
    
genCpy :: Gen T
genCpy = return (ASPT CPY)
genHsh :: Gen T
genHsh = return (ASPT HSH)
genSig :: Gen T
genSig = return (ASPT SIG)

genChar :: Gen Char
genChar = elements ['a'..'z']

genString :: Gen String 
genString = listOf genChar

genAspc :: Gen T
genAspc = 
  do id <- choose (1,100)
     arg <- listOf genString
     return (ASPT (ASPC id arg))

genAllB :: Gen SP
genAllB = return ALL 
genNoneB :: Gen SP 
genNoneB = return NONE

genASPT :: Gen T
genASPT =
  do oneof [genCpy, genHsh, genSig, genAspc]


genAt :: Int -> Gen T
genAt n =
  do pl <- choose (1, 100)
     ph <- genT n
     return (AT pl ph)

genLn :: Int -> Gen T
genLn n =
  do p1 <- genT3 n
     p2 <- genT3 n
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
     p1 <- genT2 n
     p2 <- genT2 n
     return (BRP (b1, b2) p1 p2)


genT :: Int -> Gen T
genT 0 =
  do genASPT
genT n = 
  do oneof [genASPT, genLn (n-1), genAt (n-1), genBrs (n-1), genBrp (n-1)]

genT2 :: Int -> Gen T
genT2 0 =
  do genASPT
genT2 n = 
  do oneof [genASPT, genLn (n-1), genBrs (n-1), genBrp (n-1)]

genT3 :: Int -> Gen T
genT3 0 =
  do genASPT
genT3 n = 
  do oneof [genASPT, genLn (n-1)]

{-
testEval :: Int -> IO ()
testEval n = quickCheckWith stdArgs {maxSuccess = n}
  (\t -> checkSame (interp (pprintCop t)) (evalT t))

-}