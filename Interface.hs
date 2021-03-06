module CoplandTools where
import CoplandLang (COPLAND)
import Utils (transCop_Coq_IO, printCoq)
import CoplandParser (parseCop)
import CoplandQC (out_test, checkParser, checkTC, checkSIG_HSH)
import CoplandTypeChecker

----- Utilities -----
translate_Cop_Coq :: String -> IO ()
translate_Cop_Coq = transCop_Coq_IO

parse_to_AST :: String -> COPLAND
parse_to_AST = parseCop

pta = parse_to_AST

print_AST :: COPLAND -> String
print_AST x = case (printCoq x) of
                Just x -> x
                Nothing -> error $ "error failed to print"

-- Compilation error printing help function
compErr :: String -> IO ()
compErr s = putStrLn ("Compilation Error : " ++ s)

-- Overall Copland Compiler from COPLAND Phrase String -> Coq Output
compileCopland :: String -> IO ()
compileCopland copStr = do{ let ast = parse_to_AST copStr
                            -- Run basic type-checking 
                            ; let (tcB,tcM) = typeCheckCop ast
                            ; if (tcB == False) 
                                -- Print type check error message
                                then (compErr tcM)
                                -- run the sig-hash check
                                else (
                                    do { let (cB,cM) = checkSH ast
                                        ; if (cB == False)
                                             -- Print check sign-hash error message
                                             then (compErr cM)
                                             -- Success!
                                             else ( case (printCoq ast) of
                                                        Just x -> (putStrLn x)
                                                        Nothing -> compErr "Failed to print Coq"
                                                    )
                                    }
                                )
                            }
                            
----- Testing -----

-- Creates an example copland phrase and translates it to Coq
--      Arg 1 : String  := controls output file
--      Arg 2 : Nat     := controls the complexity of the generated phrase
generate_example_coq :: String -> Int -> IO ()
generate_example_coq = out_test

-- Runs QuickCheck on the parser, generating copland than ensuring it parses
--      Arg 1 : Nat     := controls how many tests to run
check_parser :: Int -> IO ()
check_parser = checkParser 

csh = \x -> checkSH (pta x)