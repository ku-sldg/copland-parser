{-  Copland language definition.
    
   -Term(T):  Copland Phrase AST.
-}

module CoplandLang where

{- Identify Places (protocol participants) -}
type Pl = Int
{-  Arguments to measurement commands -}
type ARG = String
{- Identify ASP (Attestation Service Provider) -}
type ASP_ID = Int

{-  Evidence splitting functions.
    ALL-  keep all evidence
    NONE- keep no evidence -}
data SP = ALL | NONE
        deriving (Read,Show)

-- Primitive Measurement Term.
data ASP
  = CPY
  | SIG
  | HSH
  | ASPC ASP_ID [ARG]
  deriving (Read,Show)
  
-- Copland Term.
data T
  = ASPT ASP
  | AT Pl T
  | LN T T
  | BRS (SP,SP) T T
  | BRP (SP,SP) T T  
  deriving (Read,Show)
