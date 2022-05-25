{-  Copland language definition.
    
   -Term(T):  Copland Phrase AST.
-}

module CoplandLang where

{-  Evidence splitting functions.
    ALL-  keep all evidence
    NONE- keep no evidence -}
data SP = ALL | NONE
        deriving (Read,Show,Eq)

type SYMBOL = String

data PLACE = PLC SYMBOL
  deriving (Read,Show,Eq)

-- Primitive Measurement Term.
data ASP
  = CPY
  | SIG
  | HSH
  | NULL
  -- | ASPC ASP_ID [ARG]
  | SPS SYMBOL PLACE SYMBOL
  deriving (Read,Show,Eq)
  
-- Copland Term.
data T
  = ASPT ASP
  | AT PLACE T
  | AT_S PLACE T
  | LN T T
  | BRS (SP,SP) T T
  | BRP (SP,SP) T T
  | PAREN T
  deriving (Read,Show,Eq)

data COPLAND = STAR PLACE T
              | COP_PHRASE T
              | COMMENT String
                deriving (Read,Show,Eq)
