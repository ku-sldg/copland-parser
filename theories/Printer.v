From CoplandParser Require Export Parser Lexer.
From RocqCandy Require Export All.
From JSON Require Import Printer.
From CoplandSpec Require Import Term_Defs_Core.

Definition quote (s : string) : string := """" ++ s ++ """".
Definition parens (s : string) : string := "(" ++ s ++ ")".

Definition asp_to_string (a : ASP) : string :=
  match a with
  | NULL => "{}"
  | ASPC (asp_paramsC aid args tplc tid) => 
    String.concat " " [quote aid; Printer.to_string args; quote tplc; quote tid]
  | SIG => "!"
  | HSH => "#"
  | APPR => "?"
  | ENC p => "* " ++ quote p
  end.

Definition sp_to_string (s : SP) : string :=
  match s with
  | ALL => "+"
  | NONE => "-"
  end.

Fixpoint term_to_string (t : Term) : string :=
  match t with
  | asp a => asp_to_string a
  | att p t => "@ " ++ quote p ++ " [" ++ term_to_string t ++ "]"
  | lseq t1 t2 => parens (term_to_string t1 ++ " -> " ++ term_to_string t2)
  | bseq (s1, s2) t1 t2 => parens (term_to_string t1) ++ " " ++ sp_to_string s1 ++ "<" ++ sp_to_string s2 ++ " " ++ parens (term_to_string t2)
  | bpar (s1, s2) t1 t2 => parens (term_to_string t1) ++ " " ++ sp_to_string s1 ++ "~" ++ sp_to_string s2 ++ " " ++ parens (term_to_string t2)
  end.
