(*
From Coq Require Import Strings.String.
From Coq Require Import Strings.Ascii.
From Coq Require Import Lists.List. Import ListNotations.

*)
From Coq Require Import Strings.String.

Inductive SP : Type :=
  | ALL 
  | NONE.
 
Inductive ASP : Type :=
  | CPY
  | SIG
  | HSH
  | ASPC (ASP_ID : nat) (Arg : string).


Inductive T : Type :=
  | ASPT (a : ASP)
  | AT (Pl : nat) (t : T)
  | LN (t1 t2 : T)
  | BRS (s1 s2 : SP) (t1 t2 : T)
  | BRP (s1 s2 : SP) (t1 t2 : T).


Delimit Scope string_scope with string.
Local Open Scope string_scope.

Check ASPT (CPY).
Check AT (123) (AT 45 (ASPT CPY)).
Check BRP ALL ALL (ASPT CPY) (ASPT HSH).
Check AT 2 (AT 3 (BRS NONE ALL (ASPT (ASPC 11 bank)) (ASPT SIG))).
Check AT 1 (AT 2 (BRS NONE ALL (ASPT HSH) (LN (ASPT SIG) (AT 3 (ASPT CPY))))).