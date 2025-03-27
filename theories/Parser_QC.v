From QuickChick Require Import QuickChick.
From CoplandParser Require Import Parser Lexer Printer.

From CoplandSpec Require Import Term_Defs_Core Term_Defs_Core_Typeclasses.

Global Instance EqClass_impl_Dec_Eq {A} `{EqClass A} : Dec_Eq A := {
  dec_eq := EqClass_impl_DecEq _
}.

Local Open Scope string_scope.

Definition ex_json_1 : json :=
  JSON__Object [("name", JSON__String "foo"); ("count", JSON__Number 42)].
Definition ex_json_2 : json :=
  JSON__Object [("bd", JSON__String "fhdhds"); ("yes", JSON__True)].
Definition ex_json_3 : json :=
  JSON__Object [("bd", JSON__String "fhdhds"); ("yes", JSON__False)].
Definition ex_json_4 : json :=
  JSON__Object [("rec", JSON__Object [
    ("rec2", JSON__Object [
      ("final", JSON__String "yes");
      ("final2", JSON__False)
    ])
  ]); ("yes", JSON__False)].

Global Instance Gen_json : Gen json.
econstructor.
eapply (elems_ ex_json_1 [ex_json_1; ex_json_2; ex_json_3; ex_json_4]).
Qed.

Global Instance Shrink_json : Shrink json := {
  shrink := fun j => 
    if (EqClass.eqb j ex_json_1)
    then nil
    else ex_json_1 :: nil
}.
Global Instance Arbitrary_json : Arbitrary json.
Qed.
Global Instance Show_json : Show json := {
  show := Printer.to_string
}.

Ltac kill_false := let C := fresh "CONTRA" in
                   intro C; inversion C; congruence.

Ltac adec := try (left; reflexivity); try (right; kill_false).

Ltac qinv H := inversion H; subst; clear H.

(* Creating Generators *)


#[local]
Instance showAscii : Show (ascii) :=
{
  show a := (String a EmptyString) 
}.

(******************************************************************)
(* Extra General Purpose Generators *)
(******************************************************************)

(** Lowercase ascii *)
Definition ascii_from_range (min max : nat) : G ascii := 
  bind (choose (min,max)) (fun val =>
    ret (ascii_of_nat val)).

Definition correct_ascii_from_range (min max : nat) : ascii -> bool :=
  fun x => let x' : nat := nat_of_ascii x in
  andb (Nat.leb min x') (Nat.leb x' max).

Definition genLower : G ascii := ascii_from_range 97 122.
Definition isLowerCorrect : ascii -> bool := correct_ascii_from_range 97 122.
QuickChick (forAll (genLower) isLowerCorrect).

(** Digits *)
Definition genDigits : G ascii := ascii_from_range 48 57.
Definition isDigitsCorrect : ascii -> bool := correct_ascii_from_range 48 57.
QuickChick (forAll (genDigits) isDigitsCorrect).

(** Underscores *)
Definition genUnderScore : G ascii := ascii_from_range 95 95.
Definition isUnderScoreCorrect : ascii -> bool := correct_ascii_from_range 95 95.
QuickChick (forAll (genUnderScore) isUnderScoreCorrect).

(** Uppercase *)
Definition genUpper : G ascii := ascii_from_range 65 90.
Definition isUpperCorrect : ascii -> bool := correct_ascii_from_range 65 90.
QuickChick (forAll (genUpper) isUpperCorrect).

(** ID chars *)
Definition genIdChar : G ascii :=
  oneOf_ genLower [genLower; genUpper; genDigits; genUnderScore].
Definition genIdCharCorrect (x : ascii) : bool :=
  orb ((correct_ascii_from_range 97 122) x)
    (orb ((correct_ascii_from_range 48 57) x) 
      (orb ((correct_ascii_from_range 95 95) x) ((correct_ascii_from_range 65 90) x)
      )
    ).

QuickChick (forAll (genIdChar) genIdCharCorrect).

(** Symbols *)
Fixpoint genSymbolTail (sz : nat) : G string :=
  match sz with
  | 0 => ret ""
  | S sz' => freq_     (x <- genIdChar ;;
          ret (String x EmptyString)) [
    (1,   x <- genIdChar ;;
          ret (String x EmptyString));
    (sz,  h <- genIdChar ;;
          t <- genSymbolTail sz' ;;
          ret (String h t))
    ]
  end.
Definition genSymbol : G string :=
  h <- genLower ;;
  tailSize <- choose (0,20) ;; 
  (* NOTE: We enforce a size limit here, it is questionable
           if we can really justify this as arbitrary Gthen.
           But, I have faith that if it can work for any string length 1-21
           the parser will work for any string. the buck has to stop somewhere
           might as well be here *)
  t <- genSymbolTail tailSize ;;
  ret (String h t).

Fixpoint shrinkSymbolTail (s : string) : list (string) :=
  match s with
  | EmptyString => []
  | String h t => [t] ++ (map (fun t' => (String h t')) (shrinkSymbolTail t))
  end.

(* Only difference is that here, we enforce keeping the first letter 
  (as it might be the only lower-case letter) *)
Definition shrinkSymbol (s : string) : list (string) :=
  match s with
  | EmptyString => []
  | String h t => (map (fun t' => (String h t')) (shrinkSymbolTail t))
  end.

Global Instance Gen_string : Gen string := {
  arbitrary := genSymbol
}.
Global Instance Shrink_string : Shrink string := {
  shrink := shrinkSymbol
}.
Global Instance Arbitrary_string : Arbitrary string. Qed.

(******************************************************************)
(******************************************************************)
(******************************************************************)
(*               Starting Copland Generators                      *)
(******************************************************************)
(******************************************************************)
(******************************************************************)

Derive (Arbitrary, Show) for SP.
Derive (Arbitrary, Show) for ASP_PARAMS.
Derive (Arbitrary, Show) for ASP.
Derive (Arbitrary, Shrink, Show) for Term.

(** Testing *)

Fixpoint term_seq_size (t : Term) : nat :=
  match t with
  | asp _ => 0
  | att _ t' => 1 + term_seq_size t'
  | lseq t1 t2 => 1 + term_seq_size t1 + term_seq_size t2
  | bseq sp t1 t2 => 1 + term_seq_size t1 + term_seq_size t2
  | bpar sp t1 t2 => 1 + term_seq_size t1 + term_seq_size t2
  end.

Definition parse_succeeds (a : Term) :=
  match Lexer.from_string (Printer.term_to_string a) with
  | inr a' => if (EqClass.eqb a a') then true else false
  | _ => false
  end.

(* Extract Constant defNumTests   => "100000". *)
(* Extract Constant defSize        => "7". *)
Extract Constant defNumShrinks => "0".

(* QuickChick parse_succeeds'. *)

(* Conjecture all_terms_parseable : forall (a : Term), (parse_succeeds a) = true.
QuickChick all_terms_parseable. *)

(* Extract Constant defNumTests => "100000". 
It passes this, but seems excessive for everytime we run
*)
Extract Constant defSize => "10".

Conjecture qc_parsed_terms_match : forall (a : Term), parse_succeeds a = true.
QuickChick qc_parsed_terms_match.
