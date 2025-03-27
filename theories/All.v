From CoplandParser Require Import Parser Lexer Parser_QC.

Global Instance Stringifiable_Term : Stringifiable Term.
refine (Build_Stringifiable _ Printer.term_to_string Lexer.from_string _).
intros a.
pose proof (qc_parsed_terms_match a).
unfold parse_succeeds in *.
destruct (from_string (Printer.term_to_string a)) eqn:Heq; try congruence; ff; eq_crush.
Qed.