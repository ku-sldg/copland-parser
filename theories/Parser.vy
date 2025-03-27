%{
From CoplandSpec Require Export Term_Defs_Core.
From JSON Require Import JSON.

From Coq Require Extraction.
From Coq Require Export
     List.
Export
  ListNotations.
Open Scope list_scope.
%}

%token TRUE FALSE NULL COLON COMMA EOF
%token LBRACE RBRACE BANG HASH HOOK STAR AT LBRACKET RBRACKET LPAREN RPAREN 
  DASH PLUS LANGLE RANGLE TILDE

%token<Z>      NUMBER
%token<string> NAME

%start<Term> p_term_full

%type<json>      p_element
%type<list json> p_elements p_array
%type<string * json>        p_member
%type<list (string * json)> p_members p_object

%type<ASP> p_asp
%type<SP> p_op
%type<Term> p_term0
%type<Term> p_term1
%type<Term> p_term2

%%

p_element :
  LBRACE p_object RBRACE  { JSON__Object $2 }
| LBRACKET p_array RBRACKET { JSON__Array  $2 }
| NAME            { JSON__String $1 }
| NUMBER          { JSON__Number $1 }
| TRUE            { JSON__True  }
| FALSE           { JSON__False }
| NULL            { JSON__Null  }

p_array :
             { [] }
| p_elements { $1 }

p_elements :
  p_element { [$1] }
| p_element COMMA p_elements { $1 :: $3 }

p_object :
            { [] }
| p_members { $1 }

p_members :
  p_member { [$1] }
| p_member COMMA p_members { $1 :: $3 }

p_member :
  NAME COLON p_element { ($1, $3) }

p_asp :
  LBRACE RBRACE { Term_Defs_Core.NULL }
| NAME p_element NAME NAME { ASPC (asp_paramsC $1 $2 $3 $4) }
| BANG { SIG }
| HASH { HSH }
| HOOK { APPR }
| STAR NAME { ENC $2 }

p_op :
| DASH { NONE }
| PLUS { ALL }

p_term2 :
| LPAREN p_term0 RPAREN { $2 }
| p_asp { asp $1 }
| AT NAME p_term2 { att $2 $3 }
| AT NAME LBRACKET p_term0 RBRACKET { att $2 $4 }

p_term1 :
| p_term2 { $1 }
| p_term1 p_op LANGLE p_op p_term2 { bseq ($2, $4) $1 $5 }
| p_term1 p_op TILDE p_op p_term2 { bpar ($2, $4) $1 $5 }

p_term0 :
| p_term1 { $1 }
| p_term1 DASH RANGLE p_term0 { lseq $1 $4 }

p_term_full : p_term0 EOF { $1 }