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
  PPP NPP PPN NPN PSP NSP PSN NSN RARROW


%token<Z>      NUMBER
%token<string> NAME

%start<Term> p_term

%type<json>      p_element
%type<list json> p_elements p_array
%type<string * json>        p_member
%type<list (string * json)> p_members p_object
%type<ASP> p_asp
%type<Term> p_term_extra
%type<Term> p_term_term

%type<Term> p_branch

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

p_term_term :
| p_asp { asp $1 }

p_branch : 
| p_term_extra PPP p_term_term { bpar (ALL, ALL) $1 $3 }
| p_term_extra NPP p_term_term { bpar (NONE, ALL) $1 $3 }
| p_term_extra PPN p_term_term { bpar (ALL, NONE) $1 $3 }
| p_term_extra NPN p_term_term  { bpar (NONE, NONE) $1 $3 }
| p_term_extra PSP p_term_term  { bseq (ALL, ALL) $1 $3 }
| p_term_extra NSP p_term_term  { bseq (NONE, ALL) $1 $3 }
| p_term_extra PSN p_term_term  { bseq (ALL, NONE) $1 $3 }
| p_term_extra NSN p_term_term  { bseq (NONE, NONE) $1 $3 }

p_term_extra :
| p_term_term { $1 }
| AT NAME LBRACKET p_term_extra RBRACKET { att $2 $4 }
| p_term_extra RARROW p_term_term { lseq $1 $3 }
| p_branch { $1 }
| LPAREN p_term_extra RPAREN { $2 }

p_term : p_term_extra EOF { $1 }