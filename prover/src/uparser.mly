%{
  open Types
%}

%token EOF

%token <string> IDENT

(* keywords *)
%token FUN
%token FST SND
%token TRUE
%token LEFT RIGHT
%token CASE
%token OF
%token ABSURD
%token ONE ZERO
%token NATZERO SUCC TNAT

(* symbols *)
%token LPAR RPAR
%token LBRAC RBRAC
%token TARROW ARROW
%token LAND LOR
%token COMMA
%token COLON
%token NOT
%token BAR

(* priorities *)

%right COLON
%left  COMMA
%right ARROW TARROW
%right LOR
%right LAND
%nonassoc NOT

%start term
%start type_only
%type <Types.term> term
%type <Types.ty> type_only
%type <Types.ty> ttype

%%

term: term_ EOF { $1 };
type_only: ttype EOF { $1 };

term_:
| LPAR RPAR                   { Unit }
| LPAR term_ RPAR             { $2 }
| NATZERO                     { Zero }
| SUCC term_                  { Succ $2 }
| IDENT                       { Var $1 }
| IDENT term_                 { App (Var $1, $2) }
| LPAR term_ RPAR term_       { App ($2, $4) }
| LPAR term_ COMMA term_ RPAR { Pair ($2, $4) }
| FUN LPAR s=IDENT COLON ty=ttype RPAR ARROW t=term_
                              { Abs (s, ty, t) }
| FST term_                   { Fst $2 }
| SND term_                   { Snd $2 }
| LEFT LBRAC ty=ttype RBRAC t=term_  { Left (ty, t) }
| RIGHT LBRAC ty=ttype RBRAC t=term_ { Right (ty, t) }
| CASE LPAR t1=term_ COMMA t2=term_ COMMA t3=term_ RPAR { Case (t1, t2, t3) }
| CASE LBRAC ty=ttype RBRAC t=term_ { Case_ (ty, t) }
;

ttype:
| ZERO               { T0 }
| TNAT                { TNat }
| NOT ttype          { TArrow ($2, T0) }
| ONE                { T1 }
| LPAR ttype RPAR    { $2 }
| IDENT              { TVar $1 }
| ttype TARROW ttype { TArrow ($1, $3) }
| ttype COMMA ttype  { TPair ($1, $3) }
| ttype LAND ttype   { TPair ($1, $3) }
| ttype LOR ttype    { TPlus ($1, $3) }
;
