%{
open Lang
%}

%token <int> INT

%token LPAREN     (* ( *)
%token RPAREN     (* ) *)
%token TRUE
%token FALSE
%token PLUS       (* + *)
%token SUBT       (* - *)
%token MULT       (* * *)
%token DIVD       (* / *)
%token IF         (* if *)
%token LESSEQLS   (* <= *)


%token EOF

%start <Lang.exp> prog


%%

prog:
  | e=exp EOF { e }

exp:
  | n=INT                                  { EInt n }
  | LPAREN TRUE RPAREN                     { ETrue  }
  | LPAREN FALSE RPAREN                    { EFalse }
  | LPAREN  e1=exp PLUS e2=exp RPAREN      { EAdd  (e1, e2) }
  | LPAREN  e1=exp SUBT e2=exp RPAREN      { ESubt (e1, e2) }
  | LPAREN  e1=exp MULT e2=exp RPAREN      { EMult (e1, e2) }
  | LPAREN  e1=exp DIVD e2=exp RPAREN      { EDivd (e1, e2) }
  | LPAREN  e1=exp LESSEQLS e2=exp RPAREN  { ELessEqls (e1, e2) }
  | LPAREN  IF e1=exp e2=exp e3=exp RPAREN { EIf   (e1, e2, e3) }
  
  