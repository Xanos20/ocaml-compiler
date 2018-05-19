%{
open Lang
%}

%token <int> INT
%token <bool> BOOL
%token <string> VAR

%token LPAREN     (* ( *)
%token RPAREN     (* ) *)
%token PLUS       (* + *)
%token SUBT       (* - *)
%token MULT       (* * *)
%token DIVD       (* / *)
%token IF         (* if *)
%token LESSEQLS   (* <= *)
%token ISEQUAL    (* == *)
%token EQUALS     (* = *)
%token IN         (* in *)
%token LET        (* let *)
%token FUN        (* fun *)
%token FUNCALL    (* funcall *)
%token FIX        (* fix *)
%token ARROW      (* -> *)
%token AND        (* and *)
%token NAND       (* nand *)
%token OR         (* or *)
%token NOR        (* nor *)
%token XOR        (* xor *)
%token NXOR       (* nxor *)
%token NOT        (* not *)

%token EOF

%start <Lang.exp> prog


%%

prog:
  | e=exp EOF { e }

exp:
  | n=INT                                            { EInt  n }
  | v=VAR                                            { EVar  v }    
  | b=BOOL                                           { EBool b }
  | LPAREN  e1=exp PLUS e2=exp RPAREN                { EAdd  (e1, e2) }
  | LPAREN  e1=exp SUBT e2=exp RPAREN                { ESubt (e1, e2) }
  | LPAREN  e1=exp MULT e2=exp RPAREN                { EMult (e1, e2) }
  | LPAREN  e1=exp DIVD e2=exp RPAREN                { EDivd (e1, e2) }
  | LPAREN  e1=exp LESSEQLS e2=exp RPAREN            { ELessEqls (e1, e2) }
  | LPAREN  e1=exp ISEQUAL e2=exp RPAREN             { EIsEqual (e1, e2)  }
  | LPAREN  IF e1=exp e2=exp e3=exp RPAREN           { EIf   (e1, e2, e3) }                             
  | LPAREN  LET v=VAR EQUALS e1=exp IN e2=exp RPAREN { ELet  (v, e1, e2) }
  | LPAREN  FUN v=VAR ARROW e1=exp                   { EFun  (v, e1)  }
  | LPAREN  FUNCALL e1=exp e2=exp RPAREN             { EFunCall (e1, e2) }
 (* | LPAREN FIX FUN v=VAR ARROW e1=exp              { ERecFun (e1, e2) }  *)
  | LPAREN  e1=exp AND e2=exp RPAREN                 { EAnd  (e1, e2) }
  | LPAREN  e1=exp NAND e2=exp RPAREN                { ENand (e1, e2) }
  | LPAREN  e1=exp OR e2=exp RPAREN                  { EOr   (e1, e2) }
  | LPAREN  e1=exp NOR e2=exp RPAREN                 { ENor  (e1, e2) }
  | LPAREN  e1=exp XOR e2=exp RPAREN                 { EXor  (e1, e2) }
  | LPAREN  e1=exp NXOR e2=exp RPAREN                { ENXor (e1, e2) }
  | LPAREN  e1=exp NOT RPAREN                        { ENot  (e1)     }
  