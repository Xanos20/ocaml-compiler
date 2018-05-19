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
%token TINT       (* TInt *)
%token TBOOL      (* TBool *)

%token EOF

%start <Lang.exp> prog


%%

prog:
  | e=exp EOF { e }


typ:
| TINT  { TInt }
| TBOOL { TBool }

exp:
  | n=INT                                { VInt  n }
  | v=VAR                                { EVar  v }
  | b=BOOL                               { VBool b }
  | FUN v=VAR TInt ARROW e=exp
  | FUN v=VAR TBool ARROW e=exp
  | FUN v=VAR TFun ARROW e=exp
  |  e1=exp PLUS e2=exp                  { EAdd  (e1, e2) }
  |   e1=exp SUBT e2=exp                 { ESubt (e1, e2) }
  |  e1=exp MULT e2=exp                  { EMult (e1, e2) }
  |   e1=exp DIVD e2=exp                 { EDivd (e1, e2) }
  |  e1=exp LESSEQLS e2=exp              { ELessEqls (e1, e2) }
  |   e1=exp ISEQUAL e2=exp              { EIsEqual (e1, e2)  }
  |  IF e1=exp e2=exp e3=exp             { EIf   (e1, e2, e3) }
  |   LET v=VAR EQUALS e1=exp IN e2=exp  { ELet  (v, e1, e2) }
  |   e1=exp e2=exp                      { EFunCall (e1, e2) }
 (* | LPAREN FIX FUN v=VAR ARROW e1=exp              { ERecFun (e1, e2) }  *)
  |   e1=exp AND e2=exp                  { EAnd  (e1, e2) }
  |   e1=exp NAND e2=exp                 { ENand (e1, e2) }
  |   e1=exp OR e2=exp                   { EOr   (e1, e2) }
  |   e1=exp NOR e2=exp                  { ENor  (e1, e2) }
  |   e1=exp XOR e2=exp                  { EXor  (e1, e2) }
  |   e1=exp NXOR e2=exp                 { ENXor (e1, e2) }
  |   NOT e1=exp                         { ENot  (e1)     }
  |   LPAREN e1=exp RPAREN                     { e1 }
  |   n=INT                              {EVal (VInt n)}
  |   b=BOOL                             {EVal (VBool b)}
  |   FUN n=VAR ARROW                    { VFun  (t1, t2)  }
