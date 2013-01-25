%token <int> NUMBER
%token <float> FLOAT
%token <string> SYMBOL
%token <string> STRING
%token <bool> BOOLEAN
%token DOT
%token QUOTE
%token RPAREN
%token LPAREN

%start main
%type <Creme.creme> main

%%
main: sexp { $1 }
    ;

sexp: atom       { $1 }
    | pair       { $1 }
    | QUOTE sexp { Creme.Quoted $2 }
    ;

pair: LPAREN sexps RPAREN          { $2 }
    ;

sexps: sexp sexps    { Creme.Pair ($1, $2) }
     | sexp DOT sexp { Creme.Pair ($1, $3) }
     |               { Creme.Empty }
     ;

atom: NUMBER  { Creme.Number $1 }
    | FLOAT   { Creme.Float $1 }
    | BOOLEAN { Creme.Boolean $1 }
    | STRING  { Creme.String $1 }
    | SYMBOL  { Creme.Symbol $1 }
    ;
%%
