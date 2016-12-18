%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token LBRACKET RBRACKET LLBRACKET RRBRACKET 
%token PLUS MINUS TIMES DIVIDE PLUSONE MINUSONE MODULUS VB ASSIGN SQRT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR NOT
%token RETURN IF ELSE FOR WHILE PASS BREAK
%token INT FLOAT BOOL COMPLEX POLY STRING VOID

%token <int> INTLIT
%token <float> FLOATLIT
%token <string> ID
%token <string> STRINGLIT
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc VB
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MODULUS
%right PLUSONE MINUSONE
%right NOT NEG

%start program
%type <Ast.program> program

%%

/* array and poly element can only be: complex, int, float --primary_ap, typ 
array is initiated in the beggging, and remain unchanged, that means the value of the element can be extracted,
	but cannot be assigned or changed. Size must be indicated */
program:
	decls EOF { $1 }

decls:
   /* nothing */ { [], [] }
	| decls vdecl { ($2 :: fst $1), snd $1 }
	| decls fdecl { fst $1, ($2 :: snd $1) }

fdecl:/*??? no void type for fdecl */
   typ ID LPAREN formal_list_opt RPAREN LBRACE vdecl_list_opt stmt_list_opt RBRACE
     {{ ftyp = $1;
	 fname = $2;
	 formals = $4;
	 locals = List.rev $7;
	 body = List.rev $8  }}

typ: /* primary type */
	  INT { Int }
	| FLOAT { Float }
	| COMPLEX { Complex }
	| BOOL { Bool }
	| STRING { String }
	| VOID { Void }
	| POLY { Poly }

formal:
  	  typ ID 		 				    { Prim_f_decl( $1, $2 ) }
  	| typ LBRACKET RBRACKET ID			{ Arr_f_decl( $1, $4) }

formal_list:
	formal                  { [ $1 ] }
	| formal_list COMMA formal { $3 :: $1 }

formal_list_opt:
    /* nothing */ { [] }
	| formal_list   {  $1 }

vdecl_list_opt:
	/* nothing */	{ [] }
	| vdecl_list_opt vdecl 			{$2 :: $1}

vdecl:
	typ ID SEMI                                     { Primdecl($1, $2) }
	| typ ID ASSIGN expr SEMI                   { Primdecl_i($1, $2, $4) }
	| typ LBRACKET INTLIT RBRACKET ID SEMI                           { Arr_poly_decl($1, $5, $3) }
	| typ LBRACKET INTLIT RBRACKET ID ASSIGN LBRACKET expr_list_opt RBRACKET SEMI { Arrdecl_i($1, $5, $3, List.rev $8) }
	| typ LBRACKET INTLIT RBRACKET ID ASSIGN LBRACE expr_list_opt RBRACE SEMI { Polydecl_i( $1, $5, $3, List.rev $8) }
	| typ LBRACKET INTLIT RBRACKET ID ASSIGN ID SEMI {Arr_poly_decl_i($1,$5,$3,$7)}

stmt_list_opt:
	PASS SEMI       {[]}
  | stmt_list  { $1 }

  stmt_list:
    stmt  { [$1] }
  | stmt_list stmt { $2 :: $1 }

stmt:
	expr SEMI { Expr $1 }
	| RETURN SEMI { Return Noexpr }
	| RETURN expr SEMI { Return $2 }
	| LBRACE stmt_list_opt RBRACE { Block(List.rev $2) }
	| IF LPAREN expr RPAREN stmt %prec NOELSE { If( $3, $5, Block([]) ) }
	| IF LPAREN expr RPAREN stmt ELSE stmt { If( $3,  $5, $7 ) }
	| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt { For($3, $5, $7, $9 ) }
	| WHILE LPAREN expr RPAREN stmt { While($3, $5) }
	| BREAK SEMI             { Break }


expr:
	  INTLIT				    { Intlit( $1 ) }
	| ID 								 { Id($1) }
	| ID LLBRACKET expr RRBRACKET 		{ Extr( $1, $3 ) }

	| FLOATLIT     				{ Floatlit( $1 ) }
	| STRINGLIT					{ Strlit( $1 ) }
	| FALSE            { Boollit( false ) }
	| TRUE             { Boollit( true ) }
	| LT FLOATLIT COMMA FLOATLIT GT	   { Complexlit( $2, $4 ) }
	| LBRACE expr_list_opt RBRACE  		{ Polylit($2 ) }
	 /* array, the whole array can be void, but any of the element cannot be void */
  	| LBRACKET expr_list_opt RBRACKET { Arrlit($2 )}
	| LPAREN expr RPAREN { $2 }
	/* Binop */
	| expr PLUS   expr { Binop($1, Add,   $3) }
	| expr MINUS  expr { Binop($1, Sub,   $3) }
	| expr TIMES  expr { Binop($1, Mult,  $3) }
	| expr DIVIDE expr { Binop($1, Div,   $3) }
	| expr EQ     expr { Binop($1, Equal, $3) }
	| expr NEQ    expr { Binop($1, Neq,   $3) }
	| expr LT     expr { Binop($1, Less,  $3) }
	| expr LEQ    expr { Binop($1, Leq,   $3) }
	| expr GT     expr { Binop($1, Greater, $3) }
	| expr GEQ    expr { Binop($1, Geq,   $3) }
	| expr AND    expr { Binop($1, And,   $3) }
	| expr OR     expr { Binop($1, Or,    $3) }
	| expr MODULUS expr { Binop($1, Modu,    $3) }
	| VB expr VB 		{ Mod($2) }
	/*  one operand */
	| MINUS expr %prec NEG { Unop(Neg, $2) }
	| NOT expr         { Unop(Not, $2) }  
	| PLUSONE	expr	{ Unop( Addone, $2 ) }
	| MINUSONE expr { Unop( Subone, $2 ) }
	| SQRT LPAREN expr RPAREN {Unop(Sqrt,$3)}
	/* function call */
	| ID LPAREN expr_list_opt RPAREN { Call( $1, $3 ) }
	/* assignment */
	| expr ASSIGN expr { Asn( $1, $3 ) }


/* expr_list_opt:
		         { [] }
	| expr_list {  List.rev $1 }
expr_opt:
				 { Noexpr }
	| expr { $1 }

expr_list:
	expr 		 { [$1] }
	| expr_list COMMA expr { $3 :: $1 }
	*/
expr_list_opt:
	  expr_opt { [$1] }
	| expr_list_opt COMMA expr_opt { $3 :: $1 }

expr_opt:
				 { Noexpr }
	| expr { $1 }