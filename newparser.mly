%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token LBRACKET RBRACKET LLBRACKET RRBRACKET
%token PLUS MINUS TIMES DIVIDE PLUSONE MINUSONE MODULUS VB ASSIGN 
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR NOT
%token RETURN IF ELSE FOR FOREACH IN WHILE 
%token INT FLOAT BOOL VOID COMPLEX POLY STRING

%token <int> INTLIT
%token <float> FLOATLIT
%token <string> ID
/* ? */
%token <string> STRINGLIT
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%left COMMA
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MODULUS
%left ADDONE MINUSONE
%right NOT NEG

%start program
%type <Ast.program> program

%%

/* array and poly element can only be: complex, int, float --primary_ap, typ_a 
array is initiated in the beggging, and remain unchanged, that means the value of the element can be extracted,
	but cannot be assigned or changed. Size must be indicated */
program:
	decls EOF { $1 }

decls:
   /* nothing */ { [], [] }
	| decls vdecl { ($2 :: fst $1), snd $1 }
	| decls fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   typ_f ID LPAREN formal_list_opt RPAREN LBRACE vdecl_list_opt stmt_list_rev RBRACE
     { typ = $1;
	 fname = $2;
	 formals = $4;
	 locals = List.rev $7;
	 body = List.rev $8  }

typ_f: /* primary type with void for function returns */
	typ { Typi( $1 ) }
	| VOID { Void }

typ: /* primary type */
	typ_a  { Typ_a( $1 ) }
	| BOOL { Bool }
	| STRING { String }
	| POLY { Poly }

typ_a: /* for array type */
	INT { Int }
	| FLOAT { Float }
	| COMPLEX { Comp }

formal:
  	typ ID 		 							{ ( $1,$2, Prim ) }
  	| typ_a LBRACKET INTLIT RBRACKET ID			{ ($1,$3, Array) }

formal_list:
	formal                  { [ $1 ] }
	| formal_list COMMA formal { $3 :: $1 }

formal_list_opt:
    /* nothing */ { [] }
	| formal_list   { List.rev $1 }

vdecl_list_opt:
			{ [] }
	| vdecl_list_opt vdecl 			{$2 :: $1}

vdecl:
	typ ID SEMI                                     { Primdecl($1, $2) }
	| typ ID ASSIGN primary SEMI                   { Primdecl_i($1, $2, $4) }
	| typ_a LBRACKET INTLIT RBRACKET ID SEMI                           { Arrdecl($1, $5, $3) }
	| typ_a LBRACKET INTLIT RBRACKET ID ASSIGN LBRACKET primary_ap_list_opt RBRACKET SEMI { Arrdecl_i($1, $5, $3, List.rev $8) }
/* */

stmt_list_rev:
	stmt { $1 }
	| stmt_list_rev stmt { $2 :: $1 }

stmt:
	expr SEMI { Expr $1 }
	| RETURN SEMI { Return Noexpr }
	| RETURN expr SEMI { Return $2 }
	| IF LPAREN expr RPAREN LBRACE stmt_list_rev RBRACE %prec NOELSE { If( $3, List.rev $6, [] ) }
	| IF LPAREN expr RPAREN LBRACE stmt_list_rev RBRACE ELSE LBRACE stmt_list_rev RBRACE { If( $3, List.rev $6, $10 ) }
	| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN LBRACE stmt_list_rev RBRACE { For($3, $5, $7, List.rev $10 ) }
	| FOREACH ID IN expr_list LBRACE stmt_list_rev RBRACE { Foreach($4, List.rev $6) }
	| WHILE LPAREN expr RPAREN LBRACE stmt_list_rev RBRACE { While($3, List.rev $6) }

expr:
	primary						{ Prim( $1 ) }
	 /* array, the whole array can be void, but any of the element cannot be void */
  	| LBRACKET primary_ap_list_opt RBRACKET { Arrlit( $2 )}
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
	| expr MODULUS expr { Binop($1, Mod,    $3) }
	/*  one operand */
	| MINUS expr %prec NEG { Unop(Neg, $2) }
	| NOT expr         { Unop(Not, $2) }  
	| PLUSONE	expr	{ Unop( Addone, $2 ) }
	| MINUSONE expr { Unop( Subsone, $2 ) }
	/* function call */
	| ID LPAREN expr_list_opt RPAREN { Call( $1, $3 ) }
	/* assignment */
	| extr_asn_value ASSIGN expr { Asn( $1, $3 ) }

primary_list:
	| primary	{ [ $1 ] }
	| primary_list COMMA primary { $3 :: $1 }

primary: /* the primary types */
	primary_ap 			   { Prim_ap( $1 ) }
	| LBRACE primary_ap_list_opt RBRACE  		{ Poly( $2 ) }
	| FALSE            { BoolLit( false ) }
	| TRUE             { BoolLit( true ) }
	| STRINGLIT			{ Strlit( $1 ) }

primary_ap_list_opt:
			{ [] }
	| primary_ap_list { List.rev $1 }

primary_ap_list:
	primary_ap  { [ $1 ] }
	| primary_ap_list COMMA primary_ap { $3 :: $1 }

primary_ap: /* what can exist in an array or in poly */
	primary_c  	{ Prim_c( $1 ) }
	  /* complex, real and im can all be void, then take it as 0 */
	| LT primary_c COMMA primary_c GT			{ Comp( $2, $4 ) }
	| extr_asn_value		   { Extr( $1 ) }

extr_asn_value:/* value can be expressed by ID, ID[3] for array, ID[[3]] for poly */
	ID   	{ Id( $1 )}
	| ID LLBRACKET INTLIT RRBRACKET 		{ Poly_extr_asn( $1, $3 ) }/* for poly extraction */ /* assignment of poly coefficient */
	| ID LBRACKET INTLIT RBRACKET 	{ Arr_extr_asn( $1, $3 ) }/* for array extraction */ /* assignment of poly, array, int, float, bool, string, complex */

primary_c_opt:
		{ Noexpr }
	| primary_c { Prim_c( $1 ) }

primary_c: /* what can exist in a complex */
	INTLIT						{ Intlit( $1 ) }
	| FLOATLIT     				{ Floatlit($1) }

expr_list_opt:
		         { [] }
	| expr_list_opt { List.rev $1 }

expr_list:
	expr 		 { [$1] }
	| expr_list COMMA expr { $3 :: $1 }

expr_opt:
	             { Noexpr }
	| expr 		 { $1 }