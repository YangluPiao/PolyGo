%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token LBRACKET RBRACKET LLBRACKET RRBRACKET
%token PLUS MINUS TIMES DIVIDE PLUSONE MINUSONE MODULUS LVB RVB ASSIGN
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR NOT
%token RETURN IF ELSE FOR FOREACH IN WHILE 
%token INT FLOAT BOOL COMPLEX POLY STRING VOID

%token <int> INTLIT
%token <float> FLOATLIT
%token <string> ID
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
   typ ID LPAREN formal_list_opt RPAREN LBRACE vdecl_list_opt stmt_list_rev RBRACE
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
	| POLY { Poly }
	| VOID { Void }

formal:
  	typ ID 		 							{ Prim_f_decl( $1, $2 ) }
  	| typ LBRACKET RBRACKET ID			{ Arr_f_decl( $1, $4) }

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
	| typ ID ASSIGN expr SEMI                   { Primdecl_i($1, $2, $4) }
	| typ LBRACKET INTLIT RBRACKET ID SEMI                           { Arrdecl($1, $5, $3) }
	| typ LBRACKET INTLIT RBRACKET ID ASSIGN LBRACKET expr_list_opt RBRACKET SEMI { Arrdecl_i($1, $5, $3, List.rev $8) }

stmt_list_rev:
	  stmt { [$1] }
	| stmt_list_rev stmt { $2 :: $1 }

stmt:
	expr SEMI { Expr $1 }
	| RETURN SEMI { Return Noexpr }
	| RETURN expr SEMI { Return $2 }
	| IF LPAREN expr RPAREN LBRACE stmt_list_rev RBRACE %prec NOELSE { If( $3, List.rev $6, [] ) }
	| IF LPAREN expr RPAREN LBRACE stmt_list_rev RBRACE ELSE LBRACE stmt_list_rev RBRACE { If( $3, List.rev $6, $10 ) }
	| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN LBRACE stmt_list_rev RBRACE { For($3, $5, $7, List.rev $10 ) }
	| FOREACH LPAREN ID IN ID RPAREN LBRACE stmt_list_rev RBRACE { Foreach($3, $5, List.rev $8) }
	| WHILE LPAREN expr RPAREN LBRACE stmt_list_rev RBRACE { While($3, List.rev $6) }

expr:
	INTLIT						{ Intlit( $1 ) }
	| FLOATLIT     				{ Floatlit( $1 ) }
	| LT expr COMMA expr GT		{ Complexlit( $2, $4 ) }
	| LBRACE expr_list_opt RBRACE  		{ Polylit( $2 ) }
	| FALSE            { Boollit( false ) }
	| TRUE             { Boollit( true ) }
	| STRINGLIT			{ Strlit( $1 ) }
	| extr_asn_value		   { Extr( $1 ) }
	 /* array, the whole array can be void, but any of the element cannot be void */
  	| LBRACKET expr_list_opt RBRACKET { Arrlit( $2 )}
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
	| LVB expr RVB 		{ Mod($2) }
	/*  one operand */
	| MINUS expr %prec NEG { Unop(Neg, $2) }
	| NOT expr         { Unop(Not, $2) }  
	| PLUSONE	expr	{ Unop( Addone, $2 ) }
	| MINUSONE expr { Unop( Subone, $2 ) }
	/* function call */
	| ID LPAREN expr_list_opt RPAREN { Call( $1, $3 ) }
	/* assignment */
	| extr_asn_value ASSIGN expr { Asn( $1, $3 ) }

extr_asn_value:/* value can be expressed by ID, ID[3] for array, ID[[3]] for poly */
	ID   	{ Id( $1 )}
	| ID LLBRACKET INTLIT RRBRACKET 		{ Polyextr( $1, $3 ) }/* for poly extraction */ /* assignment of poly coefficient */
	| ID LBRACKET INTLIT RBRACKET 	{ Arrextr( $1, $3 ) }/* for array extraction */ /* assignment of poly, array, int, float, bool, string, complex */

expr_list_opt:
		         { [] }
	| expr_list { List.rev $1 }
expr_opt:
				 { [] }
	| expr { [$1] }

expr_list:
	expr 		 { [$1] }
	| expr_list COMMA expr { $3 :: $1 }