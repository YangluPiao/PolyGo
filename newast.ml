type decltyp = Primary | Array
type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | ASSIGN
type unop = Neg | Not | Addone | Subone
type typ_a = INT | FLOAT | COMPLEX
type typ = Typ_a of tpy_a | BOOL | STRING | POLY 
type typ_f = Typ of typ | VOID
type bind = typ * string

type primary_c = 
	Intlit of int
	| Floatlit of float

type extravalue = 
	Id of string 
	Polyextr of string * int
	Arrextr of string * int 

type primary_ap = 
	Prim_c of primary_c
	| Comp of primary_c * primary_c
	| Extr of extravalue

type primary = 
	Prim_ap of primary_ap
	| Poly of primary_ap list 
	| Boolit of bool 
	| Strlit of string 

type lvalue = 
	Idasn of string
	| Polyasn of string * int 

type expr = 
	Primary of primary
	| ArrLit of primary_ap list
	| Binop of expr * op * expr
	| Unop of unop * expr
	| Asn of lvalue * expr
	| Call of string * expr list
	| Noexpr 

type stmt =
	Block of stmt list
	| Expr of expr
	| Return of expr
	| If of expr * stmt list * stmt list
	| For of expr * expr * expr * stmt list
	| Foreach of expr list * stmt list 
	| While of expr * stmt list

type variabledecl = 
	Primdecl of typ * string
	Primdecl_i of typ * string * primary
	Arrdecl of typ * string * int
	Arrdecl_i of typ * string * int primary_ap list 

type functiondecl = 
	{ 
	ftyp: typ_f;
	fname: string;
	formals: bind list;
	locals: bind list;
	body: stmt list;
	}

type program = variabledecl list * functiondecl list 