type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or
type unop = Neg | Not | Addone | Subone | Sqrt
type typ = Int | Float | Complex | Bool | String | Void | Poly
type bind = typ * string

(* type extra_asn_value = 
  Id of string 
  | Polyextr of string * int
  | Arrextr of string * int
  | Poly_id of string * string
  | Array_id of string * string *)


type expr = 
    Id of string
  | Extr of string * expr 
  | Intlit of int
  | Floatlit of float
  | Boollit of bool 
  | Strlit of string
  | Complexlit of expr * expr
  | Polylit of expr list 
  | Arrlit of expr list 
  | Binop of expr * op * expr
  | Unop of unop * expr
  | Asn of expr * expr
  | Call of string * expr list
  | Mod of expr
  | Noexpr 



type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt* stmt
  | For of expr * expr * expr * stmt 
  | Foreach of string * string * stmt 
  | While of expr * stmt 
  | Break

type formaldecl = 
   Prim_f_decl of typ * string 
  |Arr_f_decl of typ * string 

type variabledecl = 
   Primdecl of typ * string
  |Primdecl_i of typ * string * expr
  |Arrdecl of typ * string * int
  |Arrdecl_i of typ * string * int * expr list 

type functiondecl = 
  { 
  ftyp: typ;
  fname: string;
  formals: formaldecl list;
  locals: variabledecl list;
  body: stmt list;
  }

type program = variabledecl list * functiondecl list 
(*
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_unop = function
    Neg -> "-"
  | Not -> "!"
  | Addone -> "++"
  | Subone -> "--" 

let string_of_typ = function
    Int -> "int"
  | Float -> "float"
  | Complex -> "complex"
  | Bool -> "bool"
  | String -> "string"
  | Poly -> "poly"
  | Void -> "void"

let string_of_extra_asn_value = function
    Id(s) -> s
  | Polyextr(s, i) -> s ^ "[[" ^ string_of_int i ^ "]]"
  | Arrextr(s, i) -> s ^ "[" ^ string_of_int i ^ "]"

let rec string_of_expr = function
    Boollit(true) -> "true"
  | Boollit(false) -> "false"
  | Polylit(p) -> "{" ^ String.concat "," (List.map string_of_expr p) ^ "}"
  | Strlit(p) -> p
  | Intlit(l) -> string_of_int l
  | Floatlit(f) -> string_of_float f
  | Complexlit(a,b) ->  "<" ^ string_of_expr a ^ "," ^ string_of_expr b  ^ ">"
  | Extr(e) ->  string_of_extra_asn_value e
  | Arrlit (arr) -> "[" ^ String.concat "," (List.map string_of_expr arr) ^ "]"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_unop o ^ string_of_expr e
  | Mod(e) -> "|<" ^ string_of_expr e ^ ">|"
  | Asn(v, e) -> string_of_extra_asn_value v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "\n}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  (*| If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ String.concat ")\n" (List.map string_of_stmt s)^ "\n"*) (* ********* *)
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n{\n" ^ String.concat "\n" (List.map string_of_stmt s1) ^ "}\n"
       ^  "else\n{\n" ^ String.concat "\n" (List.map string_of_stmt s2) ^ "}\n"
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ")\n{\n" ^ String.concat "" (List.map string_of_stmt s) ^ "}\n" 
  | Foreach(s1, s2, s) -> "foreach (" ^ s1 ^ "in" ^ s2 ^ ")\n{" ^
      "\n" ^ String.concat "" (List.map string_of_stmt s) ^ "}\n"
  | While(e, s) -> "while (" ^ string_of_expr e ^ ")\n{\n" ^ String.concat "" (List.map string_of_stmt s) ^ "}\n"

let string_of_formaldecl = function
  Prim_f_decl(t, s) -> string_of_typ t ^ " " ^ s
  | Arr_f_decl(t, s) ->  string_of_typ t ^ " " ^ s ^ "[]"

let rec string_of_variabledecl = function
    Primdecl(a,b) -> string_of_typ a ^ " " ^ b ^ ";\n"
  | Primdecl_i (a,b,c) -> string_of_typ a ^ " " ^ b ^ " = " ^ string_of_expr ^ ";\n"
  | Arrdecl (a,b,c) -> string_of_typ a ^ " [" ^ string_of_int c ^ "]" ^ b ^ ";\n"
  | Arrdecl_i (a,b,c,d) -> string_of_typ a ^ " [" ^ string_of_int c ^ "]" ^ b ^ " = " ^ "[" ^ String.concat "," (List.map string_of_expr d) ^ "]" ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.ftyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_formaldecl fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_variabledecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_variabledecl (List.rev vars)) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl (List.rev funcs)) *)
