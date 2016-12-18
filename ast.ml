type op = Add | Sub | Mult | Div | Modu | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or
type unop = Neg | Not | Addone | Subone | Sqrt
type typ = Int | Float | Complex | Bool | String | Void | Poly
type bind = typ * string


type expr = 
    Id of string
  | Extr of string * expr 
  | Intlit of int
  | Floatlit of float
  | Boollit of bool 
  | Strlit of string
  | Complexlit of float * float
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
  | While of expr * stmt 
  | Break

type formaldecl = 
   Prim_f_decl of typ * string 
  |Arr_f_decl of typ * string 

type variabledecl = 
   Primdecl of typ * string
  |Primdecl_i of typ * string * expr
  |Arr_poly_decl of typ * string * int
  |Arrdecl_i of typ * string * int * expr list 
  |Polydecl_i of typ * string * int * expr list
  |Arr_poly_decl_i of typ * string * int * string 

type functiondecl = 
  { 
  ftyp: typ;
  fname: string;
  formals: formaldecl list;
  locals: variabledecl list;
  body: stmt list;
  }

type program = variabledecl list * functiondecl list 

(* Pretty Printer! author: Jin Zhou *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Modu -> "%"
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
  | Sqrt -> "sqrt"

let string_of_typ = function
    Int -> "int"
  | Float -> "float"
  | Complex -> "complex"
  | Bool -> "bool"
  | String -> "string"
  | Poly -> "poly"
  | Void -> "void"

let rec string_of_expr = function
   Intlit(l) -> string_of_int l
  | Floatlit(f) -> string_of_float f
  | Id(s) -> s
  | Extr(s, e) -> s ^ "[[" ^ string_of_expr e ^ "]]"
  | Complexlit(a,b) ->  "<" ^ string_of_float a ^ "," ^ string_of_float b  ^ ">"
  | Boollit(true) -> "true"
  | Boollit(false) -> "false"
  | Polylit(p) -> "{" ^ String.concat "," (List.map string_of_expr p) ^ "}"
  | Strlit(p) -> p
  | Arrlit (arr) -> "[" ^ String.concat "," (List.map string_of_expr arr) ^ "]"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_unop o ^ "(" ^ string_of_expr e ^ ")"
  | Mod(e) -> "|" ^ string_of_expr e ^ "|"
  | Asn(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(e) -> string_of_expr e ^ ";\n";
  | Return(e) -> "return " ^ string_of_expr e ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s ^ "\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1
       ^  "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ")\n" ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | Break -> "break;\n"

let string_of_formaldecl = function
  Prim_f_decl(t, s) -> string_of_typ t ^ " " ^ s
  | Arr_f_decl(t, s) ->  string_of_typ t ^ " " ^ s ^ "[]"

let string_of_variabledecl = function
    Primdecl(a,b) -> string_of_typ a ^ " " ^ b ^ ";"
  | Primdecl_i (a,b,c) -> string_of_typ a ^ " " ^ b ^ " = " ^ string_of_expr c ^ ";"
  | Arr_poly_decl(a,b,c) -> string_of_typ a ^ " [" ^ string_of_int c ^ "]" ^ b ^ ";" 
  | Polydecl_i (a,b,c,d) -> string_of_typ a ^ " [" ^ string_of_int c ^ "]" ^ b ^ " = " ^ "{" ^ String.concat ", " (List.map string_of_expr d) ^ "}" ^ ";"
  | Arrdecl_i (a,b,c,d) -> string_of_typ a ^ " [" ^ string_of_int c ^ "]" ^ b ^ " = " ^ "[" ^ String.concat ", " (List.map string_of_expr d) ^ "]" ^ ";"

let string_of_fdecl fdecl =
  string_of_typ fdecl.ftyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_formaldecl fdecl.formals) ^
  ")\n{\n" ^
  String.concat "\n" (List.map string_of_variabledecl fdecl.locals) ^ "\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "\n" (List.map string_of_variabledecl (List.rev vars)) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl (List.rev funcs)) 