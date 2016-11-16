(*pretty printing, should be attached after ast.*)


let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | ASSIGN -> "="
  | Mod -> "%"


let string_of_typ_a = function
    INT -> "int"
  | FLOAT -> "float"
  | COMPLEX -> "complex"

let string_of_typ = function
	Typ_a a-> string_of_typ_a a
  | BOOL -> "bool"
  | STRING -> "string"
  | POLY -> "poly"

 let string_of_typ_f =  function
 	Typ a-> string_of_typ a
  |	VOID-> "void"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"
  | Addone -> "++"
  | Subone -> "--"

let string_of_extra (a,b)=  "this is extra type" (* ********* *)

let string_of_p_c = function
    Intlit(i) -> string_of_int i
  | Floatlit(f) -> string_of_float f

let string_of_c (a,b)=  "<" ^ string_of_p_c a ^ "," ^ string_of_p_c b



let string_of_p_ap = function
    Prim_c(p_c) -> string_of_p_c  p_c
  | Comp (c, d) -> "this is a complex number" (* ********* *)
  | Extr(extr) -> "this is an extra" (* ********* *)

let string_of_primary= function
    Boolit(true) -> "true"
  | Boolit(false) -> "false"
  | Poly(p) -> "this is a poly" (* ********* *)
  | Strlit(p) -> p
  | Prim_ap(p_ap)-> string_of_p_ap p_ap

let string_of_lval = function
    Idasn(id) -> id
  | Polyasn(id,pl) -> id ^ string_of_int pl 

let rec string_of_expr = function
    Primary(prim) -> string_of_primary prim
  | ArrLit (arr) -> "this is an array" (* ********* *)
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Asn(v, e) -> string_of_lval v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  (*| If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ String.concat ")\n" (List.map string_of_stmt s)^ "\n"*) (* ********* *)
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ String.concat ")\n" (List.map string_of_stmt s1)
       ^  String.concat "else\n" (List.map string_of_stmt s2) ^ "\n"
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ String.concat "" (List.map string_of_stmt s) ^ "\n" 
  | Foreach(e1, s) -> "foreach (" ^ String.concat "" (List.map string_of_expr e1) ^ ")" ^
      " \n " ^ String.concat "" (List.map string_of_stmt s) ^ "\n"
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ String.concat "" (List.map string_of_stmt s) ^ "\n"



let rec string_of_vdecl = function  (* ********* *)
    Primdecl(a,b) -> string_of_typ a ^ b 
  | Primdecl_i (a,b,c) -> string_of_typ a ^ b ^ string_of_primary c
  | Arrdecl (a,b,c) -> string_of_typ a ^ b ^  string_of_int c
  | Arrdecl_i (a,b,c,d) -> string_of_typ a ^ b ^  string_of_int c ^ String.concat "" (List.map string_of_p_ap d)

let string_of_fdecl fdecl =
  string_of_typ_f fdecl.ftyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)