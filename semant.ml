(* Author: Jin Zhou *)
open Ast

module StringMap = Map.Make(String)

let semant_check ast  =
	let check_duplicate l err= 
		let rec helper list = match list with
			n1 :: n2 :: _ when n1 = n2 -> raise (Failure (err^n1))
		| _ :: t -> helper t
		| [] -> ()
		in helper l
	in
	let function_decls = 
		let built_in_decls =  StringMap.add "print"
			{ ftyp = Void; fname = "print"; formals = [Prim_f_decl(Int, "x")];
			locals = []; body = [] } (StringMap.empty)
		in   (*built-in function*)  (*How to add the definition of built-in function? *)
		List.fold_left (fun m fd -> StringMap.add fd.fname fd m) built_in_decls (List.rev (snd ast))
	in		
	let function_decl s = try StringMap.find s function_decls
		with Not_found -> raise (Failure ("undefined function " ^ s))
	in
	let check_func ast  =
		let fname_list = List.fold_left (fun l fd -> fd.fname :: l) ["print"] (List.rev (snd ast))
		in 
		let _ = function_decl "main" 
		in (* Ensure "main" is defined *)
		check_duplicate (List.sort compare fname_list) ("duplicate function definition (or conflict with built-in function): ")
	in
	let typ_of_identifier s map = 
		try StringMap.find s map
		with Not_found ->
			raise (Failure ("undecalred identifier " ^ s))
	in
	let check_assign lv rv err =	(* Besides lv=rv, poly and array indexing is also considered *)
		if lv = rv then lv else
		if lv = "poly_co" && (rv = "int" || rv = "float" || rv = "complex") then rv else
		if (lv = "int array" || lv = "float array" || lv = "bool array" || lv = "complex array") && rv = "array" then lv
		else raise err
	in
	let typ_arrtyp s = 		(* When an array is declared, its type is transformed before being stored in map *)
		if s = "int" then "int array" else
		if s = "float" then "float array" else 
		if s = "bool" then "bool array" else
		if s = "complex" then "complex array"
		else "array"
	in
	let arrtyp_typ s = 
		if s = "int array" then "int" else
		if s = "float array" then "float" else 
		if s = "bool array" then "bool" else 
		if s = "complex array" then "complex" 
	    else "poly_co"
	in
	let check_expr expression m =   (* Give the type of the expression *)
		let build_formal_list l e = match e with    (* Build the formal list for Call expression *)
				Prim_f_decl(t, id) -> (string_of_typ t, id) :: l
			| Arr_f_decl(t, id) -> (typ_arrtyp(string_of_typ t), id) :: l
		in
		let check_extr t expr =
			if t <> "poly" && t <> "int array" && t <> "float array" && t <> "bool array" && t <> "complex array" then
			raise (Failure("type " ^ t ^ " is illegal for indexing: " ^ expr ^ ". poly or array type is expected."))
		in
		let rec expr_typ expr map = match expr with
			 Intlit _ -> "int"
			| Floatlit _ -> "float"
			| Complexlit (_, _) -> "complex"
			| Polylit _ -> "poly"
		 	| Boollit _ -> "bool" 
		  	| Strlit _ -> "string" 
		  	| Arrlit l as e -> (match l with
		  					[] -> "array"
							| [a] -> typ_arrtyp (expr_typ a map)
							| head :: tail -> ignore(List.iter (fun arr -> ignore(check_assign (expr_typ head map) (expr_typ arr map) (Failure(string_of_expr e ^ 
								": type of the elements in array assignment list is inconsistent ")))) tail); typ_arrtyp (expr_typ head map))
			| Binop(e1, op, e2) as e -> let t1 = expr_typ e1 map
		  								and t2 = expr_typ e2 map in
		  								(match op with
		  									Add | Sub | Mult | Div when t1 = t2 -> t1
		  									| Equal | Neq when t1 = t2 -> "bool"
		  									| Less | Leq | Greater | Geq when t1 = t2 && (t1 = "int" || t1 = "float" || t1 = "bool") -> "bool"
		  									| And | Or when  t1 = "bool" && t2 = "bool" -> "bool"
		  									| Modu when t1 = "int" && t2 = "int" -> "int"
		  									| _ -> raise (Failure ("illegal binary operator " ^ t1 ^ " " ^ string_of_op op ^
		  										" " ^ t2 ^ " in " ^ string_of_expr e))
		  								)
		  	| Unop(op, e1) as e -> let t = expr_typ e1 map in
		  							(match op with
		  								Neg when t = "int" || t = "float" -> t
		  								| Not when t = "bool" -> "bool"
		  								| Addone | Subone when t = "int" -> "int"
		  								| _ -> raise (Failure ("illegal unary operator " ^ string_of_unop op ^
		  										" " ^ t ^ " in " ^ string_of_expr e))
		  							)
		  	| Id s -> typ_of_identifier s map
			| Extr (s, e) as ex -> if expr_typ e map = "int" then		(* array and poly indexing *)
							let t = typ_of_identifier s map in
							( ignore(check_extr t (string_of_expr ex));	(* check indexing is legal or not *)
							arrtyp_typ (typ_of_identifier s map))
						else raise (Failure("The index cannot be type " ^ expr_typ e map ^ " in " ^ string_of_expr ex))
		  	| Asn(extr, e) as ex -> let lt = expr_typ extr map
		  						and rt = expr_typ e map in
		  						check_assign lt rt (Failure ("illegal assignment " ^ lt ^ " = " ^ rt ^ " in " ^ string_of_expr ex)) 
		  	| Mod c as e -> let t = expr_typ c map in
							(if t = "complex" then "float"
		  						else raise (Failure ("illegal type " ^ t ^ " in Mod expression " ^ string_of_expr e)))
		  	| Call (fname, actuals) as call-> if fname = "print" then "int" else
		  								(let fd = function_decl fname 
										and l = List.fold_left build_formal_list [] (function_decl fname).formals
										in
										if List.length actuals <> List.length l then
											raise (Failure ("expecting " ^ string_of_int
	 										(List.length l) ^ " arguments in " ^ string_of_expr call))
										else
											List.iter2 (fun (ft, _) e -> 
											let et = expr_typ e map in
											ignore (check_assign ft et (Failure ("illegal actual argument found in " ^ string_of_expr call)))) l (List.rev actuals);
										string_of_typ fd.ftyp)
			| Noexpr -> "void"
		in 
		expr_typ expression m
	in
	let check_decl table decl_list =	(* Check the variable declarations *)
		let check_function m variabledecl=
			let check_void t e =	(* all decl type can not be void *)
				if t = "void"  then raise (Failure ("Illegal variable type voidma in " ^ string_of_variabledecl e))
			in
			let check_arr t expr = 	(* check if the type can be declared as an array *)
				if t <> "int" && t <> "float" && t <> "bool" && t <> "complex" then
				raise (Failure("type " ^ t ^ " is not supported as an array type in " ^ expr))
			in
			let check_arr_init t id i l map=	(* check the type and size of array initializer *)
				if List.length l <> i
				then raise (Failure ("array " ^ id ^ ": length of the initializer doesn't match the array size"));
				List.iter (fun e -> ignore(check_assign (string_of_typ t) (check_expr e map) (Failure("array " ^ id ^ 
					": type of the element " ^ check_expr e map ^ " in initialization " ^ "[" ^ String.concat "," (List.map string_of_expr l) ^ "]" 
					^ " doesn't match the array type " ^ string_of_typ t )))) l
			in
			let check_poly_init i id l map decl =	(* check the type and size of poly initializer *)
				if List.length l <> i+1
				then raise (Failure ("poly " ^ id ^ ": length of the initializer doesn't match the poly size"));
				List.iter (fun e -> let t = check_expr e map in
									if t <> "int" && t <> "float" && t <> "complex" then
									raise(Failure("type " ^ t ^ " of " ^ string_of_expr e ^ "can not be used in the initilization of " ^ string_of_variabledecl decl))) l
			in
			let check_arr_decl t expr =
				if t <> "poly" && t <> "int" && t <> "float" && t <> "bool" && t <> "complex" then
				raise (Failure("type " ^ t ^ " is illegal in " ^ expr ^ ". poly or acceptable array type is expected."))
			in
			let create vdecl = match vdecl with		(* check the declarations *)
				Primdecl(t, id) as decl -> ignore(check_void (string_of_typ t) decl);
									StringMap.add id (string_of_typ t) m
			| Primdecl_i(t, id, e) as decl -> ignore(check_void (string_of_typ t) decl);	(* primitive type with initialization *)
										let rt = check_expr e m in
										ignore( check_assign (string_of_typ t) rt
										(Failure ("illegal assignment " ^ string_of_typ t ^
									     " = " ^ rt ^ " in " ^ string_of_variabledecl decl)));
										StringMap.add id (string_of_typ t) m
			| Arr_poly_decl (t, id, _) as decl -> check_arr_decl (string_of_typ t) (string_of_variabledecl decl);
												StringMap.add id (typ_arrtyp (string_of_typ t)) m
			| Arrdecl_i (t, id, i, l) as decl -> ignore(check_arr (string_of_typ t) (string_of_variabledecl decl));   (* arry decl with initialization *)
										ignore(check_arr_init t id i l m);
										StringMap.add id (typ_arrtyp (string_of_typ t)) m
			| Polydecl_i (t, id, i, l) as decl -> if string_of_typ t <> "poly" then		  (* poly decl with initialization *)
												raise(Failure("type " ^ string_of_typ t ^ " is illegal in " ^ string_of_variabledecl decl))
											else check_poly_init i id l m decl;
											StringMap.add id (string_of_typ t) m
			in
			create variabledecl
		in
		List.fold_left check_function table decl_list
	in
	let check_body func map = 
		let check_bool e = if check_expr e map <> "bool"
						then raise (Failure ("expected Boolean expression in " ^ string_of_expr e ))
		in
		let rec check_stmt counter s = match s with
			Expr e -> ignore (check_expr e map); counter (*counter is used to check if the break statement is legal*)
		| If (e, s1, s2) -> ignore(check_bool e); ignore(check_stmt counter s1); ignore(check_stmt counter s2); counter
		| For (e1, e2, e3, s) -> ignore (check_expr e1 map); ignore(check_bool e2); ignore(check_expr e3 map); ignore(check_stmt 1 s); counter
		| While (e, s) -> ignore(check_bool e); ignore(check_stmt 1 s); counter
		| Return e -> if string_of_typ func.ftyp <> check_expr e map then
					raise (Failure ("Invalid return type " ^ check_expr e map ^ " in function " ^ func.fname ^ 
						". It should be " ^ string_of_typ func.ftyp)); counter
		| Break -> if counter > 0 then counter-1
				else raise (Failure("illegal Break statement in function " ^ func.fname))
		| Block e -> let rec check_block blk cnt = match blk with
					[Return _ as st] -> ignore(check_stmt cnt st); cnt
				| Return _ :: _ -> raise (Failure "nothing can follow a Return statement")
				| Block sl :: ss -> check_block (sl @ ss) cnt
				| st :: ss -> ignore(check_block ss (check_stmt cnt st)); cnt
				| [] -> cnt
			in ignore(check_block e counter); counter
		in
		check_stmt 0 (Block func.body)
	in
	let formal_table global func =
		let check_void_formal t e =
			if string_of_typ t = "void" 
			then raise (Failure ("illegal type void in formal decl " ^ string_of_formaldecl e))
		in
		let build_list l e = match e with 
				Prim_f_decl(_, id) -> id :: l
			| Arr_f_decl(_, id) -> id :: l
		in
		let formal_create map e = match e with
			Prim_f_decl(t, id) ->  ignore(check_void_formal t e);
								StringMap.add id (string_of_typ t) map
		| Arr_f_decl(t, id) -> ignore(check_void_formal t e);
							StringMap.add id (typ_arrtyp (string_of_typ t)) map
		in
		ignore(check_duplicate (List.sort compare (List.fold_left build_list [] func.formals)) 
			("duplicate formal variables in function " ^ func.fname ^ ": "));
		List.fold_left formal_create global func.formals
	in
	check_func ast;  (* check if there is duplicate function definition first *)
	let global = check_decl StringMap.empty (List.rev (fst ast)) in (*check global variables*)
	List.iter (fun func -> ignore(check_body func (check_decl (formal_table global func) func.locals))) (List.rev (snd ast));;