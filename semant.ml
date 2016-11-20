open Ast
open Ast_checker

module StringMap = Map.Make(String)

let check func =
	let check_function m local_decl=
		let check_assign lv rv err =
		 if lv == rv then lv else raise err
		in
		let type_of_identifier s map= 
			try StringMap.find s map
			with Not_found ->
				raise (Failure ("undecalred identifier " ^ s)) 
		in 
		let extr e map = match e with
			Id s -> type_of_identifier s map
		| Polyextr(_, _) -> Int  (***No solution yet***)
		| Arrextr(_, _) -> Int   (***No solution yet***)
		in 
		let prim_c e = match e with
			Intlit _ -> Int
		| Floatlit _ -> Float
		in
		let prim_ap e map = match e with
			Prim_c a -> prim_c a
		| Comp (_, _) -> Complex
		| Extr a -> extr a map
		in
		let primary e map = match e with
			Strlit _ -> String
		| Boollit _ -> Bool
		| Polylit _ -> Poly 
		| Prim_ap s -> prim_ap s map
		in
		let check_array_init t id i l map=	(*check the type and size of array initializer*)
			if List.length l != i
			then raise (Failure ("array " ^ id ^ ": length of the initializer doesn't match the array size"));
			List.iter (fun prim -> ignore(check_assign t (prim_ap prim map) (Failure("array " ^ id ^ 
				": type of the element in initializer " ^ string_of_typ (prim_ap prim map) ^ " doesn't match the array type " ^ string_of_typ t)))) l
		in
		let create vdecl = match vdecl with
			Primdecl(t, id) -> ignore( if string_of_typ t = "void" 
								then raise (Failure ("variable cannot be type void")));
								StringMap.add id t m
		| Primdecl_i(t, id, prim) -> ignore( if string_of_typ t = "void" 
									then raise (Failure ("variable cannot be type void")));
									let rt = primary prim m in
									ignore( check_assign t (primary prim m)
									(Failure ("illegal assignment " ^ string_of_typ t ^
								     " = " ^ string_of_typ rt ^ " in " ^ 
								     string_of_variabledecl vdecl)));
									StringMap.add id t m
		| Arrdecl (t, id, _) -> StringMap.add id t m (***No solution yet***)
		| Arrdecl_i (t, id, i, l) -> ignore(check_array_init t id i l m);
									StringMap.add id t m (***No solution yet***)
		in
		create local_decl
	in
	let result = List.fold_left check_function StringMap.empty (func.locals)
	in
	print_string (func.fname ^ "\n");
	StringMap.iter (fun id t -> print_string(id ^ " " ^ string_of_typ t ^ "\n")) result;
	print_string "\n";;

let lexbuf = Lexing.from_channel stdin;;
let ast = Parser.program Scanner.token lexbuf;;
List.iter check (List.rev (snd ast));