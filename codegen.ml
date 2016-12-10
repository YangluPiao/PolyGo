(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (globals, functiondecl) =
  let context = L.global_context () in
  let the_module = L.create_module context "PolyGo"
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and d64_t  = L.double_type context
  and void_t = L.void_type context in

  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Float -> d64_t 
    | A.Bool -> i1_t
    | A.Void -> void_t 
    | A.String  -> L.pointer_type i8_t
    | A.Complex -> d64_t
    | A.Poly -> d64_t
  in


  let type_of_global= function
      A.Primdecl (t,s) -> (t,s)
  in

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m global =
        let (t,s) = type_of_global global in
        let init = (match t with  A.Int -> L.const_int (ltype_of_typ t) 0
                                | A.Float -> L.const_float (ltype_of_typ t) 0.0
                                | A.Bool -> L.const_int (ltype_of_typ t) 0
                                | A.Void -> L.const_null (ltype_of_typ t)
                                | A.String -> L.const_pointer_null (ltype_of_typ t)
                                | A.Complex -> L.const_float (ltype_of_typ t) 0.0
                                | A.Poly -> L.const_float (ltype_of_typ t) 0.0
                                )in 
    StringMap.add s (L.define_global s init the_module) m in
  List.fold_left global_var StringMap.empty globals in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func = L.declare_function "printf" printf_t the_module in

  let printf_s = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func_s = L.declare_function "printf" printf_s the_module in

  let printf_f = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func_f = L.declare_function "printf" printf_f the_module in


  let strcat_t = L.function_type (L.pointer_type i8_t) 
    [| L.pointer_type i8_t; L.pointer_type i8_t |] in
    let strcat_func = L.declare_function "strcat" strcat_t the_module in

  let type_of_formaldecl = function
      A.Prim_f_decl (t, s) -> (ltype_of_typ t,s) 
  in
  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let typ'= List.map type_of_formaldecl fdecl.A.formals in
      let name = fdecl.A.fname and
          formal_types = Array.of_list (List.map (fun (t,_) ->t) typ') in 
      let ftype = L.function_type (ltype_of_typ fdecl.A.ftyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functiondecl 
  in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function,_) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let float_format_str = L.build_global_stringptr "%f\n" "float" builder in
    let str_format_str = L.build_global_stringptr "%s\n" "str" builder in
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)

    let type_of_locals = function
        A.Primdecl    (t,s)-> (t,s)
      | A.Primdecl_i  (t,s,expr) -> (t,s) in
    let local_vars =
      let add_formal m (formal_typ, name) param = L.set_value_name name param;

      let local = L.build_alloca (ltype_of_typ formal_typ) name builder in
      ignore (L.build_store param local builder);
      StringMap.add name local m in

      let add_local m (local_typ, name) =
      let local_var = L.build_alloca (ltype_of_typ local_typ) name builder in
      StringMap.add name local_var m in

      let my_formals = function
        A.Prim_f_decl (t, s) -> (t,s)
      in
      let formall = List.map my_formals fdecl.A.formals in
      let locall = List.map type_of_locals fdecl.A.locals in 
      let formals  = List.fold_left2 add_formal StringMap.empty formall (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals locall  in

    (* Return the value for a variable or formal argument *)
    let lookup name = try StringMap.find name local_vars
                   with Not_found -> (*StringMap.find name global_vars*)raise (Failure ("Can't find primitive"))
    in

    let rec range a b =
                      if a > b then []
                      else a :: range (a+1) b in
    let rec build_s s length= (if   length = 0 then []
                                      else let result = build_s s (length-1) in
                                          s::result) in

  let whole_map = 
    let add_extra m s name = StringMap.add name s m in
    let rec init_extra  m (t,s)=  match t with A.Poly -> 
                               let length = 10 in
                               let nums = List.map string_of_int (range 1 length) in
                               let ss =  build_s s length in
                               let new_name = List.map2 (fun a c ->a^c)ss nums in
                               let s' = List.map (fun s->L.build_alloca d64_t s builder)new_name in(*TODO: Need more types*)
                               List.fold_left2 add_extra m s' new_name 
                             | A.Complex -> 
                                            let image = s^"i" in
                                            let image_addr = L.build_alloca d64_t image builder in
                                            add_extra m image_addr image; 
                             | _ -> m in
    let whole_local = List.map type_of_locals fdecl.A.locals in
  List.fold_left (init_extra) local_vars whole_local  in

  let search_whole name = try StringMap.find name whole_map
                   with Not_found -> raise (Failure ("Can't find extra"))
    in
  let get_expr_type expr = L.float_of_const (List.hd expr) in


    let primary_builder p = match p with 
       A.Intlit i -> [L.const_int i32_t i]
     | A.Floatlit f -> [L.const_float  d64_t f]
     | A.Strlit s -> [L.build_global_stringptr (String.sub s 1 ((String.length s) - 2)) "" builder]
     | A.Boollit b -> [L.const_int i1_t (if b then 1 else 0)] in

     let asn_poly s e= 
               let length = List.length e in
               let nums = List.map string_of_int (range 1 length) in
               let ss =  build_s s length in
               let new_name = List.map2 (fun a c ->a^c)ss nums in
               let s' = List.map search_whole new_name in 
               ignore(List.map2 (fun e s-> L.build_store e s builder)e s') in
     let asn_complex s e =
               let real_addr = lookup s in
               let image = s^"i" in
               let image_addr = search_whole image in
               let real_vl = List.hd e in
               let image_vl = List.hd (List.tl e) in
               ignore(L.build_store real_vl real_addr builder);
               ignore(L.build_store image_vl image_addr builder) in
    (* Construct code for an expression; return its value *)
    let rec expr builder = function
      A.Asn (ex,e) ->  
      (*TODO: Shouldn't use length to determine type.*)
      (match ex with A.Id s -> (match List.length (expr builder e )with 1-> let e' = List.hd (expr builder e) 
                                                      in ignore(L.build_store e' (lookup s) builder);[e']
                                                  | 2 -> let e' = expr builder e in asn_complex s e';e'
                                                  | _ -> let e' = expr builder e in asn_poly s e';e')
                   | A.Polyextr (s,index) -> let name = s^(string_of_int index) in let e' = List.hd (expr builder e) 
                                            in ignore(L.build_store e' (search_whole name) builder);[e'])
    | A.Primary p -> primary_builder p
    | A.Polylit pl -> (if List.length pl <> 0 then List.map (fun [e] -> e)(List.map (expr builder) pl) 
                                                  else [L.const_int i32_t 0])
    | A.Extr e -> (match e with A.Id s -> [L.build_load (lookup s) s builder]
                             | A.Polyextr (s,index)-> let name = s^(string_of_int index) in
                                                  [L.build_load (search_whole name) name builder] )
    | A.Complexlit (e1,e2) -> let ee1 = List.hd (expr builder e1) in let ee2 = List.hd (expr builder e2) in
                              [ee1;ee2]
    | A.Binop (e1, op, e2) ->
    (let e1' = (expr builder e1)
    and e2' = (expr builder e2) in 
    let int_op =(match op with
      A.Add     -> L.build_add
    | A.Sub     -> L.build_sub
    | A.Mult    -> L.build_mul
    | A.Div     -> L.build_sdiv
    | A.And     -> L.build_and
    | A.Or      -> L.build_or
    | A.Equal   -> L.build_icmp L.Icmp.Eq
    | A.Neq     -> L.build_icmp L.Icmp.Ne
    | A.Less    -> L.build_icmp L.Icmp.Slt
    | A.Leq     -> L.build_icmp L.Icmp.Sle
    | A.Greater -> L.build_icmp L.Icmp.Sgt
    | A.Geq     -> L.build_icmp L.Icmp.Sge) in
    let float_op =(match op with
      A.Add     -> L.build_fadd
    | A.Sub     -> L.build_fsub
    | A.Mult    -> L.build_fmul
    | A.Div     -> L.build_fdiv
    | A.And     -> L.build_and
    | A.Or      -> L.build_or
    | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
    | A.Neq     -> L.build_fcmp L.Fcmp.One
    | A.Less    -> L.build_fcmp L.Fcmp.Ult
    | A.Leq     -> L.build_fcmp L.Fcmp.Ole
    | A.Greater -> L.build_fcmp L.Fcmp.Ogt
    | A.Geq     -> L.build_fcmp L.Fcmp.Oge) in
    let pattern = get_expr_type e1' in
    let opp = (match pattern with None -> int_op
                                  | Some float -> float_op) in
    match List.length e1' with 1 -> let x = List.hd e1' in 
                                   List.map (fun a -> opp x a "tmp" builder)e2'
                             | _ ->List.map2 (fun a b ->opp a b "tmp" builder)e1' e2')
    | A.Unop(op, e) ->
    (match e with A.Extr ee -> (match ee with A.Id s -> let e' = List.hd (expr builder e) in 
                                (match op with A.Neg     -> [L.build_neg e' "tmp" builder]; 
                                              | A.Not     -> [L.build_not e' "tmp" builder]; 
                                              | A.Addone  -> 
                                              ignore(L.build_store (L.build_add e' (L.const_int i32_t 1)"tmp" builder) (lookup s) builder);[L.build_add e' (L.const_int i32_t 1) "tmp" builder]
                                              | A.Subone  -> 
                                              ignore(L.build_store (L.build_sub e' (L.const_int i32_t 1)"tmp" builder) (lookup s) builder);[L.build_sub e' (L.const_int i32_t 1) "tmp" builder]
                                ))
              | _ ->let e' = List.hd (expr builder e) in (match op with  A.Neg     -> [L.build_neg e' "tmp" builder]
                                        | A.Not     -> [L.build_not e' "tmp" builder]
                                        | A.Addone  -> [L.build_add e' (L.const_int i32_t 1) "tmp" builder]
                                        | A.Subone  -> [L.build_sub e' (L.const_int i32_t 1) "tmp" builder]
              )) 
    | A.Call ("print", [e]) | A.Call ("print_b", [e]) ->
    [L.build_call printf_func [| int_format_str ; (List.hd (expr builder e)) |]
      "printf" builder]
    | A.Call ("print_s", [e]) -> [L.build_call printf_func_s 
        [| str_format_str; (List.hd (expr builder e)) |] "printf" builder]
    | A.Call ("print_f", [e]) -> [L.build_call printf_func_f 
        [| float_format_str; (List.hd (expr builder e)) |] "printf" builder]
    | A.Call (f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
   let actuals = List.rev (List.map (fun l -> List.hd (expr builder l)) (List.rev act)) in
   let result = (match fdecl.A.ftyp with A.Void -> ""
                                      | _ -> f ^ "_result") in
         [L.build_call fdef (Array.of_list actuals) result builder]
    | A.Noexpr -> [L.const_int i1_t 0]
    in

    let init t= (match t with  A.Int -> [L.const_int (ltype_of_typ t) 0]
                                | A.Float -> [L.const_float (ltype_of_typ t) 0.0]
                                | A.Bool -> [L.const_int (ltype_of_typ t) 0]
                                | A.Void -> [L.const_null (ltype_of_typ t)]
                                | A.String -> [L.const_pointer_null (ltype_of_typ t)]
                                | A.Poly -> [L.const_float (ltype_of_typ t) 0.0]
                                | A.Complex -> [L.const_float (ltype_of_typ t) 0.0;L.const_float (ltype_of_typ t) 0.0])in 

    let get_asn_local = function
        A.Primdecl_i  (t,s,e) -> (t,s,expr builder e)
      | A.Primdecl    (t,s)      -> (t,s,init t)
    in

    let asn_local = List.map get_asn_local fdecl.A.locals in

    let rec asn= function 
      (t,s,e)->match t with 
                A.Poly -> asn_poly s e;
              | A.Complex -> asn_complex s e;
              | _->  ignore(L.build_store (List.hd e) (lookup s) builder);
                      in
    List.map asn asn_local;

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (f builder) in
  
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
        A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> ignore (match fdecl.A.ftyp with
    A.Void -> L.build_ret_void builder
  | _ -> L.build_ret (List.hd (expr builder e)) builder); builder
      | A.If (predicate, then_stmt_list, else_stmt_list) ->
         let bool_val = List.hd (expr builder predicate) in
   let merge_bb = L.append_block context "merge" the_function in

   let then_bb = L.append_block context "then" the_function in
   add_terminal (stmt (L.builder_at_end context then_bb) then_stmt_list)
     (L.build_br merge_bb);

   let else_bb = L.append_block context "else" the_function in
   add_terminal (stmt (L.builder_at_end context else_bb) else_stmt_list)
     (L.build_br merge_bb);

   ignore (L.build_cond_br bool_val then_bb else_bb builder);
   L.builder_at_end context merge_bb

      | A.While (predicate, body) ->
    let pred_bb = L.append_block context "while" the_function in
    ignore (L.build_br pred_bb builder);

    let body_bb = L.append_block context "while_body" the_function in
    add_terminal (stmt (L.builder_at_end context body_bb) body)
      (L.build_br pred_bb);

    let pred_builder = L.builder_at_end context pred_bb in
    let bool_val = List.hd (expr pred_builder predicate) in

    let merge_bb = L.append_block context "merge" the_function in
    ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
    L.builder_at_end context merge_bb
    | A.For (e1, e2, e3, body) -> stmt builder
      ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.ftyp with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functiondecl;
  the_module