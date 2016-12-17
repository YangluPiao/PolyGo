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
    | A.Primdecl_i  (t,s,_) -> (t,s) 
    | A.Arr_poly_decl      (t,s,_)->(t,s) 
    | A.Arrdecl_i    (t,s,_,_) -> (t,s)
    | A.Polydecl_i (t,s,_,_) -> (t,s)
  in

      let init t= (match t with  A.Int -> L.const_int (ltype_of_typ t) 0
                                | A.Float -> L.const_float (ltype_of_typ t) 0.0
                                | A.Bool -> L.const_int (ltype_of_typ t) 0
                                | A.Void -> L.const_null (ltype_of_typ t)
                                | A.String -> L.const_pointer_null (ltype_of_typ t)
                                | A.Complex -> L.const_float (ltype_of_typ t) 0.0
                                | A.Poly -> L.const_float (ltype_of_typ t) 0.0
                                )in
  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m global =
        let (t,s) = type_of_global global in
    StringMap.add s (L.define_global s (init t) the_module) m in
  List.fold_left global_var StringMap.empty globals in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func = L.declare_function "printf" printf_t the_module in

  let printf_s = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func_s = L.declare_function "printf" printf_s the_module in

  let printf_f = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func_f = L.declare_function "printf" printf_f the_module in


  (*let strcat_t = L.function_type (L.pointer_type i8_t) 
    [| L.pointer_type i8_t; L.pointer_type i8_t |] in
    let strcat_func = L.declare_function "strcat" strcat_t the_module in*)

  let type_of_formaldecl = function
      A.Prim_f_decl (t, s) -> (ltype_of_typ t,s) 
    | A.Arr_f_decl (t,s) -> (ltype_of_typ t,s) 
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
    
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%f\n" "float" builder
    and str_format_str = L.build_global_stringptr "%s\n" "str" builder
    and real_format_str = L.build_global_stringptr "%.3f+" "real" builder 
    and image_format_str = L.build_global_stringptr "%.3fi\n" "image" builder in
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)


    (*let decl_map =StringMap.empty in
    let init_expr e s= match e with A.Primary p -> ignore(StringMap.add s (init_expr e) decl_map);List.hd (primary_builder p);
                                | _ -> StringMap.find 
                     in*)
    (*let type_of_locals = function
        A.Primdecl    (t,s)-> (t,s,0,[init t])
      | A.Primdecl_i  (t,s,e) -> (t,s,0,[init_expr e s]) 
      | A.Arrdecl      (t,s,_)->(t,s,1,[init t]) 
      | A.Arrdecl_i    (t,s,_,e) -> (t,s,1,List.map2 init_expr e s) in*)

    let type_of_locals = function
        A.Primdecl    (t,s)-> (t,s,0,init t)
      | A.Primdecl_i  (t,s,e) -> (t,s,0,init t) 
      | A.Arr_poly_decl      (t,s,i)->(t,s,i,init t) 
      | A.Arrdecl_i    (t,s,i,e) -> (t,s,i,init t) 
      | A.Polydecl_i (t,s,i,e) -> (t, s, i , init t) in

    let rec range a b =
                      if a > b then []
                      else a :: range (a+1) b in
    let rec build_s s length= (if   length = 0 then []
                                      else let result = build_s s (length-1) in
                                          s::result) in
    let local_vars =
      let add_formal m (formal_typ, name) param = L.set_value_name name param;

      let local = L.build_alloca (ltype_of_typ formal_typ) name builder in
      ignore (L.build_store param local builder);
      StringMap.add name local m in

      let add_local m (local_typ, name,_,_) = 
      let local_var = L.build_alloca (ltype_of_typ local_typ) name builder in
                            StringMap.add name local_var m in

      let my_formals = function
        A.Prim_f_decl (t, s) -> (t,s)
      | A.Arr_f_decl (t,s) ->(t,s)
      in

      let formall = List.map my_formals fdecl.A.formals in
      let locall = List.map type_of_locals fdecl.A.locals in 
      let formals  = List.fold_left2 add_formal StringMap.empty formall (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals locall  in

    let basic_map = 
    let add_arr m s name = StringMap.add name s m in
         let  init_arr m (local_typ, name,index,_)= 
              (match index with 0 -> m
                     | _ -> let nums = List.map string_of_int (range 0 (index-1)) in
                               let ss =  build_s name index in
                               let new_name = List.map2 (fun a c ->a^c)ss nums in
                               let s' = List.map (fun s->L.build_alloca (ltype_of_typ local_typ) s builder)new_name in(*TODO: Need more types*)
                               List.fold_left2 add_arr m s' new_name
                ) in
      let for_arr = List.map type_of_locals fdecl.A.locals in
    List.fold_left init_arr local_vars for_arr in


    (* Return the value for a variable or formal argument *)
    let search_whole name = try StringMap.find name basic_map
                   with Not_found -> StringMap.find name global_vars (*raise (Failure ("SBaa"))*)
    in

  let whole_map = 
    let add_extra m s name = StringMap.add name s m in
    (*TODO: index of poly and complex*)
    let init_extra  m (t,s,_,_)=  (match t with 
                                     A.Complex -> 
                                            let image = s^"i" in
                                            let image_addr = L.build_alloca d64_t image builder in
                                            add_extra m image_addr image; 
                              | _ -> m) in
    let whole_local = List.map type_of_locals fdecl.A.locals in
  List.fold_left (init_extra) basic_map whole_local  in

  let search_whole name = try StringMap.find name whole_map
                   with Not_found -> raise (Failure ("Can't find extra"))
    in
  let get_expr_type expr = L.type_of (List.hd expr) in


     let asn_poly s e= 
               let length = List.length e in
               let suffix = List.map string_of_int (range 1 length) in
               let ss =  build_s s length in
               let poly_ID = List.map2 (fun a c ->a^c)ss suffix in
               let s' = List.map search_whole poly_ID in 
               ignore(List.map2 (fun e s-> L.build_store e s builder)e s') in

     let asn_complex s e =
               let real_addr = search_whole s in
               let image = s^"i" in
               let image_addr = search_whole image in
               let real_vl = List.hd e in
               let image_vl = List.hd (List.tl e) in
               ignore(L.build_store real_vl real_addr builder);
               ignore(L.build_store image_vl image_addr builder) in
     let asn_array s e index= 
                let suffix = List.map string_of_int (range 0 (index-1)) in
                let ss = build_s s index in
                let arr_ID = List.map2 (fun a c -> a^c)ss suffix in
                let s' = List.map search_whole arr_ID in
                ignore(List.map2 (fun e s-> L.build_store e s builder)(List.rev e) s') in


    (* Construct code for an expression; return its value *)
    let rec expr builder = function
      A.Asn (ex,e) ->  
      (*TODO: Shouldn't use length to determine type.*)
      (match ex with A.Id s -> (match List.length (expr builder e )with 1-> let e' = List.hd (expr builder e) 
                                                      in ignore(L.build_store e' (search_whole s) builder);[e']
                                                  | 2 -> let e' = expr builder e in asn_complex s e';e'
                                                  | _ -> let e' = expr builder e in asn_poly s e';e')
                   | A.Extr (s, e) -> let i = List.hd (expr builder e) in 
                                      let i_option = L.int64_of_const i in 
                                      let index  = 
                                        (match i_option with None -> raise(Failure("Holy crap"))
                                                            | Some v -> string_of_int (Int64.to_int v)
                                        ) in
                                    let name = s^index and 
                                        e' = List.hd (expr builder e) 
                                            in ignore(L.build_store e' (search_whole name) builder);[e']
      )

     | A.Intlit i -> [L.const_int i32_t i]
     | A.Floatlit f -> [L.const_float  d64_t f]
     | A.Strlit s -> [L.build_global_stringptr (String.sub s 1 ((String.length s) - 2)) "" builder]
     | A.Boollit b -> [L.const_int i1_t (if b then 1 else 0)] 
     | A.Polylit pl -> (if List.length pl <> 0 then List.map (fun [e] -> e)(List.map (expr builder) pl) 
                                                  else [L.const_int i32_t 0])
    | A.Arrlit al -> (if List.length al <> 0 then List.map (fun [e] -> e)(List.map (expr builder) al) 
                                                 else [L.const_int i32_t 0])
    | A.Id s -> (try  [L.build_load (search_whole s) s builder;L.build_load (StringMap.find (s^"i") whole_map) (s^"i") builder]  with
                                                                        Not_found -> [L.build_load (search_whole s) "tmp" builder])
    
    | A.Extr (s, e) -> let i = List.hd (expr builder e) in 
                    let i_option = L.int64_of_const i in 
                                      let index  = 
                                        (match i_option with None -> raise(Failure("Holy crap"))
                                                            | Some v -> string_of_int (Int64.to_int v)
                                        ) in
                                    let name = s^index in
                                    [L.build_load (search_whole name) name builder] 

    (*  | A.Extr e -> (match e with A.Id s -> (try  [L.build_load (search_whole s) s builder;L.build_load (StringMap.find (s^"i") whole_map) (s^"i") builder]  with
                                                                        Not_found -> [L.build_load (search_whole s) "tmp" builder]
                                          )
                             | A.Polyextr (s,index)-> let name = s^(string_of_int index) in
                                                  [L.build_load (search_whole name) name builder] 
                             | A.Arrextr (s,index)-> let name = s^(string_of_int index) in
                                                  [L.build_load (search_whole name) name builder]
                             | A.Poly_id (s1,s2) -> let i = List.hd (expr builder (A.Extr (A.Id s2))) in
                                          let i_option = L.int64_of_const i in 
                                      let index  = 
                                        (match i_option with None -> raise(Failure("Holy crap"))
                                                            | Some v -> string_of_int (Int64.to_int v)
                                        ) in
                                    let name = s1^index in
                                    [L.build_load (search_whole name) name builder]
                             
                              )  *)

    | A.Complexlit (e1,e2) -> let ee1 = List.hd (expr builder e1) in let ee2 = List.hd (expr builder e2) in
                              [ee1;ee2]
    | A.Mod e -> (match e with A.Complexlit (e1,e2) ->
                    let real = L.float_of_const (L.build_fmul (List.hd (expr builder e1)) (List.hd (expr builder e1))"tmp" builder)in
                    let out1 = (match real with None -> raise(Failure("Invalid complex value1."))
                                | Some v1 -> v1) in
                    let image = L.float_of_const (L.build_fmul (List.hd (expr builder e2)) (List.hd (expr builder e2))"tmp" builder)in
                    let out2 = (match image with None -> raise(Failure("Invalid complex value2."))
                                | Some v2 -> v2) in
                                expr builder (A.Floatlit (sqrt(out1+.out2)))
                           | A.Id s -> 
                                let e1 = L.build_load (search_whole s) "tmp" builder in
                                   let e2 = L.build_load (search_whole (s^"i")) "tmp" builder in
                                   let real = L.float_of_const (L.build_fmul e1 e1"tmp" builder)in
                                    let out1 = (match real with None -> raise(Failure("Invalid complex value3."))
                                                | Some v1 -> v1) in
                                    let image = L.float_of_const (L.build_fmul e2 e2 "tmp" builder)in
                                    let out2 = (match image with None -> raise(Failure("Invalid complex value4."))
                                                | Some v2 -> v2) in
                                                expr builder  (A.Floatlit (sqrt(out1+.out2)))
                                                      | _ -> raise(Failure("Invalid complex value5.")) 
                                        
                           | _->raise(Failure("Invalid complex value6.")) 
                )

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
    | A.Geq     -> L.build_icmp L.Icmp.Sge
    | A.Modu    -> L.build_srem) in
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
    | A.Geq     -> L.build_fcmp L.Fcmp.Oge
    | A.Modu -> L.build_frem ) in
    let typ = get_expr_type e1' in
    let opp = (if typ = i32_t then int_op
                                  else float_op) in
    match List.length e1' with 1 -> let x = List.hd e1' in 
                                   List.map (fun a -> opp x a "tmp" builder)e2'
                             | _ ->(let a1 = List.hd e1'
                                    and a2 = List.hd(List.tl e1')
                                    and b1 = List.hd e2'
                                    and b2 = List.hd(List.tl e2') in
                                    match op with A.Mult -> 
                                    let first = L.build_fsub (opp a1 b1 "tmp" builder) (opp a2 b2 "tmp" builder)"tmp" builder
                                    and second = L.build_fadd (opp a1 b2 "tmp" builder) (opp a2 b1 "tmp" builder)"tmp" builder in
                                    [first;second]
                                                | A.Div -> 
                                    let molecular1 = L.build_fadd (L.build_fmul a1 b1 "tmp" builder) (L.build_fmul a2 b2 "tmp" builder)"tmp" builder
                                    and molecular2 = L.build_fsub (L.build_fmul a2 b1 "tmp" builder) (L.build_fmul a1 b2 "tmp" builder)"tmp" builder
                                    and denominator = L.build_fadd (L.build_fmul b1 b1 "tmp" builder) (L.build_fmul b2 b2 "tmp" builder) "tmp" builder in
                                    let first = opp molecular1 denominator "tmp" builder 
                                    and second = opp molecular2 denominator "tmp" builder in
                                    [first;second]
                                                | _ -> List.map2 (fun a b ->opp a b "tmp" builder)e1' e2'
                                  )
    )
    | A.Unop(op, e) ->let e' = List.hd (expr builder e) in  
                                let var_opt = L.float_of_const e' in
                                let var = (match var_opt with None -> 1000.0
                                                          | Some v1 -> v1) in
                                let typ = get_expr_type [e'] in
                                let neg = (if typ = i32_t then (L.build_neg e' "tmp" builder )else (L.build_fneg e' "tmp" builder)) in 
                                let addone = (if typ = i32_t then ((L.build_add e' (L.const_int i32_t 1)"tmp" builder)) else ((L.build_fadd e' (L.const_float d64_t 1.0)"tmp" builder)))in
                                let subone = (if typ = i32_t then ((L.build_sub e' (L.const_int i32_t 1)"tmp" builder)) else ((L.build_fsub e' (L.const_float d64_t 1.0)"tmp" builder)))in
                                let sqrt = (if var > 0.0 then [List.hd (expr builder (A.Floatlit (sqrt(var))));List.hd (expr builder (A.Floatlit (sqrt(-.var))))] else 
                                                [List.hd (expr builder (A.Floatlit (sqrt(-.var))))]) in 
    (match e with A.Id s ->(match op with A.Neg     -> [neg ]; 
                                              | A.Not     -> [L.build_not e' "tmp" builder]; 
                                              | A.Sqrt -> sqrt;          
                                              | A.Addone  -> 
                                              ignore(L.build_store addone (search_whole s) builder);[addone]
                                              | A.Subone  -> 
                                              ignore(L.build_store subone (search_whole s) builder);[subone]
                                )
              | _ ->(match op with  A.Neg     -> [neg]
                                | A.Not     -> [L.build_not e' "tmp" builder]
                                | A.Sqrt -> sqrt          
                                | A.Addone  -> [addone]
                                | A.Subone  -> [subone]
              )) 
    | A.Call ("print", [e]) | A.Call ("print_b", [e]) ->
    [L.build_call printf_func [| int_format_str ; (List.hd (expr builder e)) |]
      "printf" builder]
    | A.Call ("print_s", [e]) -> [L.build_call printf_func_s 
        [| str_format_str; (List.hd (expr builder e)) |] "printf" builder]
    | A.Call ("print_f", [e]) -> [L.build_call printf_func_f 
        [| float_format_str; (List.hd (expr builder e)) |] "printf" builder]
    | A.Call ("print_c", [e]) -> [(L.build_call printf_func_f 
        [| image_format_str; (List.hd (List.tl(expr builder e))) |] "printf" builder);(L.build_call printf_func_f 
        [| real_format_str; (List.hd (expr builder e)) |] "printf" builder)]
    |  A.Call ("print_n", [e]) -> [L.build_call printf_func_f
        [| image_format_str; (List.hd (expr builder e)) |] "printf" builder]
    | A.Call (f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
   let actuals = List.rev (List.map (fun l -> List.hd (expr builder l)) (List.rev act)) in
   let result = (match fdecl.A.ftyp with A.Void -> ""
                                      | _ -> f ^ "_result") in
         [L.build_call fdef (Array.of_list actuals) result builder]
    | A.Noexpr -> [L.const_int i1_t 0]
    in


(*       let init t= (match t with  A.Int -> L.const_int (ltype_of_typ t) 0
                                | A.Float -> L.const_float (ltype_of_typ t) 0.0
                                | A.Bool -> L.const_int (ltype_of_typ t) 0
                                | A.Void -> L.const_null (ltype_of_typ t)
                                | A.String -> L.const_pointer_null (ltype_of_typ t)
                                | A.Complex -> L.const_float (ltype_of_typ t) 0.0
                                | A.Poly -> L.const_float (ltype_of_typ t) 0.0
                                )in *)
      
    let init t= (match t with  A.Int -> [L.const_int (ltype_of_typ t) 0]
                                | A.Float -> [L.const_float (ltype_of_typ t) 0.0]
                                | A.Bool -> [L.const_int (ltype_of_typ t) 0]
                                | A.Void -> [L.const_null (ltype_of_typ t)]
                                | A.String -> [L.const_pointer_null (ltype_of_typ t)]
                                | A.Poly -> [L.const_float (ltype_of_typ t) 0.0]
                                | A.Complex -> [L.const_float (ltype_of_typ t) 0.0;L.const_float (ltype_of_typ t) 0.0])in 

    let get_asn_local = function
        A.Primdecl_i  (t,s,e) -> (t,s,expr builder e,0)
      | A.Primdecl    (t,s)      -> (t,s,init t,0)
      | A.Arr_poly_decl      (t,s,index)->let arr_poly_init = build_s (List.hd (init t)) (index) in (t,s,arr_poly_init,index) 
      | A.Arrdecl_i    (t,s,index,e) -> (match t with A.Complex -> raise(Failure("Do we supoort array of Poly or Complex!?"))
                                                    | _ ->let arr_poly_decl = List.map (fun [a]->a)(List.map (expr builder) e)in 
                                              (t,s,arr_poly_decl,index) 
                                        )
      | A.Polydecl_i (t,s,index,e) -> (match t with A.Complex -> raise(Failure("Do we supoort array of Poly or Complex!?"))
                                                    | _ ->let arr_poly_decl = List.map (fun [a]->a)(List.map (expr builder) e)in 
                                              (t,s,arr_poly_decl,index) 
                                        )
    in

    let asn_local = List.map get_asn_local fdecl.A.locals in
    let asn= function 
      (t,s,e,index)->(match index with 0 ->(match t with 
                                            A.Poly -> ignore(asn_poly s e);
                                          | A.Complex -> ignore(asn_complex s e);
                                          | _->  ignore(L.build_store (List.hd e) (search_whole s) builder);)
                                    | _ -> ignore(asn_array s e index);)
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
    let rec stmt (builder,break_b) = function
        A.Block sl -> List.fold_left stmt (builder,break_b) sl
      | A.Expr e -> ignore (expr builder e); (builder,break_b)
      | A.Break -> 
        ignore(add_terminal builder (L.build_br break_b));
        let new_block = L.append_block context "after.break" the_function in
        let builder = L.builder_at_end context new_block in (builder, break_b)
      | A.Return e -> ignore (match fdecl.A.ftyp with
                  A.Void -> L.build_ret_void builder
                  | _ -> L.build_ret (List.hd (expr builder e)) builder); (builder,break_b)
      | A.If (predicate, then_stmt, else_stmt) ->
         let bool_val = List.hd (expr builder predicate) in
   let merge_bb = L.append_block context "merge" the_function in

   let then_bb = L.append_block context "then" the_function in
   let b = L.builder_at_end context then_bb in
   let (tmp1,_) = (stmt (b,break_b) then_stmt) in
   add_terminal tmp1 (L.build_br merge_bb);

   let else_bb = L.append_block context "else" the_function in
   let b = L.builder_at_end context else_bb in
   let (tmp1,_) = (stmt (b,break_b) else_stmt) in
   add_terminal tmp1 (L.build_br merge_bb);

   ignore (L.build_cond_br bool_val then_bb else_bb builder);
   ((L.builder_at_end context merge_bb),break_b)

      | A.While (predicate, body) ->
    let pred_b = L.append_block context "pred" the_function in
        ignore (L.build_br pred_b builder);
        let body_b = L.append_block context "body" the_function in
        let merge_b = L.append_block context "merge.block" the_function in
        let break_builder = merge_b in
        let b = L.builder_at_end context body_b in
        let (temp1,_)= stmt (b, break_builder) body in
        ignore(add_terminal temp1 (L.build_br pred_b)); 
        let pred_builder = L.builder_at_end context pred_b in
        let bool_val = match expr pred_builder predicate with p->List.hd p in
        ignore (L.build_cond_br bool_val body_b merge_b pred_builder);
        ((L.builder_at_end context merge_b), break_builder)
    | 
    A.For (e1, e2, e3, body) -> 
        stmt (builder, break_b)
        ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let dummy_bb = L.append_block context "dummy.toremove.block" the_function in
    let break_builder = dummy_bb in
    let (builder, _) = (stmt (builder, break_builder) (A.Block fdecl.A.body))  in
    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.ftyp with
        A.Void -> L.build_ret_void
      | A.Int -> L.build_ret (L.const_int i32_t 0)
      | A.Float -> L.build_ret (L.const_float d64_t 0.0));
    ignore(L.builder_at_end context dummy_bb);
    ignore(L.block_terminator dummy_bb);
    ignore(L.delete_block dummy_bb);
  in

  List.iter build_function_body functiondecl;
  the_module
