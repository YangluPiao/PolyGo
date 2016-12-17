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
     let rec range a b =
                      if a > b then []
                      else a :: range (a+1) b in
    let copy_array size old_addr new_addr builder =
    let rec copy_idx idx =(match idx with
      -1 -> 0
    | _  ->
        let idx' = [|L.const_int i32_t idx|] in
        let idx_ptr_n = L.build_in_bounds_gep new_addr idx' "newArr" builder
        and idx_ptr_o = L.build_in_bounds_gep old_addr idx' "oldArr" builder
        in
        let val_old = L.build_load idx_ptr_o "oldArrIdx" builder in
        ignore(L.build_store val_old idx_ptr_n builder); copy_idx (idx-1)
    ) in copy_idx (size-1)
  in

    let type_of_locals = function
        A.Primdecl    (t,s)-> (match t with  (* A.Complex -> (t,s,2,init t) *) 
                                          | _ -> (t,s,0,init t))
      | A.Primdecl_i  (t,s,e) -> (match t with  (* A.Complex -> (t,s,2,init t)  *)
                                          | _ -> (t,s,0,init t))
      | A.Arr_poly_decl      (t,s,i)->(t,s,i,init t) 
      | A.Arrdecl_i    (t,s,i,e) -> (t,s,i,init t) 
      | A.Polydecl_i (t,s,i,e) -> (t, s,i,init t) in

    let local_vars =
      let add_formal m (formal_typ, name) param = L.set_value_name name param;

      let local = L.build_alloca (ltype_of_typ formal_typ) name builder in
      ignore (L.build_store param local builder);
      StringMap.add name (local,0) m in

      let add_local m (local_typ, name,length,_) = 
      let local_var =   (match length with 0 -> let prim_addr = (match local_typ with A.Complex -> L.build_array_alloca (ltype_of_typ local_typ) (L.const_int i32_t 2) name builder
                                                                                | _ -> L.build_alloca (ltype_of_typ local_typ) name builder )in prim_addr
                                      | _ ->let addr = (match local_typ with A.Poly -> L.build_array_alloca (ltype_of_typ local_typ) (L.const_int i32_t (length+1)) name builder
                                                            | _ -> L.build_array_alloca (ltype_of_typ local_typ) (L.const_int i32_t (length)) name builder) in addr
                        )in
       (match local_typ with A.Poly -> StringMap.add name (local_var,(length+1)) m
                          | A.Complex -> StringMap.add name (local_var,(2)) m
                          | _ -> StringMap.add name (local_var,(length)) m) in

      let my_formals = function
        A.Prim_f_decl (t, s) -> (t,s)
      | A.Arr_f_decl (t,s) ->(t,s)
      in

      let formall = List.map my_formals fdecl.A.formals in
      let locall = List.map type_of_locals fdecl.A.locals in 
      let formals  = List.fold_left2 add_formal StringMap.empty formall (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals locall  in

   
 let lookup_name name = (fun (s,_)->s)(try StringMap.find name local_vars
                   with Not_found -> raise(Failure("WTF"))) (*raise (Failure ("SBaa"))*)
    in
    let lookup_size name = (fun (_,l)->l)(try StringMap.find name local_vars
                   with Not_found -> raise(Failure("WTF"))) (*raise (Failure ("SBaa"))*)
    in
  let get_expr_type expr = L.type_of (List.hd expr) in


  let asn_extr_value s index_expr value builder =
    let i  = [|index_expr|]in
    let addr' = L.build_in_bounds_gep (lookup_name s) i "storeArr" builder in
    L.build_store value addr' builder
  in
  let build_addr size s = 
     let r = range 0 (size-1) in 
     let i  = List.map (fun index -> [|L.const_int i32_t index|]) r in
     List.map (fun i -> L.build_in_bounds_gep (lookup_name s) i "tmp_addr" builder)i in
  let load_value size s = 
      (match size with 0 -> [L.build_load (lookup_name s) "tmp" builder]
                    | _ ->  let addrs = build_addr size s in List.map (fun addr -> L.build_load addr "tmp_val" builder) addrs )
    in
  let asn_val s vals =
      let size = lookup_size s in 
      (match size with 0 -> [L.build_store (List.hd vals) (lookup_name s) builder] 
                     | _ -> let addrs = build_addr size s in 
                            List.map2 (fun value addr -> L.build_store value addr builder)vals addrs
      )
       in
    (* Construct code for an expression; return its value *)
    let rec expr builder = function
      A.Asn (ex,e) -> (match ex with A.Extr (s,index_expr) -> let e' = List.hd(expr builder e) in let index = List.hd(expr builder index_expr) in
                                                                      [asn_extr_value s index e' builder]
                                   | A.Id s ->  let e' = expr builder e in asn_val s e'
                                                
                       ) 
     | A.Intlit i -> [L.const_int i32_t i]
     | A.Floatlit f -> [L.const_float  d64_t f]
     | A.Strlit s -> [L.build_global_stringptr (String.sub s 1 ((String.length s) - 2)) "" builder]
     | A.Boollit b -> [L.const_int i1_t (if b then 1 else 0)] 
     | A.Polylit pl -> (if List.length pl <> 0 then List.map (fun [e] -> e)(List.map (expr builder) pl) 
                                                  else [L.const_int i32_t 0])
    | A.Arrlit al -> (if List.length al <> 0 then List.map (fun [e] -> e)(List.map (expr builder) al) 
                                                 else [L.const_int i32_t 0])
    | A.Id s -> let size = lookup_size s in load_value size s
    | A.Extr(s,index) -> let e' = List.hd(expr builder index) in 
                           let addr = L.build_in_bounds_gep (lookup_name s) [|e'|] "storeArrIdx" builder  in
                                                                [L.build_load addr "tmp" builder]

    | A.Complexlit (e1,e2) -> let ee1 = List.hd (expr builder e1) in let ee2 = List.hd (expr builder e2) in
                              [ee1;ee2]
    | A.Mod e-> 
                let real = L.float_of_const (L.build_fmul (List.hd (expr builder e)) (List.hd (expr builder e))"tmp" builder)in
                let out1 = (match real with None -> raise(Failure("Invalid complex value1."))
                            | Some v1 -> v1) in
                let image = L.float_of_const (L.build_fmul (List.hd(List.tl (expr builder e))) (List.hd(List.tl (expr builder e)))"tmp" builder)in
                let out2 = (match image with None -> raise(Failure("Invalid complex value2."))
                            | Some v2 -> v2) in
                            expr builder (A.Floatlit (sqrt(out1+.out2)))

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
    (* | A.Unop(op, e) ->let e' = List.hd (expr builder e) in  
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
              ))  *)
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
      
    let init t= (match t with  A.Int -> [L.const_int (ltype_of_typ t) 0]
                                | A.Float -> [L.const_float (ltype_of_typ t) 0.0]
                                | A.Bool -> [L.const_int (ltype_of_typ t) 0]
                                | A.Void -> [L.const_null (ltype_of_typ t)]
                                | A.String -> [L.const_pointer_null (ltype_of_typ t)]
                                | A.Poly -> [L.const_float (ltype_of_typ t) 0.0]
                                | A.Complex -> [L.const_float (ltype_of_typ t) 0.0;L.const_float (ltype_of_typ t) 0.0])in 
    let rec generate_zeros_int length typ= if length = 0 then []
                      else (L.const_int i32_t 0) :: generate_zeros_int (length-1) typ in
    let rec generate_zeros_float length typ= if length = 0 then []
                      else (L.const_float d64_t 0.0) :: generate_zeros_float (length-1) typ in
    let get_asn_local = function
        A.Primdecl_i  (t,s,e) -> (t,s,0,expr builder e)
      | A.Primdecl    (t,s)      -> (t,s,0,init t)
      | A.Arr_poly_decl      (t,s,length)->(match t with A.Int -> (t,s,length,(generate_zeros_int (length) t))
                                                       | A.Float -> (t,s,length,(generate_zeros_float (length) t))
                                                       | A.Poly ->  (t,s,(length+1),(generate_zeros_float (length+1) t))
                                            )
      | A.Arrdecl_i    (t,s,size,e) -> (let arr_decl = List.map (fun [a]->a)(List.map (expr builder) (List.rev e))in 
                                              (t,s,size,arr_decl))
      | A.Polydecl_i (t,s,size,e) ->  (let poly_decl = List.map (fun [a]->a)(List.map (expr builder) (e))in 
                                              (t,s,(size+1),poly_decl))
    in
     
  

    let asn_local = List.map get_asn_local fdecl.A.locals in
    let asn= function 
      (t,s,size,e)->ignore(asn_val s e) in
    
     List.map (asn) asn_local;


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
