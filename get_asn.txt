    
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