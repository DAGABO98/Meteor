(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/
   http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Meteor" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context 
  and void_t     = L.void_type   context
  and float_t    = L.float_type  context in

  (* Declare C structs *)
  
  (*Mut structs*)
  let struct_mut_int_t : L.lltype = 
      L.named_struct_type context "MutInt" in

  let struct_mut_float_t : L.lltype = 
      L.named_struct_type context "MutFloat" in

  let struct_mut_bool_t : L.lltype = 
      L.named_struct_type context "MutBool" in

  let _ = 
      L.struct_set_body struct_mut_int_t
      [| L.pointer_type i32_t |] false;
      L.struct_set_body struct_mut_float_t
      [| L.pointer_type float_t |] false;
      L.struct_set_body struct_mut_bool_t
      [| L.pointer_type i1_t |] false in

  (* Ref structs *)
  let struct_ref_int_t : L.lltype = 
      L.named_struct_type context "RefInt" in

  let struct_ref_float_t : L.lltype = 
      L.named_struct_type context "RefFloat" in

  let struct_ref_bool_t : L.lltype = 
      L.named_struct_type context "RefBool" in

  let _ = 
      L.struct_set_body struct_ref_int_t
      [| L.pointer_type i32_t |] false;
      L.struct_set_body struct_ref_float_t
      [| L.pointer_type float_t |] false;
      L.struct_set_body struct_ref_bool_t
      [| L.pointer_type i1_t |] false in
   
  (* Foo struct *)
  let struct_foo_t : L.lltype = 
      L.named_struct_type context "Foo" in

  let _ = 
      L.struct_set_body struct_foo_t
      [| i32_t |] false in

  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
      A.Mut(x)   -> (match x with
                    | A.Int -> struct_mut_int_t
                    | A.Bool -> struct_mut_bool_t
                    | A.Float -> struct_mut_float_t
                    | _ -> i32_t )
    | A.Ref(x)  -> (match x with
                    | A.Int -> struct_ref_int_t
                    | A.Bool -> struct_ref_bool_t
                    | A.Float -> struct_ref_float_t 
                    | _ -> i32_t )
    | A.RType(x) -> (match x with 
                    | A.Foo -> struct_foo_t
                    | A.Int -> i32_t
                    | A.Bool -> i1_t
                    | A.Char -> i8_t
                    | A.Float -> float_t
                    | A.String -> i8_t )

  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in

  (* Declare each C function*)

  (* Mut functions*)
    (* Init *)
  let initMutInt_t : L.lltype = L.function_type (void_t)
    [| L.pointer_type struct_mut_int_t |] in

  let initMutInt : L.llvalue = L.declare_function "initMutInt" initMutInt_t the_module in

  let initMutFloat_t : L.lltype = L.function_type (void_t)
    [| L.pointer_type struct_mut_float_t |] in

  let initMutFloat : L.llvalue = L.declare_function "initMutFloat" initMutFloat_t the_module in

  let initMutBool_t : L.lltype = L.function_type (void_t)
    [| L.pointer_type struct_mut_bool_t |] in

  let initMutBool : L.llvalue = L.declare_function "initMutBool" initMutBool_t the_module in

  (* Assign *)
  let assignMutInt_t : L.lltype = L.function_type (void_t)
    [| L.pointer_type struct_mut_int_t; i32_t |] in

  let assignMutInt : L.llvalue = L.declare_function "assignMutInt" assignMutInt_t the_module in

  let assignMutFloat_t : L.lltype = L.function_type (void_t)
    [| L.pointer_type struct_mut_float_t; float_t |] in

  let assignMutFloat : L.llvalue = L.declare_function "assignMutFloat" assignMutFloat_t the_module in

  let assignMutBool_t : L.lltype = L.function_type (void_t)
    [| L.pointer_type struct_mut_bool_t; i1_t |] in

  let assignMutBool : L.llvalue = L.declare_function "assignMutBool" assignMutBool_t the_module in

    (* Read *)
  let readMutInt_t : L.lltype = L.function_type (i32_t)
    [| L.pointer_type struct_mut_int_t |] in 

  let readMutInt : L.llvalue = L.declare_function "readMutInt" readMutInt_t the_module in 

  let readMutFloat_t : L.lltype = L.function_type (float_t)
    [| L.pointer_type struct_mut_float_t |] in 

  let readMutFloat : L.llvalue = L.declare_function "readMutFloat" readMutFloat_t the_module in 

  let readMutBool_t : L.lltype = L.function_type (i1_t)
    [| L.pointer_type struct_mut_bool_t |] in 

  let readMutBool : L.llvalue = L.declare_function "readMutBool" readMutBool_t the_module in 

  (* Destroy *)
  let destroyMutInt_t : L.lltype = L.function_type (void_t)
    [| L.pointer_type struct_mut_int_t |] in

  let destroyMutInt : L.llvalue = L.declare_function "destroyMutInt" destroyMutInt_t the_module in

  let destroyMutFloat_t : L.lltype = L.function_type (void_t)
    [| L.pointer_type struct_mut_float_t |] in

  let destroyMutFloat : L.llvalue = L.declare_function "destroyMutFloat" destroyMutFloat_t the_module in

  let destroyMutBool_t : L.lltype = L.function_type (void_t)
    [| L.pointer_type struct_mut_bool_t |] in

  let destroyMutBool : L.llvalue = L.declare_function "destroyMutBool" destroyMutBool_t the_module in

  (* Ref functions*)
    (* Init *)
  let initRefInt_t : L.lltype = L.function_type (void_t)
    [| L.pointer_type struct_ref_int_t |] in

  let initRefInt : L.llvalue = L.declare_function "initRefInt" initRefInt_t the_module in

  let initRefFloat_t : L.lltype = L.function_type (void_t)
    [| L.pointer_type struct_ref_float_t |] in

  let initRefFloat : L.llvalue = L.declare_function "initRefFloat" initRefFloat_t the_module in

  let initRefBool_t : L.lltype = L.function_type (void_t)
    [| L.pointer_type struct_ref_bool_t |] in

  let initRefBool : L.llvalue = L.declare_function "initRefBool" initRefBool_t the_module in

  (* Assign *)
  let assignRefIntMut_t : L.lltype = L.function_type (void_t)
    [| L.pointer_type struct_ref_int_t; L.pointer_type struct_mut_int_t |] in

  let assignRefIntMut : L.llvalue = L.declare_function "assignRefIntMut" assignRefIntMut_t the_module in

  let assignRefIntRef_t : L.lltype = L.function_type (void_t)
    [| L.pointer_type struct_ref_int_t; L.pointer_type struct_ref_int_t |] in

  
  let assignRefIntRef : L.llvalue = L.declare_function "assignRefIntRef" assignRefIntRef_t the_module in
  
  let assignRefFloatMut_t : L.lltype = L.function_type (void_t)
    [| L.pointer_type struct_ref_float_t; L.pointer_type struct_mut_float_t |] in

  
  let assignRefFloatMut : L.llvalue = L.declare_function "assignRefFloatMut" assignRefFloatMut_t the_module in
  
  let assignRefFloatRef_t : L.lltype = L.function_type (void_t)
    [| L.pointer_type struct_ref_float_t; L.pointer_type struct_ref_float_t |] in

  
  let assignRefFloatRef : L.llvalue = L.declare_function "assignRefFloatRef" assignRefFloatRef_t the_module in
  
  let assignRefBoolMut_t : L.lltype = L.function_type (void_t)
    [| L.pointer_type struct_ref_bool_t; L.pointer_type struct_mut_bool_t |] in

  
  let assignRefBoolMut : L.llvalue = L.declare_function "assignRefBoolMut" assignRefBoolMut_t the_module in
  
  let assignRefBoolRef_t : L.lltype = L.function_type (void_t)
    [| L.pointer_type struct_ref_bool_t; L.pointer_type struct_ref_bool_t |] in

  
  let assignRefBoolRef : L.llvalue = L.declare_function "assignRefBoolRef" assignRefBoolRef_t the_module in

    (* Read *)
  let readRefInt_t : L.lltype = L.function_type (i32_t)
    [| L.pointer_type struct_ref_int_t |] in 

  let readRefInt : L.llvalue = L.declare_function "readRefInt" readRefInt_t the_module in 

  let readRefFloat_t : L.lltype = L.function_type (float_t)
    [| L.pointer_type struct_ref_float_t |] in 

  let readRefFloat : L.llvalue = L.declare_function "readRefFloat" readRefFloat_t the_module in 

  let readRefBool_t : L.lltype = L.function_type (i1_t)
    [| L.pointer_type struct_ref_bool_t |] in 

  let readRefBool : L.llvalue = L.declare_function "readRefBool" readRefBool_t the_module in 

(* Foo functions *)
  let incFoo_t : L.lltype = L.function_type (void_t)
    [| L.pointer_type struct_foo_t |] in

  let incFoo : L.llvalue = L.declare_function "incFoo" incFoo_t the_module in

  let initFoo : L.llvalue = L.declare_function "initFoo" incFoo_t the_module in

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_fnction_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore ( match t with 
                | A.Mut(x) -> (match x with 
                                | A.Int -> ignore(L.build_call initMutInt [| local |] "" builder);
                                            (L.build_call assignMutInt [| local; p |] "" builder) 
                                | A.Float -> ignore(L.build_call initMutFloat [| local |] "" builder);
                                            (L.build_call assignMutFloat [| local; p |] "" builder) 
                                | A.Bool -> ignore(L.build_call initMutBool [| local |] "" builder);
                                            (L.build_call assignMutBool [| local; p |] "" builder) 
                                )
                | _ -> L.build_store p local builder);
        StringMap.add n local m

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in let _ = match t with 
                    | A.Mut(x) -> (match x with 
                                    | A.Int -> (L.build_call initMutInt [| local_var |] "" builder)
                                    | A.Float -> (L.build_call initMutFloat [| local_var |] "" builder)
                                    | A.Bool -> (L.build_call initMutBool [| local_var |] "" builder)
                                    )
                    | A.Ref(x) -> (match x with
                                    | A.Int -> (L.build_call initRefInt [| local_var |] "" builder)
                                    | A.Float -> (L.build_call initRefFloat [| local_var |] "" builder)
                                    | A.Bool -> (L.build_call initRefBool [| local_var |] "" builder) 
                                    )
                    | _ -> local_var
        in StringMap.add n local_var m

      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
      with Not_found -> StringMap.find n global_vars
    in
    
    (* Construct code for an expression; return its value *)
    let rec build_expr_args builder ((typ, e) : sexpr) = match e with
        SIntLit i  -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SFloatLit f -> L.const_float float_t f
      (*|SCharLit c -> L... c*)
      (*| SStrLit s -> s*)
      | SVar s       -> (match typ with 
                            | A.Mut(A.Int) -> (L.build_call readMutInt [| (lookup s) |] "" builder)

                            | A.Mut(A.Float) -> (L.build_call readMutFloat [| (lookup s) |] "" builder)
                            | A.Mut(A.Bool) -> (L.build_call readMutBool [| (lookup s) |] "" builder)
                            | _ -> L.build_load (lookup s) s builder)
      | SAssign (s, e) -> let e' = (match (fst e) with
                                    | A.Mut(x) when typ = A.Ref(x) -> 
                                        (match (snd e) with 
                                        | SVar y -> L.build_load (lookup y) y builder
                                        | _ -> L.build_load (lookup s) s builder )
                                    | _ -> build_expr_args builder e) in
                            ignore (match typ with 
                            | A.Mut(A.Int) -> (L.build_call assignMutInt [| (lookup s); e' |] "" builder)                                                                               
                            | A.Mut(A.Float) -> (L.build_call assignMutFloat [| (lookup s); e' |] "" builder    )                                                        
                            | A.Mut(A.Bool) -> (L.build_call assignMutBool [| (lookup s); e' |] "" builder)
                            | A.Ref(A.Int) when fst e = A.Mut(A.Int) 
                            -> (L.build_call assignRefIntMut [| (lookup s); e' |] "" builder)
                            | A.Ref(A.Int) when fst e = A.Ref(A.Int) 
                            -> (L.build_call assignRefIntRef [| (lookup s); e' |] "" builder)
                            | A.Ref(A.Float) when fst e = A.Mut(A.Float) 
                            -> (L.build_call assignRefFloatMut [| (lookup s); e' |] "" builder)
                            | A.Ref(A.Float) when fst e = A.Ref(A.Float) 
                            -> (L.build_call assignRefFloatRef [| (lookup s); e' |] "" builder)
                            | A.Ref(A.Bool) when fst e = A.Mut(A.Bool) 
                            -> (L.build_call assignRefBoolMut [| (lookup s); e' |] "" builder)
                            | A.Ref(A.Bool) when fst e = A.Ref(A.Bool) 
                            -> (L.build_call assignRefBoolRef [| (lookup s); e' |] "" builder)

                            | _ -> L.build_store e' (lookup s) builder); e'

      | SBinop (e1, op, e2) ->
        let e1' = build_expr_args builder e1
        and e2' = build_expr_args builder e2 in
        (match op with
           A.Add     -> L.build_add
         | A.Sub     -> L.build_sub
         | A.Mult    -> L.build_mul
         | A.Div     -> L.build_sdiv
         | A.FAdd    -> L.build_fadd
         | A.FSub    -> L.build_fsub
         | A.FMult   -> L.build_fmul
         | A.FDiv    -> L.build_fdiv
         | A.Eq      -> L.build_icmp L.Icmp.Eq
         | A.Neq     -> L.build_icmp L.Icmp.Ne
         | A.Lt      -> L.build_icmp L.Icmp.Slt
         | A.Gt      -> L.build_icmp L.Icmp.Sgt
         | A.Leq     -> L.build_icmp L.Icmp.Sle
         | A.Geq     -> L.build_icmp L.Icmp.Sge
         | A.FEq     -> L.build_fcmp L.Fcmp.Oeq
         | A.FNeq    -> L.build_fcmp L.Fcmp.One
         | A.FLt     -> L.build_fcmp L.Fcmp.Olt
         | A.FGt     -> L.build_fcmp L.Fcmp.Ogt
         | A.FLeq    -> L.build_fcmp L.Fcmp.Ole
         | A.FGeq    -> L.build_fcmp L.Fcmp.Oge
         | A.And     -> L.build_and
         | A.Or      -> L.build_or
         | A.Not     -> L.build_xor

        ) e1' e2' "tmp" builder
      | SCall ("print", [e]) ->
        L.build_call printf_func [| int_format_str ; (build_expr_args builder e) |]
          "printf" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (build_expr_args builder) (List.rev args)) in
        let result = f ^ "_result" in
        L.build_call fdef (Array.of_list llargs) result builder
    in

    (* Construct code for an expression; return its value *)
    let rec build_expr builder ((typ, e) : sexpr) = match e with
        SIntLit i  -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SFloatLit f -> L.const_float float_t f
      (*|SCharLit c -> L... c*)
      (*| SStrLit s -> s*)
      | SVar s       -> (match typ with 
                            | A.Mut(A.Int) -> (L.build_call readMutInt [| (lookup s) |] "" builder)

                            | A.Mut(A.Float) -> (L.build_call readMutFloat [| (lookup s) |] "" builder)
                            | A.Mut(A.Bool) -> (L.build_call readMutBool [| (lookup s) |] "" builder)
                            | A.Ref(A.Int) -> (L.build_call readRefInt [| (lookup s) |] "" builder)
                            | A.Ref(A.Float) -> (L.build_call readRefFloat [| (lookup s) |] "" builder)
                            | A.Ref(A.Bool) -> (L.build_call readRefBool [| (lookup s) |] "" builder)

                            | _ -> L.build_load (lookup s) s builder)
      | SAssign (s, e) -> let e' = (match (fst e) with
                                    | A.Mut(x) when typ = A.Ref(x) -> 
                                        (match (snd e) with 
                                        | SVar y -> L.build_load (lookup y) y builder
                                        | _ -> L.build_load (lookup s) s builder )
                                    | A.Ref(x) when typ = A.Ref(x) ->
                                        (match (snd e) with 
                                        | SVar y -> L.build_load (lookup y) y builder 
                                        | _ -> L.build_load (lookup s) s builder )
                                    | _ -> build_expr builder e) in
                            ignore (match typ with 
                            | A.Mut(A.Int) -> (L.build_call assignMutInt [| (lookup s); e' |] "" builder)                                                                               
                            | A.Mut(A.Float) -> (L.build_call assignMutFloat [| (lookup s); e' |] "" builder    )                                                        
                            | A.Mut(A.Bool) -> (L.build_call assignMutBool [| (lookup s); e' |] "" builder)
                            | A.Ref(A.Int) when fst e = A.Mut(A.Int) 
                            -> (L.build_call assignRefIntMut [| (lookup s); e' |] "" builder)
                            | A.Ref(A.Int) when fst e = A.Ref(A.Int) 
                            -> (L.build_call assignRefIntRef [| (lookup s); e' |] "" builder)
                            | A.Ref(A.Float) when fst e = A.Mut(A.Float) 
                            -> (L.build_call assignRefFloatMut [| (lookup s); e' |] "" builder)
                            | A.Ref(A.Float) when fst e = A.Ref(A.Float) 
                            -> (L.build_call assignRefFloatRef [| (lookup s); e' |] "" builder)
                            | A.Ref(A.Bool) when fst e = A.Mut(A.Bool) 
                            -> (L.build_call assignRefBoolMut [| (lookup s); e' |] "" builder)
                            | A.Ref(A.Bool) when fst e = A.Ref(A.Bool) 
                            -> (L.build_call assignRefBoolRef [| (lookup s); e' |] "" builder)

                            | _ -> L.build_store e' (lookup s) builder); e'
      | SBinop (e1, op, e2) ->
        let e1' = build_expr builder e1
        and e2' = build_expr builder e2 in
        (match op with
           A.Add     -> L.build_add
         | A.Sub     -> L.build_sub
         | A.Mult    -> L.build_mul
         | A.Div     -> L.build_sdiv
         | A.FAdd    -> L.build_fadd
         | A.FSub    -> L.build_fsub
         | A.FMult   -> L.build_fmul
         | A.FDiv    -> L.build_fdiv
         | A.Eq      -> L.build_icmp L.Icmp.Eq
         | A.Neq     -> L.build_icmp L.Icmp.Ne
         | A.Lt      -> L.build_icmp L.Icmp.Slt
         | A.Gt      -> L.build_icmp L.Icmp.Sgt
         | A.Leq     -> L.build_icmp L.Icmp.Sle
         | A.Geq     -> L.build_icmp L.Icmp.Sge
         | A.FEq     -> L.build_fcmp L.Fcmp.Oeq
         | A.FNeq    -> L.build_fcmp L.Fcmp.One
         | A.FLt     -> L.build_fcmp L.Fcmp.Olt
         | A.FGt     -> L.build_fcmp L.Fcmp.Ogt
         | A.FLeq    -> L.build_fcmp L.Fcmp.Ole
         | A.FGeq    -> L.build_fcmp L.Fcmp.Oge
         | A.And     -> L.build_and
         | A.Or      -> L.build_or
         | A.Not     -> L.build_xor

        ) e1' e2' "tmp" builder
      | SCall ("print", [e]) ->
        L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
          "printf" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (build_expr_args builder) (List.rev args)) in
        let result = f ^ "_result" in
        L.build_call fdef (Array.of_list llargs) result builder
    in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    let rec build_stmt builder = function
        SBlock sl -> List.fold_left build_stmt builder sl
      | SExpr e -> ignore(build_expr builder e); builder
      | SReturn e -> ignore(L.build_ret (build_expr builder e) builder); builder
      | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = build_expr builder predicate in

        let then_bb = L.append_block context "then" the_function in
        ignore (build_stmt (L.builder_at_end context then_bb) then_stmt);
        let else_bb = L.append_block context "else" the_function in
        ignore (build_stmt (L.builder_at_end context else_bb) else_stmt);

        let end_bb = L.append_block context "if_end" the_function in
        let build_br_end = L.build_br end_bb in (* partial function *)
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context end_bb

      | SWhile (predicate, body) ->
        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in (* partial function *)
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr while_builder predicate in

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;

        let end_bb = L.append_block context "while_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        L.builder_at_end context end_bb

     | SFor (f::s::t::l, body) ->
        let for_bb = L.append_block context "for" the_function in
        let build_br_for = L.build_br for_bb in (* partial function *)
        ignore (build_br_for builder);
        let for_builder = L.builder_at_end context for_bb in
        let bool_val = build_expr for_builder (s) in

        let init_bb = L.append_block context "for_init" the_function in
        add_terminal (ignore (build_expr (L.builder_at_end context init_bb) f); L.builder_at_end context init_bb ) build_br_for; 
        let body_bb = L.append_block context "for_body" the_function in
        add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_for;
        let update_bb = L.append_block context "for_update" the_function in
        add_terminal (ignore (build_expr (L.builder_at_end context update_bb) t); L.builder_at_end context update_bb ) build_br_for;

        let end_bb = L.append_block context "for_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb for_builder);
        L.builder_at_end context end_bb
    in
    (* Build the code for each statement in the function *)
    let func_builder = build_stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))

in

  List.iter build_fnction_body functions;
  the_module
