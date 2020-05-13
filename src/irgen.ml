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

  (* Declare struct Foo *)
    (*
  let struct_ref_t : L.lltype = 
      L.named_struct_type context "Ref" in

  let _ = 
      L.struct_set_body struct_ref_t
      [| |]
      *)
  let struct_foo_t : L.lltype = 
      L.named_struct_type context "Foo" in

  let _ = 
      L.struct_set_body struct_foo_t
      [| i32_t |] false in

  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
      A.Mut(x)   -> i32_t
      (*TODO*)
    | A.Ref(x)  -> i32_t
      (*TODO*)
    | A.RType(x) -> match x with 
                    | A.Foo -> struct_foo_t
                    | A.Int -> i32_t
                    | A.Bool -> i1_t
                    | A.Char -> i8_t
                    | A.Float -> float_t
                    | A.String -> i8_t

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

  let incFoo_t : L.lltype = L.function_type (L.void_type context)
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
        ignore (L.build_store p local builder);
        StringMap.add n local m

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in let _ = if t = A.RType(A.Foo) then 
                            (ignore (L.build_call initFoo [| local_var |] "" builder);
                            L.build_call incFoo [| local_var |] "" builder)
                else local_var
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
    let rec build_expr builder ((_, e) : sexpr) = match e with
        SIntLit i  -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SFloatLit f -> L.const_float float_t f
      (*|SCharLit c -> L... c*)
      (*| SStrLit s -> s*)
      | SVar s       -> L.build_load (lookup s) s builder
      | SAssign (s, e) -> let e' = build_expr builder e in
        ignore(L.build_store e' (lookup s) builder); e'
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
        let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
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
