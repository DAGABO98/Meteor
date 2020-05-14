open Ast
open Printf

module StringMap = Map.Make(String)


(* Takes an AST and returns a TAST (typed AST) *)
let infer (globals, functions) =
    let change_function (func : func_def) =

      (* Build local symbol table of variables for this function *)
      let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
          StringMap.empty (globals @ func.formals @ func.locals )
      in

      (* Return a variable from our local symbol table *)
      let type_of_identifier s =
        try StringMap.find s symbols
        with Not_found -> raise (Failure ("Not Found"))
      in

      (* Add function name to symbol table *)
      let add_func map fd =
        let built_in_err = "function " ^ fd.fname ^ " may not be defined"
        and dup_err = "duplicate function " ^ fd.fname
        and make_err er = raise (Failure er)
        and n = fd.fname (* Name of the function *)
        in match fd with (* No duplicate functions or redefinitions of built-ins *)
        | _ when StringMap.mem n map -> make_err dup_err
        | _ ->  StringMap.add n fd map
      in
        
      (* Collect all function names into one symbol table *)
      let function_decls = List.fold_left add_func StringMap.empty functions
      in

      (* Return a function from our symbol table *)
      let find_func s =
        try StringMap.find s function_decls
        with Not_found -> raise (Failure ("unrecognized function " ^ s))
      in

      let rec get_type = function
          IntLit l -> RType Int
        | FloatLit l -> RType Float
        | BoolLit l -> RType Bool
        | Var var -> (type_of_identifier var)
        | Assign(var, e) -> get_type e
        | Binop(e1, op, e2) -> get_type e1
        | Call(fname, args) -> (find_func fname).rtyp

      in

      (* Return a semantically-checked expression, i.e., with a type *)
      let rec check_expr new_ds = function
          IntLit l -> (IntLit l, [])
        | FloatLit l -> (FloatLit l, [])
        | BoolLit l -> (BoolLit l, [])
        | Var var -> 
                let inferred_type = get_type (Var var)
                in
                    if (List.mem (inferred_type, var) (new_ds @ globals @ func.formals))
                    then Var var, []
                    else Var var, [(get_type (Var var), var)]
        | Assign(var, e) as ex ->
            (try 
                ignore (type_of_identifier var);
                 Printf.printf "%s\n" "type already declared returning empty";
                (ex, [])

            with Failure "Not Found" -> 
                 Printf.printf "%s\n" "type not found inferring...";
              let inferred_type = get_type e
              in
                if (List.mem (inferred_type, var) new_ds) then ex, []
                else ex, [(inferred_type, var)])

        | Binop(e1, op, e2) -> 
                 Printf.printf "%s\n" "Bin op...";

                let exp1, new_locals1 = check_expr new_ds e1
                in
                 Printf.printf "expr: %s returned new_locals1: %s\n" (string_of_expr exp1) (String.concat "" (List.map string_of_vdecl new_locals1));
                                let exp2, new_locals2 = check_expr (new_ds @ new_locals1) e2
                                in
                 Printf.printf "expr2: %s returned new_locals2: %s\n" (string_of_expr exp1) (String.concat "" (List.map string_of_vdecl new_locals2));
                                (Binop(exp1, op, exp2), new_ds @ new_locals1 @ new_locals2)

        | Call(fname, args) as call -> (call, [])
      in
            
      let rec check_stmt_list = function
        ([], new_ds) -> ([], new_ds) 
        (* Flatten blocks *)
        | (Block sl :: sl', new_ds)  -> check_stmt_list (sl @ sl', new_ds)
        | (s :: sl, new_ds) -> 
                Printf.printf "start with s: %s and new locals: %s\n" (String.concat "" (List.map string_of_stmt [s])) (String.concat "" (List.map string_of_vdecl new_ds));
                                let s1, new_locals = check_stmt new_ds s
                               in
                                Printf.printf "got new locals: %s\n" (String.concat "" (List.map string_of_vdecl new_locals));
                                
                                Printf.printf "and new_ds: %s\n" (String.concat "" (List.map string_of_vdecl new_ds));
                               let s2, new_locals2 = (check_stmt_list (sl, new_ds @ new_locals))
                               in
                               (s1 :: s2, new_locals2)

        (* Return a semantically-checked statement i.e. containing sexprs *)
       and check_stmt new_ds = function
       (* A block is correct if each statement is correct and nothing
          follows any Return statement.  Nested blocks are flattened. *)
       (* TODO *)
         Block sl as block -> 
             Printf.printf "going into block:\n";
             (block, [])
         | Expr e -> let e1, new_locals = check_expr new_ds e
                     in (Expr e1, new_locals)
         | If(e, st1, st2) as ifst -> (ifst, [])
         | While(e, st) as whilest -> (whilest, [])
         | For(est::ndt::tdt::_, st) as forst-> (forst, [])
         | Return e -> 
                Printf.printf "Return: expr %s\n" (string_of_expr e);
                 let e1, rlocals = check_expr new_ds e
                     in 
                    Printf.printf "check_expr new locals: %s\n" (String.concat "" (List.map string_of_vdecl rlocals));

                     (Return e1, rlocals)
         | _ -> raise (Failure ("Block is incomplete or in the worng format"))

    in (* body of change_func *)

    let new_body, new_locals = check_stmt_list (func.body, func.locals)
    in
    Printf.printf "Returning func: %s\n" (String.concat "" (List.map string_of_stmt new_body)
        ^ String.concat "" (List.map string_of_vdecl new_locals));
    { rtyp = func.rtyp;
      fname = func.fname;
      formals = func.formals;
      locals  = new_locals;
      body = new_body
    }
in
(globals, List.map change_function functions)
