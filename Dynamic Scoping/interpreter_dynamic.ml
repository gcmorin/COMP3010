  type typ =
    | TBool
    | TInt
    | TArrow of typ * typ

  type exp =
    | True
    | False
    | If of exp * exp * exp
    | Num of int
    | IsZero of exp
    | Plus of exp * exp
    | Mult of exp * exp
    | Var of string
    | Lambda of string * exp
    | Apply of exp * exp
    | Let of string * exp * exp

  type type_environment = (string * typ) list
  type environment = (string * exp) list

  exception Eval_error
  exception Type_error
  exception Substitution_error
  exception Print_error

  let rec free_variables (e : exp) =
    match e with
    | Var x -> [x]
    | Num n -> []
    | True -> []
    | False -> []
    | Plus(e1, e2) -> List.sort_uniq compare((free_variables e1) @ (free_variables e2))
    | Mult(e1, e2) -> List.sort_uniq compare((free_variables e1) @ (free_variables e2))
    | Apply(e1,e2) -> List.sort_uniq compare((free_variables e1) @ (free_variables e2))
    | Let(x, e1, e2) -> List.sort_uniq compare((free_variables e1) @ (free_variables e2))
    | IsZero(e) -> free_variables e
    | If(e, e1, e2) -> List.sort_uniq compare((free_variables e) @ (free_variables e1) @ (free_variables e2))
    | Lambda(var, body) -> List.filter(fun y -> var <> y) ([var]);;


  let rec substitution (e1 : exp) (x : string) (e2 : exp) =
    match e1 with
    | Num i -> Num i
    | True -> True
    | False -> False
    | Plus(exp1,exp2) -> Plus(substitution exp1 x e2, substitution exp2 x e2)
    | Mult(exp1,exp2) -> Mult(substitution exp1 x e2, substitution exp2 x e2)
    | Apply(exp1,exp2) -> Apply(substitution exp1 x e2, substitution exp2 x e2)
    | IsZero(exp1) -> IsZero(substitution exp1 x e2)
    | If(exp1, exp2, exp3) -> If(substitution exp1 x e2, substitution exp2 x e2, substitution exp3 x e2)
    | Var y -> if y = x then e2 else Var y
    | Let(var, exp1, exp2) -> raise Substitution_error
    | Lambda(var, body) ->
                      if ( (var=x) || (List.mem var (free_variables e2)) ) then Lambda(var,body)
                      else if (not (List.mem var (free_variables e1))) && var!=x then Lambda(var, (substitution body x e2))
                      else raise Substitution_error;;

  let rec step (env : environment) (e : exp) : (environment * exp) =
    match e with
    | Num i -> raise Eval_error
    | Var s -> (try
        env, List.assoc s env
    with
    | _ -> raise Eval_error)
    | True -> raise Eval_error
    | False -> raise Eval_error
    | Lambda(var, body) -> raise Eval_error
    | Plus(Num i, Num j) ->
      let n1 = i + j in env, Num n1
    | Plus(e1, Num i) ->
      if e1 = True || e1 = False then raise Eval_error else
      let p = step env e1 in fst p, Plus(snd p, Num i)
    | Plus(Num i, e1) ->
      if e1 = True || e1 = False then raise Eval_error else
      let p = step env e1 in fst p, Plus(Num i, snd p)
    | Plus(e1, e2) ->
      if e1 = True || e1 = False || e2 = True || e2 = False then raise Eval_error else
      let p = step env e1 in fst p, Plus(snd p, e2)
    | Mult(Num i, Num j) ->
      let n1 = i * j in env, Num n1
    | Mult(e1, Num i) ->
      if e1 = True || e1 = False then raise Eval_error else
      let p = step env e1 in fst p, Mult(snd p, Num i)
    | Mult(Num i, e1) ->
      if e1 = True || e1 = False then raise Eval_error else
      let p = step env e1 in fst p, Mult(Num i, snd p)
    | Mult(e1, e2) ->
      if e1 = True || e1 = False || e2 = True || e2 = False then raise Eval_error else
      let p = step env e1 in fst p, Mult(snd p, e2)
    | IsZero(Num i) ->
      if i = 0 then env, True
      else if i != 0 then env, False
      else raise Eval_error
    | IsZero(True) -> raise Eval_error
    | Let(x, e1, e2) ->  env, (Apply(Lambda(x, e2), e1))
    | Apply(Lambda(var, body), True) -> ((var, True)::env), body
    | Apply(Lambda(var, body), False) -> ((var, False)::env), body
    | Apply(Lambda(var, body), Num i) -> ((var, Num i)::env), body
    | Apply(Lambda(var, body), Lambda(v, b)) -> ((var, Lambda(v, b))::env), body
    | Apply(Lambda(var, body), e2) -> let p = step env e2 in ((var, e2)::env), Apply(Lambda(var, body), snd p)
    | Apply(e1, e2) ->let p = step env e1 in env, Apply(snd p, e2)
    | IsZero(False) -> raise Eval_error
    | IsZero(e1) -> let p = step env e1 in env, IsZero(snd p)
    | If(True, e1, e2) -> env, e1
    | If(False, e1, e2) -> env, e2
    | If(e, e1, e2) -> (match e with
      | Num i -> raise Eval_error
      | _ -> let p = step env e in env, (If(snd p, e1, e2)));;

  let rec multi_step (env : environment) (e : exp) : (environment * exp) =
    match e with
    | Num i -> env, Num i
    | True -> env, True
    | False -> env, False
    | Lambda(var, body) -> env, Lambda(var, body)
    | _ -> let exprafteronestep = step env e in
            multi_step (fst exprafteronestep) (snd exprafteronestep);;


  let rec string_of_type (t : typ) =
    match t with
    | TBool -> "TBool"
    | TInt -> "TInt"
    | TArrow(t1, t2) -> "TArrow(" ^ string_of_type t1 ^ ", " ^ string_of_type t2 ^ ")";;

  let rec string_of_fun (e : exp) =
    match e with
    | IsZero (expression) ->
       "IsZero(" ^ string_of_fun expression ^ ")"
    | Plus (left, right) ->
       "Plus(" ^ string_of_fun left ^ ", " ^ string_of_fun right ^ ")"
    | Mult (left, right) ->
       "Mult(" ^ string_of_fun left ^ ", " ^ string_of_fun right ^ ")"
    | If (left, center, right) ->
       "If(" ^ string_of_fun left ^ ", " ^ string_of_fun center ^ ", " ^ string_of_fun right ^ ")"
    | Lambda(var, body) -> "Lambda( \"" ^ var ^ "\", " ^ string_of_fun body ^ ")"
    | Apply(left, right) -> "Apply (" ^ string_of_fun left ^ ", " ^ string_of_fun right ^ ")"
    | True -> "True"
    | False -> "False"
    | Let(var, e1, e2) -> raise Print_error
    | Var s -> "Var \"" ^ s ^"\""
    | Num i -> "Num " ^ string_of_int i;;

  let rec string_of_exp (e : exp) =
    match e with
    | IsZero (expression) ->
       "(isZero " ^ string_of_exp expression ^ ")"
    | Plus (left, right) ->
       "(" ^ string_of_exp left ^ " + " ^ string_of_exp right ^ ")"
    | Mult (left, right) ->
       "(" ^ string_of_exp left ^ " * " ^ string_of_exp right ^ ")"
    | If (left, center, right) ->
       "if " ^ string_of_exp left ^ " then " ^ string_of_exp center ^ " else " ^ string_of_exp right
    | Lambda(var, body) -> "Lambda( \"" ^ var ^ "\", " ^ string_of_fun body ^ ")"
    | Apply(left, right) -> raise Print_error
    | True -> "True"
    | False -> "False"
    | Let(var, e1, e2) -> raise Print_error
    | Var s -> "(Var " ^ s ^")"
    | Num i -> "(Num " ^ string_of_int i ^ ")";;

  let rec print_list_string myList = match myList with
    | [] -> print_endline " "
    | head::body ->
    begin
    print_endline head;
    print_list_string body
    end;;

  let print_multi_step e =
      print_endline(string_of_exp((multi_step [] e) |> snd));;

  let print_substitution e1 x e2 =
      print_endline(string_of_fun(substitution e1 x e2));;
