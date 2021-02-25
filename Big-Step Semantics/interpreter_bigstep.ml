type exp =
  | True
  | False
  | If of exp * exp * exp
  | Num of int
  | IsZero of exp
  | Plus of exp * exp
  | Mult of exp * exp

  exception Eval_error of string

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
    | True ->
       "true"
    | False ->
       "false"
    | Num i -> string_of_int i;;


  let rec eval (e : exp) =
    match e with
    | IsZero (e1) ->
        let n1 = eval e1 in
        n1
    | Plus (e1, e2) ->
        let n1 = eval e1 in
        let n2 = eval e2 in
        let n3 = n1 + n2 in
        n3
    | Mult (e1, e2) ->
        let n1 = eval e1 in
        let n2 = eval e2 in
        let n3 = n1 * n2 in
        n3
    | Num i -> i
    | True -> 1
    | False -> 0
    | If (e1, e2, e3) ->
        match e1 with
        | True -> eval e2
        | False -> eval e3
        | _ -> invalid_arg("Eval Error");;

  let print_expr e =
    print_endline (string_of_exp e);;
  let print_eval e =
    print_endline (string_of_int(eval e));;



let () =
  (*1 print_eval(True);;
    2 print_eval(False);;*)
  (*3 Works*)print_eval(Num 0);;
  (*4 print_eval(IsZero (Num 0));;
    5 print_eval(IsZero (Plus (Num 1, Num 1)));;
    6 print_eval(IsZero (Plus (Plus (Num 2, Num (-1)), Num 1)));;*)
  (*7 Works*)print_eval(Plus (Plus (Num (-1), Num 1), Plus (Num (-1), Num 1)));;
  (*8 Works*)print_eval(Plus (Num (-1), Plus (Mult (Num 2, Num 2), Num 1)));;
  (*9 Works*)print_eval(Plus (Plus (Plus (Num 2, Num (-1)), Num 1), Num (-1)));;
  (*10 Works print_eval(eval(Plus (IsZero (Plus (Num (-1), Num 1)), Num 1));; *)
  (*11 print_eval(IsZero (If (IsZero (Num 0), True, Num 0)));; *)
  (*12 Works  print_eval(IsZero(If( IsZero (Mult (Num 5, Num 0)), If (False, Num 0, IsZero (Plus (Num (-1), Num 0))), Num 0 )));; *)
  (*13 print_eval(If (IsZero (Plus (Num (-1), Num 1)), Num 2, True));; *)
  (*14 print_eval(If( If (IsZero (Mult (Plus (Num 1, Num (-1)), Num 1)), False, True), Mult (Num 1, Num 2), True ));; *)
  (*15 print_eval(If( If (IsZero (Mult (Num 0, Num 0)), IsZero (Num 2), Num 0), Mult (Num 2, Mult (Num 1, Num 1)), Plus( Plus( Plus( Plus (If (IsZero (Num 0), Num 1, Num 0), Num (-1)), Num 1 ), Num (-1) ), Num 1 ) ));; *)
  (*16 Works *)print_eval(If( True, If (True, Mult (If (False, Num 0, Num 1), Num 1), Num 5), Plus (Mult (Num 4, Num 1), Num 1) ));;
  (*17 print_eval(If( IsZero (If (IsZero (Plus (Num (-1), Num 2)), Num 0, Num 1)), If( True, If (False, Mult (Num 0, Num 6), Plus (Num 0, Num 1)), Num 5 ), Num 5 ));; *)
  (*18 print_eval(If( IsZero (Plus (Num (-1), Plus (Num 1, Plus (Num (-1), Num 1)))), IsZero True, Num 1 ));; *)
  (*19 print_eval(Plus( Num 1, Plus( Num (-1), If( IsZero (Plus (Num 1, If (True, Num 1, Num 2))), Plus (Num 1, Num 2), Mult (Num 2, Num 2) ) ) ));; *)
  (*20 print_eval(Plus( Num (-1), If( IsZero (Plus (Num 5, Num (-4))), Mult (Num 123, Plus (Num 5, Num (-4))), IsZero (Num 0) ) ));; *)
