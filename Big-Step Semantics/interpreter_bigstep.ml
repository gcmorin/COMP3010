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
        if (eval e1 = "True" || eval e1 = "False")
          then
            raise(Eval_error "Invalid expected expression")
          else
            let n1 = eval e1 in
              if int_of_string(n1) = 0
                then
                  "True"
                else
                  "False"
    | Plus (e1, e2) ->
        if (eval e1 = "True" || eval e1 = "False" || eval e2 = "True" || eval e2 = "False")
          then
            raise(Eval_error "Invalid expected expression")
          else
            let n1 = eval e1 in
            let n2 = eval e2 in
            let n3 = int_of_string(n1) + int_of_string(n2) in
            string_of_int n3
    | Mult (e1, e2) ->
        if (eval e1 = "True" || eval e1 = "False" || eval e2 = "True" || eval e2 = "False")
          then
            raise(Eval_error "Invalid expected expression")
          else
            let n1 = eval e1 in
            let n2 = eval e2 in
            let n3 = int_of_string(n1) * int_of_string(n2) in
            string_of_int n3
    | Num i -> string_of_int i
    | True -> "True"
    | False -> "False"
    | If (e1, e2, e3) ->
        match eval e1 with
        | "True" -> eval e2
        | "False" -> eval e3
        | _ -> raise(Eval_error "Invalid expected expression");;

  let print_expr e =
    print_endline (string_of_exp e);;
  let print_eval e =
    print_endline ((eval e));;

  let () =
  print_eval(IsZero
      (If
         ( IsZero (Mult (Num 5, Num 0))
         , If (False, Num 0, IsZero (Plus (Num (-1), Num 0)))
         , Num 0 )));;
