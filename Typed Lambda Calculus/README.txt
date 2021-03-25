Typed Lambda Calculus is working 100%. 

free_variables, substitution, multi_step, and type_check all work correctly

You can use the function print_substitution to print out the tests 1 - 4. 

Example to call test 1: print_substitution (Mult (Plus (Num 5, Var "x"), Plus (Num 3, Var "x"))) "x" (Num 2);;

You can use the function print_multi_step to print out the tests 5 - 12. 

Example to call test 5: print_multi_step(Apply( Lambda("x", TInt, Mult (Plus (Num 5, Var "x"), Plus (Num 3, Var "x"))), Num 2 ));;

You can use the function print_type_checkto print out the tests 13 - 20. 

Example to call test 13: print_type_check(Apply( Lambda("x", TInt, Mult (Plus (Num 5, Var "x"), Plus (Num 3, Var "x"))), Num 2 ));;

The code passes all 20 Tests in order.