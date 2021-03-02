The multi_step function and step function work properly, both returning type exp.
The step function only does one step in the expression and then returns it to multi_step.
multi_step then looks to see if step returned a value, if so multi_step returns the value, if not it calls step again.

You can use the function print_multi_step to print out the tests. This function calls function string_of_exp with function multi_step as a parameter to print out the expression.
For example with test 1 the code is print_multi_step (True);;

The code passes all 20 Tests in order.