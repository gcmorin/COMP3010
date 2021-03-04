The type_check function is working correctly returning type typ.
The way type_check works is it takes a type exp and looks to see if it raises a type error first then it calls step and tests for type again. 
This way it will not raise an eval error.

You can use the function print_type_check to print out the tests. This function calls a new function I made string_of_type with function type_check as a parameter to print out the type of the expression.
For example with test 1 the code is print_type_check (True);; and it returns TBool

The code passes all 20 Tests in order.