Dynamic Typing is working 100%. 

The test file on the website seems to be broken so I had to run the tests manually. 

You can use the function print_multi_step to print out all of the tests if you do not run the test file. 
All you need to do is call it like so

print_multi_step((Apply
      ( Apply
          ( Lambda
              ( "mybool"
              , Lambda
                  ("myfunction", Apply (Var "myfunction", Var "mybool"))
              )
          , Lambda ("x", Plus (Var "x", Var "x")) )
      , True )));;



The code passes all 7 Tests in order.