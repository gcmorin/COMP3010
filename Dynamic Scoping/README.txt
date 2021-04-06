Dynamic Scoping is working 100%. 

It passes all tests by running the test file given on the website. 

You can use the function print_multi_step to print out all of the tests if you do not run the test file. 
All you need to do is call it like so

print_multi_step(Plus
        ( Apply
            ( Lambda
                ( "n"
                , If
                    ( IsZero (Var "n")
                    , Let ("x", Num 5, Var "n")
                    , Let
                        ( "n"
                        , Num 6
                        , Let ("x", Plus (Var "n", Num 1), Var "n") )
                    ) )
            , Num 0 )
        , Var "x" ));;



The code passes all 7 Tests in order.