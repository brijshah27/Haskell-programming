module ArithExprTests where

import DataDeclrs (Elmt(..))
import qualified ParseState_v1Type as V1 (ParseState_v1, evalArith)
import qualified ParseState_v2Type as V2 (ParseState_v2, evalArith)

---------------------------------------------------------------------
-- ArithExpr Tests
---------------------------------------------------------------------

-- Displays the actual expression, the parsed expression using ParseState_v1Type
-- and display the result of arithmatic expression evaluation
showEvalArithV1 :: String -> String
showEvalArithV1 string = showResult $ V1.evalArith string


showEvalArithV2 :: String -> String
showEvalArithV2 string = showResult $ V2.evalArith string


showResult :: (String, Elmt, Int) -> String
showResult (str, expr, val) = 
          "     " 
       ++ show str 
       ++ " --> "
       ++ (stripOuterParens . show) expr -- Strip the outer parentheses
       ++ " --> " 
       ++ show val

-- Removes the "(" and ")" around a given expression 
stripOuterParens :: String -> String
stripOuterParens ('(':rest) = init rest
stripOuterParens str        = str

                             -- left    right
testCases :: [String]        -- assoc   assoc
testCases = [ "8-4-2"        --   2       6
            , "(8-4)-2"      --   2       2
            , "8-(4-2)"      --   6       6
            , "8/4/2"        --   1       4
            , "(8/4)/2"      --   1       1
            , "8/(4/2)"      --   4       4
            , "2^3^2"        --  64     512
            , "(2^3)^2"      --  64      64
            , "2^(3^2)"      -- 512     512
            , "16-7-3*(2^2)" --  -3      21

      -- Associativity not relevant for the rest.
            , "3*4+5^2"      --  37            
            , "6-3*2"        --   0       
            , "6*3-2"        --  16      
            , "6*(3-1)"      --  12      
            , "6-3*(1-7)"    --  24      
            , "(6-3)*2"      --   6       
            , "(6-3)^2+5"    --  14      
            , "5+(6-3)^2+5"  --  19      
            , "3+4/(6-4)^2"  --   4        
            , "3+4/6-4^2"    -- -13      
            , "2^(6-3)"      --   8       
            , "2^6-3"        --  61      
            , "(6-3)^2"      --   9       
            , "16-3^2"       --   7       
            , "3"            --   3 
            , "3+4/6-4^2"     -- This example does not parse.
                             -- Instead it produces an error
                             -- message, which shows how far  
                             -- it got: [(,(3+(4/6)),-,4,^,)]   
            ]    


testArithExprs :: String -> IO ()
testArithExprs version = 
  let strings = map (case version of
                     "V1" -> showEvalArithV1 
                     "V2" -> showEvalArithV2
                    ) 
                    testCases
  in putStr . unlines $ "   Input string --> Expr --> Value" : strings