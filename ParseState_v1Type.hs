module ParseState_v1Type (ParseState_v1, evalArith) where

import DataDeclrs (Elmt, evalExpr)
import Tokenizer (tokenize)
import ParseStateClass (ParseState(..), replace) 

---------------------------------------------------------------------
-- Define  ParseState_v1  to be a PairOfLists of [Elmt] lists.
-- Define how  ParseState_v1  implements  ParseState.  
---------------------------------------------------------------------

-- One possible concrete implementation of ParseStateClass
-- where parse state is maintained as a Pair of Elmt lists.
data ParseState_v1 = PairOfLists [Elmt] [Elmt] deriving (Eq, Show)

---------------------------------------------------------------------
-- evalArith is the top-level function. 
-- Because it generates an intermediate ParseState, 
-- it must be defined for each ParseState type.
---------------------------------------------------------------------

-- Evalutes an arithmetic expression using the following steps:
-- Step-1:
--     Tokenize input expression as string into [Elmt]
-- Step-2:
--     Precedence parse the [Elmt] into ParseState were "operand op operand" are 
--     wrapped in paranthesis based on the operator precedence
-- Step-3:
--     Evaluate final expression stored in window and print 
--     (original expr str, parsed expr, result)
-- let p = parse $ tokenize "3+5*4" :: ParseState_v1
-- p ==> Pair [] [(3+(5*4))]
-- segment p ==> ([],[(3+(5*4))],[])
evalArith :: String -> (String, Elmt, Int) 
evalArith string = 
  let parseState = (parse . tokenize $ string) :: ParseState_v1
      (left, window, _) = segment parseState
  in case window of
     []     -> error $ show string ++ " => " ++ (replace "," ", " $ show left)
     [expr] -> (string, expr, evalExpr expr)


-- Define how to perform the ParseState functions on ParseState_v1 objects
instance ParseState ParseState_v1 where

  -- The starting parse state of the processing with the initial parsed tokens
  -- from the given arithmetic expression
  initialParseState tokens = PairOfLists [] tokens


  -- Splits the list of elements into variable "window" containing first 5 elements 
  -- and the remaining elements in variable "rest"
  -- Ex-1: segment (Pair [] (tokenize "(3+5)*2")) ==> ([],[(,(,3,+,5],[),*,2,)])
  -- Ex-2: segment (Pair [] (tokenize "3+5*2")) ==> ([],[(,3,+,5,*],[2,)])
  segment (PairOfLists left right) = 
    let (window, rest) = splitAt 5 right
    in (reverse left, window, rest)


  -- Moves specified number of elements from left to right when n is negative
  -- and from right to left when n is positive
  -- The first boundary case does nothing when n is zero or
  -- n is negative but left is empty or n is psotive but right is empty.
  -- The second case is a recursive call after moving one element from left to right
  -- The third case is a recursive call after moving one element from right to left
  slideWindow n (PairOfLists left right) 
    |    n == 0 
      || n < 0 && null left 
      || n > 0 && null right = PairOfLists left right
  slideWindow n (PairOfLists (x:xs) right) 
    | n < 0 = slideWindow (n+1) (PairOfLists xs (x:right))
  slideWindow n (PairOfLists left (x:xs)) 
    | n > 0 = slideWindow (n-1) (PairOfLists (x:left) xs)


  -- Merge elements back to geather.
  -- Ex- unSegment (Pair [],[(,(,3,+,5],[),*,2,)]) ==> [] [(,(,3,+,5,),*,2,)])
  unSegment  (left, window, right) = PairOfLists (reverse left) (window ++ right)

