module ParseState_v2Type (ParseState_v2, evalArith) where

import DataDeclrs (Elmt, evalExpr)
import Tokenizer (tokenize)
import ParseStateClass (ParseState(..), replace) 

---------------------------------------------------------------------
-- Define  ParseState_v1  to be an [Elmt] list with an index for the
-- start of the window. Define how  ParseState_v2  implements  ParseState.  
---------------------------------------------------------------------

-- <Your comment>
data ParseState_v2 = IndexAndList Int [Elmt] deriving (Eq, Show)

---------------------------------------------------------------------
-- evalArith is the top-level function. 
-- Because it generates an intermediate ParseState, 
-- it must be defined for each ParseState type.
---------------------------------------------------------------------

-- <Your comment>
evalArith :: String -> (String, Elmt, Int) 
evalArith string = 
  let parseState = (parse . tokenize $ string) :: ParseState_v2
      (left, window, _) = segment parseState
  in case window of
     []     -> error $ show string ++ " => " ++ (replace "," ", " $ show left)
     [expr] -> (string, expr, evalExpr expr)


-- Define how to perform the ParseState functions on ParseState_v2 objects
instance ParseState ParseState_v2 where

  -- <Your comment>
  initialParseState tokens = IndexAndList 0 tokens


  -- <Your comment> 
  segment (IndexAndList index list) = 
    let (window, rest) = splitAt 5 . drop index $ list
    in (take index list, window, rest)


  -- <Your comment>
  slideWindow n (IndexAndList index list) 
    |    n == 0 
      || n < 0 && index == 0 
      || n > 0 && index > length list = IndexAndList index list
  slideWindow n (IndexAndList index list) 
    | n < 0 = IndexAndList (max 0 (index + n)) list -- n < 0
  slideWindow n (IndexAndList index list) 
    | n > 0 = IndexAndList (min (length list) (index + n)) list


  -- <Your comment>
  unSegment  (left, window, right) = IndexAndList (length left) (left ++ window ++ right)

