module Tokenizer (tokenize) where

import DataDeclrs (Elmt(..), Optr(Optr), evalExpr) 

import Data.Map as M (fromList, lookup)


---------------------------------------------------------------------
-- These functions convert a string into a list of Elmts.
-- The main function is tokenize.
---------------------------------------------------------------------

-- Adds spaces around (before and after) each operator and folds the 
-- input artithmetic expression into a list of chars delimited by space
-- addSpaces "3*4+5" => "3 * 4 + 5" OR ['3',' ','*',' ','4',' ','+',' ','5'] 
addSpaces :: String  -> String
addSpaces = foldr (\c str -> if c `elem` " 0123456789" 
                             then c:str 
                             else [' ', c, ' '] ++ str) 
                  []

-- Looks for a string in key-value of map
--  If a match is found then the Elmt from the map is returned by Just op -> op
-- If no match is found, then the token should be an operand number
-- The makeToken assumes that the input is a string of one of the
-- values ['(',')','+','-','*','^','[0-9]+']
-- read knows its type from definition of Elmt = Nbr Int
makeToken :: String -> Elmt
makeToken str = 
  case M.lookup str $ fromList [ ( "(", LPar),             ( ")", RPar) 
                               , ( "+", Op (+) '+' 1 'L'), ( "-", Op (-) '-' 1 'L') 
                               , ( "*", Op (*) '*' 2 'L'), ( "/", Op div '/' 2 'L') 
                               , ( "^", Op (^) '^' 3 'R')
                               ] of
  Just op -> op
  Nothing -> Nbr (read str) -- How do we know we should perform: read str? 
                            -- How do we know what type  read  returns? 


-- Converts a expression of the form "3+4*5" to a list of "Elmt"s of the form
-- [(,3,+,4,*,5,)] where each element in the list is of type Elmt
-- Step 1: Add spaces around operator. "3+4*5" becomes "3 + 4 * 5"
-- Step 2: function words splits a input string into a list of strings 
--         delimited by space. "3 + 4 * 5" becomes ["3","+","4","*","5"]
-- Step 3: map applies makeToken on each of the string in the above list
--         and converts it to a list of "Elmt"s
-- Step 4: concat adds Elmt [(] and [)] around the above list of element and 
--         produces the final list of "Elmt"s with paranthesis as [(,3,+,4,*,5,)]
tokenize :: String -> [Elmt]
tokenize string =  
  concat [[LPar], map makeToken . words . addSpaces $ string, [RPar]]