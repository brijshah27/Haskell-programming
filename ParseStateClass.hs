module ParseStateClass (ParseState (..), replace) where

import DataDeclrs (Elmt(..), Optr (Optr)) 

import Debug.Trace


-----------------------------------------------------------------------
-- The ParseState TypeClass. A TypeClass is like an Interface in Java.
-- Define a  ParseState  as supporting these functions. Also, require 
-- that every  ParseState  satisfies (i.e., implements)  Eq.
-----------------------------------------------------------------------

-- These functions will depend on the specific implementation
-- of a ParseState.
class Eq parseState => ParseState parseState where
  initialParseState :: [Elmt] -> parseState
  segment :: parseState -> ([Elmt], [Elmt], [Elmt])
  slideWindow :: Int -> parseState -> parseState
  unSegment :: ([Elmt], [Elmt], [Elmt]) -> parseState


  -- <Your comment>
  -- The loop condition to check if we should continue processing the expression.
  -- The processing stops when the whole expression in right is reduced to just a 
  -- single expression from [Elmt]
  -- The program should not continue processing when
  --    (1) There are no Elmts to be processed in the parsed tokens
  --    (2) When there is just one Elmt in the parsed token (Ex: (Pair [] [(3+5)]))
  --    True in all other cases (more than one Elmt in parsed tokens)
  continueParse :: parseState -> Bool
  continueParse pState =
    case segment pState of
    (_,  [],  []) -> False   -- Parse failed. Stop.
    ([], [_], []) -> False   -- Parse succeeded. Stop.
    otherwise     -> True    -- Parse not finished. Continue.


  -- Parse  is implementation-independent and can be used by 
  -- all  ParseState  instances.
  -- <Your comment>
  parse :: [Elmt] -> parseState  
  parse tokens = 
    let initialPState = initialParseState tokens
    in perhapsTrace ("     " ++ showParseState initialPState) $
       while initialPState
             continueParse
             parseAStep


  showParseState :: parseState -> String
  showParseState pState = 
    let (left, window, right) = segment pState   
    in replace "," ", " $ show left ++ "   " ++ show window ++ "    " ++ show right


-- -----------------------------------------------------------------------
-- -- Utility functions used by ParseState functions.
-----------------------------------------------------------------------

-- <Your comment>
applyARule :: [Elmt] -> [Elmt]
-- <Your comment. What does this rule do?>
  -- This rule removes the paranthesis around a single Elmt, for example
  -- Ex-1: applyARule (Pair [] [LPar, Nbr 5, RPar]) ==> Pair [] [5]
  --       applyARule (Pair [] [LPar, o, RPar]) ==> Pair [] [+]
applyARule window
  | [LPar, e, RPar] <- take 3 window
  , isOperand e                       = [e] ++ drop 3 window
-- <Your comment. What does this rule do?>
-- This rule deals with nagative numbers
-- Ex if passed window is [(,-,4,)] this means it will end into negative results.
-- This rule evaluate by converting [(,-,4,)]==> [(0-4)] making it proper expression.
applyARule window
  | [LPar, op@(Op opfn '-' _ _), e, RPar] <- take 4 window 
  , isOperand e                       = [LPar, Nbr 0, op, e, RPar] ++ drop 4 window
-- <Your comment. What does this rule do?>
-- This rule builds an expression out of current operator and its operands provided
  -- this operator has higher precedence than any operator on its left and right
  -- Ex-1: applyARule (Pair [] (tokenize "3*5+6")) ==> Pair [] [(,(3*5),+,6,)]
  --       Elmts 3*5 are converted into the expression (3*5)
  --       tokenize "3*5+6" ==> [(,3,*,5,+,6,)]
  --       leftOperator(elmt0) = '(', op = '*' & rightOperator(elmt3) = '+'
applyARule [elmt0, e1, op@(Op opfn c _ _), e2, elmt3]
    | isOperand e1 && isOperand e2
      && higherPrec elmt0 op elmt3 =
                       let newExpr = Expr e1 (Optr opfn c) e2
                       in  [elmt0, newExpr, elmt3]
-- <Your comment. Why is this defined?>
-- There is no change in the parse state if the above 3 rules do not apply.
  -- This rule will just let the current parse window shift in the current parse state
applyARule window = window


-- Checks if the operator op has higher precedence than the leftOp and 
-- rightOp.
higherPrec :: Elmt -> Elmt -> Elmt -> Bool  
higherPrec leftOp op rightOp =
  higherPrecThanLeft leftOp op && higherPrecThanRight op rightOp  


-- Checks if an operator op has higher precedence than the left Elmt.
-- Returns true if leftOp is "(" or if precedence of op greater than leftOp
-- and false in all other cases. 
higherPrecThanLeft :: Elmt -> Elmt -> Bool
higherPrecThanLeft LPar          _  = True
higherPrecThanLeft (Op _ _ p0 _) (Op _ _ p1 _) = p0 < p1
higherPrecThanLeft _             _             = False
  

-- Checks if an operator op has higher precedence than the right Elmt.
-- Returns true if rightOp is ")" or if precedence of op greater than rightOp
-- and false in all other cases.
higherPrecThanRight :: Elmt -> Elmt -> Bool
higherPrecThanRight _             RPar          = True
higherPrecThanRight (Op _ _ p1 _) (Op _ _ p2 _) = p1 >= p2
higherPrecThanRight _           _               = False


isOperand :: Elmt -> Bool
isOperand (Nbr _)      = True
isOperand (Expr _ _ _) = True
isOperand _            = False


-- Parses the expression one step at a time using the following steps:
-- applies any of the rules on the window and updates the window
-- depending on whether the updatedWindow and previous window were same or not,
-- slides the window and updates the newState
parseAStep :: ParseState parseState => parseState -> parseState
parseAStep pState = 
  let (left, window, right) = segment pState
      updatedWindow = applyARule window
      updatedState  = unSegment (left, updatedWindow, right)
      newState      = 
         -- If a rule applied, pull three Elmts back
         -- into the window from the left. Otherwise, 
         -- slide the window right by one element.
        if updatedWindow /= window
        then slideWindow (-3) updatedState 
        else slideWindow 1 updatedState 
  in perhapsTrace (" ns==> "  ++ showParseState newState) 
     newState 


-- Finds if xs is a sub-list of zs and if it is, replaces the 
-- matching sub-list of zs with ys.
-- Ex: replace [5,6] [1] [8,5,6,7] ==> [8,1,7]
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace xs ys zs 
  | xs == take (length xs) zs
               = ys ++ replace xs ys (drop (length xs) zs)
  | w:ws <- zs = w : replace xs ys ws
  | null zs    = []


-- While function that takes a initial state and executes the 
-- bodyFn as long as continueTest is true for the current state.
-- The body function takes a state and converts the current state to
-- a next state.
while :: state -> (state -> Bool) -> (state -> state) -> state
while state continueTest bodyFn = wh state
  -- This auxiliary function is not necessary. Defining it
  -- just avoids repeatedly passing  continueTest  and  bodyFn.
  where 
    wh st
      | continueTest st = wh (bodyFn st)
      | otherwise       = st 

-- ---------------------------------------------------------
--  Trace stuff
-- ---------------------------------------------------------

perhapsTrace :: String -> a ->  a
perhapsTrace str e = if   shouldTrace 
                     then trace str e
                     else e
  
  where shouldTrace = False