module PowerRule where

import Parser
import Tokenizer
import PtreeTraversal

-- Function that determines if a Ptree branch is a term
isTerm :: Ptree -> Bool
isTerm (ProdNode _ l r)
    | leftCont == "VarNode" = True
    | rightCont == "VarNode" = True
    | rightCont == "PowNode" = isTerm r 
    | otherwise = False
    where leftCont = getNode l
          rightCont = getNode r
isTerm (PowNode _ l r)
    | leftCont == "VarNode" = True
    | otherwise = False
    where leftCont = getNode l
          rightCont = getNode r
isTerm (VarNode _) = True
isTerm _ = False

-- Function that gets the coefficient of a term
getCoeff :: Ptree -> Float
getCoeff (ProdNode _ l r)
    | (getNode l) == "NumNode" = getNum l
    | (getNode r) == "NumNode" = getNum r
    | otherwise = error "Not a term"
getCoeff (PowNode _ _ _) = 1.0
getCoeff (VarNode _) = 1.0

-- Function that gets the power of a term
getPow :: Ptree -> Float
getPow (ProdNode _ l r)
    | (getNode r) == "PowNode" = getPow r
    | otherwise = 1.0
getPow (PowNode _ _ r)
    | (getNode r) == "NumNode" = getNum r
    | otherwise = error "Not a term"
getPow (VarNode _) = 1.0

-- Function that returns the term
getTerm :: Ptree -> String
getTerm (ProdNode _ l r)
    | (getNode l) == "VarNode" = getVar l
    | (getNode r) == "VarNode" = getVar r
    | (getNode r) == "PowNode" = getTerm r
    | otherwise = error "Not a term"
getTerm (PowNode _ l _)
    | (getNode l) == "VarNode" = getVar l
    | otherwise = error "Not a term"
getTerm (VarNode c) = c

-- Function that takes a tree and returns all the terms in the form of (Coefficient, Variable, Power)
assembleTerms :: Ptree -> [(Float, String, Float)]
assembleTerms x
    | (isTerm x) && (hasLeft x) && (hasRight x) = [((getCoeff x), (getTerm x), (getPow x))]
    | (isTerm x) && (hasLeft x) && not (hasRight x) = [((getCoeff x), (getTerm x), (getPow x))]
    | (isTerm x) && not (hasLeft x) && (hasRight x) = [((getCoeff x), (getTerm x), (getPow x))]
    | (isTerm x) && not (hasLeft x) && not (hasRight x) = [((getCoeff x), (getTerm x), (getPow x))]
    | not (isTerm x) && (hasLeft x) && (hasRight x) = (assembleTerms (getLeft x)) ++ (assembleTerms (getRight x))
    | not (isTerm x) && (hasLeft x) && not (hasRight x) = (assembleTerms (getLeft x))
    | not (isTerm x) && not (hasLeft x) && (hasRight x) = (assembleTerms (getRight x))
    | otherwise = error "No Terms Present"

-- Performs a power rule on a given term
powRule :: (Float, String, Float) -> (Float, String, Float)
powRule (x, y, z) = (x*z, y, z-1)

-- Function that takes a tree and returns all operators among the terms
getOperations :: Ptree -> [Operator]
getOperations x
    | not (isTerm x) && (hasLeft x) && (hasRight x) && (isOper x) = [getOperator x] ++ (getOperations (getLeft x)) ++ (getOperations (getRight x))
    | not (isTerm x) && (hasLeft x) && not (hasRight x) && (isOper x) = [getOperator x] ++ (getOperations (getLeft x))
    | not (isTerm x) && not (hasLeft x) && not (hasRight x) && (isOper x) = [getOperator x]
    | otherwise = []

-- Converts a tuple of (Coefficient, Variable, Power) into a string
translateTerm :: (Float, String, Float) -> String
translateTerm (x, y, z) = (show x) ++ "*" ++ y ++ "^" ++ (take 1 (show z))

-- Puts together lists of terms and operators between terms together
putTogether :: String -> Operator -> String
putTogether x y = x ++ (showOp y)

-- Takes a list of terms and returns the derivative of it
derivation :: [(Float, String, Float)] -> [(Float, String, Float)]
derivation [] = error "Empty List"
derivation [x] = [(powRule x)]
derivation (x:xs) = [(powRule x)] ++ (derivation xs)

-- Function that takes a list of terms and operators between terms and returns the equation
translateEq :: [(Float, String, Float)] -> [Operator] -> String
translateEq [] _ = error "More Operators than Terms"
translateEq [x] [] = translateTerm x
translateEq (x:xs) (y:ys) = (putTogether (translateTerm x) y) ++ (translateEq xs ys) 

-- Takes an expression string and returns its derivative
getDerivative :: String -> String
getDerivative x = translateEq (derivation (assembleTerms y)) (getOperations y)
                where y = fst (expression (tokenize x))
