module PowerRule where

import Parser
import Tokenizer

assembleTerms :: Ptree -> [(Float, String, Float)]
assembleTerms (VarNode v) = [(1.0, v, 1.0)]
assembleTerms (PowNode _ (VarNode v) (NumNode p)) = [(1.0, v, p)]
assembleTerms (ProdNode _ (NumNode c) (VarNode v)) = [(c, v, 1.0)]
assembleTerms (ProdNode _ (NumNode c) (PowNode _ (VarNode v) (NumNode p))) = [(c, v, p)]
assembleTerms (ProdNode _ l r) = (assembleTerms l) ++ (assembleTerms r)
assembleTerms (SumNode _ l r) = (assembleTerms l) ++ (assembleTerms r)
assembleTerms (DivNode _ l r) = (assembleTerms l) ++ (assembleTerms r)
assembleTerms (UnaryNode _ l) = assembleTerms l
assembleTerms _ = []

assembleOperators :: Ptree -> [String]
assembleOperators (SumNode x l r) = [(showOp x)] ++ (assembleOperators l) ++ (assembleOperators r)
assembleOperators (DivNode x l r) = [(showOp x)] ++ (assembleOperators l) ++ (assembleOperators r)
assembleOperators (UnaryNode x l) = [(showOp x)] ++ (assembleOperators l)
assembleOperators (PowNode _ (VarNode _) (NumNode _)) = []
assembleOperators (PowNode x l r) = [(showOp x)] ++ (assembleOperators l) ++ (assembleOperators r)
assembleOperators (ProdNode _ (NumNode _) (VarNode _)) = []
assembleOperators (ProdNode _ (NumNode _) (PowNode _ (VarNode _) (NumNode _))) = []
assembleOperators (ProdNode x l r) = [(showOp x)] ++ (assembleOperators l) ++ (assembleOperators r)
assembleOperators _ = []

powerRule :: [(Float, String, Float)] -> [(Float, String, Float)]
powerRule [] = []
powerRule ((c, v, p):xs) = (c*p, v, p-1):(powerRule xs)

translateEq :: [(Float, String, Float)] -> [String] -> String
translateEq [(c, v, p)] [] = (show c) ++ "*" ++ v ++ "^" ++ (show (round $ p)) 
translateEq ((c, v, p):xs) (y:ys) = ((show c) ++ "*" ++ v ++ "^" ++ (show (round $ p)) ++ y) ++ (translateEq xs ys)

derivative :: String -> String
derivative x = translateEq (powerRule(assembleTerms t)) (assembleOperators t)
             where t = fst (expression (tokenize x))
