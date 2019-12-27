module PtreeTraversal where

import Tokenizer
import Parser 	 

-- Checks if the Node has a left child
hasLeft :: Ptree -> Bool
hasLeft (SumNode _ _ _) = True
hasLeft (ProdNode _ _ _) = True
hasLeft (DivNode _ _ _) = True
hasLeft (PowNode _ _ _) = True
hasLeft (UnaryNode _ _) = True
hasLeft _ = False

-- Checks if the Node has a right child
hasRight :: Ptree -> Bool
hasRight (SumNode _ _ _) = True
hasRight (ProdNode _ _ _) = True
hasRight (DivNode _ _ _) =  True
hasRight (PowNode _ _ _) = True
hasRight _ = False

-- Returns the left child Node
getLeft :: Ptree -> Ptree
getLeft (SumNode _ l _) = l
getLeft (ProdNode _ l _) = l
getLeft (DivNode _ l _) = l
getLeft (PowNode _ l _) = l
getLeft (UnaryNode _ l) = l
getLeft _ = error "Does not contain a left child"

-- Returns the right child Node
getRight :: Ptree -> Ptree
getRight (SumNode _ _ r) = r
getRight (ProdNode _ _ r) = r
getRight (DivNode _ _ r) = r
getRight (PowNode _ _ r) = r
getRight _ = error "Does not contain a right child"

-- Returns the Operator being stored in the Node
getOperator :: Ptree -> Operator
getOperator (SumNode c _ _) = c
getOperator (ProdNode c _ _) = c
getOperator (DivNode c _ _) = c
getOperator (PowNode c _ _) = c
getOperator (UnaryNode c _) = c
getOperator _ = error "Does not contain an operator"

-- Returns the Float being stored in the Node
getNum :: Ptree -> Float
getNum (NumNode c) = c
getNum _ = error "Does not contain a float"

-- Returns the Variable being stored in the Node
getVar :: Ptree -> String
getVar (VarNode c) = c
getVar _ = error "Does not contain a variable"

-- Returns the type of Node
getNode :: Ptree -> String
getNode (SumNode _ _ _) = "SumNode"
getNode (ProdNode _ _ _) = "ProdNode"
getNode (DivNode _ _ _) = "DivNode"
getNode (PowNode _ _ _) = "PowNode"
getNode (UnaryNode _ _) = "UnaryNode"
getNode (NumNode _) = "NumNode"
getNode (VarNode _) = "VarNode"

-- Checks if the Node is an Operator-related Node
isOper :: Ptree -> Bool
isOper (SumNode _ _ _) = True
isOper (ProdNode _ _ _) = True
isOper (DivNode _ _ _) = True
isOper (PowNode _ _ _) = True
isOper (UnaryNode _ _) = True
isOper _ = False
