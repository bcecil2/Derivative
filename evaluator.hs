import Parser
import Tokenizer

                                                     
diff :: Ptree -> String
diff (SumNode op left right) =
         let lft = diff left
	     rgt = diff right
         in
	     case op of
	         Plus ->  lft ++ " + " ++ rgt
		 Minus -> lft ++ " - " ++ rgt

diff (PowNode Pow (VarNode str) (NumNode x)) = let newPow = show (x - 1.0) 
                                                   oldPow = show x
                                               in
                                                   oldPow ++ str ++ "^" ++ newPow 
diff (VarNode str) = "1" 

main = do print "Differentiating (x+x^2)^2" 
          print $ (diff . parse . tokenize) "(x+x^2)^2"
