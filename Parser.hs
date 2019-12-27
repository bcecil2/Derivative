module Parser where

import Tokenizer

data Ptree = SumNode Operator Ptree Ptree
           | ProdNode Operator Ptree Ptree
           | DivNode Operator Ptree Ptree
           | PowNode Operator Ptree Ptree
           | UnaryNode Operator Ptree
           | NumNode Float
           | VarNode String
           deriving (Show)

lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (x:xs) = x

acceptTok :: [Token] -> [Token]
acceptTok [] = error $ "Error on Accept: No Tokens Found"
acceptTok (x:xs) = xs

expression :: [Token] -> (Ptree, [Token])
expression tokens =
      let (tree, toks) = term tokens
      in
         case lookAhead toks of
             (Tokop Plus) -> let (finalTree, toks') = expression (acceptTok toks) in (SumNode Plus tree finalTree, toks') 
             (Tokop Minus) -> let (finalTree, toks') = expression (acceptTok toks) in (SumNode Minus tree finalTree, toks')
             _ -> (tree, toks)

term :: [Token] -> (Ptree, [Token])
term tokens = 
      let (factorTree, toks) = factor tokens
      in
        case lookAhead toks of
             (Tokop Div) -> let (termTree, toks') = term (acceptTok toks) in (DivNode Div factorTree termTree, toks') 
             (Tokop Times) -> let (termTree, toks') = term (acceptTok toks) in (ProdNode Times factorTree termTree, toks') 
             (Tokop Pow) -> let (termTree, toks') = term (acceptTok toks) in (PowNode Pow factorTree termTree, toks') 
             _ -> (factorTree, toks)

factor :: [Token] -> (Ptree, [Token])
factor tokens = case lookAhead tokens of
                     (Toknum x) -> (NumNode x, acceptTok tokens)
                     (Tokid var) -> (VarNode var, acceptTok tokens)
                     (Tokop Plus) -> let (nTree, nTokens) = factor (acceptTok tokens) in (UnaryNode Plus nTree, nTokens)
                     (Tokop Minus) -> let (nTree, nTokens) = factor (acceptTok tokens) in (UnaryNode Minus nTree, nTokens)
                     (TokLParen) -> let (nTree, nTokens) = expression (acceptTok tokens)
                                                           in if lookAhead nTokens /= TokRParen then error $ "Parentheses mismatch"
                                                              else (nTree, acceptTok nTokens)
                     _ -> error $ "Parse error on token " ++ show tokens

parse :: [Token] -> Ptree
parse tokens = let (expr, toks) = expression tokens
               in if lookAhead toks /= TokEnd then error $ "Error parsing expression " ++ show toks
                  else expr

