import Data.Char
import Data.List

-- Primitive data types that will be used to build up expressions --
data Operator = Plus | Minus | Times | Div deriving(Show, Eq)
data Token = Tokop Operator | Tokid String | Toknum Float deriving(Show, Eq) 

-- Helper function to print operators --
showOp :: Operator -> String
showOp Plus = "+"
showOp Minus = "-"
showOp Times = "*"
showOp Div = "/"

-- Helper function to print tokens --
showToken :: Token -> String
showToken (Tokop op) = showOp op 
showToken (Tokid str) = str
showToken (Toknum num) = show num

-- Handles Identifiers that are multiple characters
-- returns a tuple containg with the first element
-- being the identifier and the second element containg
-- the rest of the input expression (can be empty)
createId :: String -> (String, String)
createId expr = (identifier, rest)
		where identifier = takeWhile (isAlpha) expr
		      rest = drop (length identifier) expr

-- Handles numbers that are multiple digits/decimals
-- returns a tuple with the first element being the 
-- number and the second element containing the rest
-- of the input expression (can be empty)
createNum :: String -> (String, String)
createNum expr
             | elem (length $ findIndices (=='.') expr) [0,1] = (num, rest)
             | otherwise = error $ "could not tokenize numeric constant too many decimals " ++ num
         where num  = takeWhile (\ x -> isDigit x || x == '.') expr
               rest = drop (length num) expr

-- Takes in a String and returns a list of tokens
tokenize :: String -> [Token]
tokenize [] = []
tokenize (x:xs)
    | elem x "+-*/" = Tokop Plus : tokenize xs
    | isAlpha x = Tokid (fst identifier) : tokenize (snd identifier) 
    | isDigit x = Toknum (read $ fst number :: Float) : tokenize (snd number)
    | isSpace x = tokenize xs
    | otherwise = error $  "Could not tokenize " ++ [x]
       where identifier =  createId(x:xs)
             number = createNum(x:xs)

--test code w o r k s g r e a t
expr = "R22.323D420+C3PO" 


main = do
    print $ tokenize expr 
