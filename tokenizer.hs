import Data.Char
-- Primitive data types that will be used to build up expressions --
data Operator = Plus | Minus | Times | Div deriving(Show, Eq)
data Token = Tokop Operator | Tokid String | Toknum Int deriving(Show, Eq) 

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
-- Takes in a String and returns a list of tokens
-- TODO: generalize Toknum to be a float and write a
-- function similar to createId that constructs floats 
-- from a string representation
tokenize :: String -> [Token]
tokenize [] = []
tokenize (x:xs)
    | elem x "+-*/" = Tokop Plus : tokenize xs
    | isAlpha x = Tokid (fst res) : tokenize (snd res) 
    | isDigit x = Toknum (digitToInt x) : tokenize xs
    | isSpace x = tokenize xs
    | otherwise = error $  "Could not tokenize " ++ [x]
       where res =  createId(x:xs)

--test code w o r k s g r e a t
expr = "R2D2+C3PO" 


main = do
    print $ tokenize expr 
