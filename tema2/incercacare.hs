import Data.List
type Position = (Int, Int)

addLevel :: [(Char, Integer)] -> (Char, Integer)
addLevel l = l ^? element 1