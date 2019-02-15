import Data.List
import System.IO
import System.Environment
-- import qualified Data.Set as Set

-- custom data type of input format
data BKG = BKG 
	{
		nonTerminals ::String,
		terminals :: String,
		initialState :: String,
		rules :: [[[Char]]]
	} deriving (Show)
			   
example1 = BKG {nonTerminals = "S,A,B", terminals = "a,b,c,d", initialState = "S", rules = [["S","#"],["S","AB"],["A","aAb"],["A","ab"],["B","cBd"],["B","cd"]]}

step0 = show example1

getRules :: BKG -> [[[Char]]]
getRules (BKG _ _ _ b) = b
example1rules = (getRules example1)

getRule = head example1rules
getFirstElement = head getRule
getSecondElement = last getRule
