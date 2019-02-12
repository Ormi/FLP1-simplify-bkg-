-- FLP Functional Project
-- Michal Ormos (xormos00)
-- xormos00@stud.fit.vutbr.cz
-- February 2019

-- @TODO every function has to have a datatypes

-- imports
import Data.List
import System.IO
import System.Environment
import qualified Data.Set as Set

-- custom data type of input format
data BKG = BKG 
	{
		nonTerminals :: Set.Set[Char],
		terminals :: Set.Set[Char],
		initialState :: [Char]
		-- rules :: [Char]
	} deriving (Show)

-- get input depend of stdin or file
getInput input = do
	if (length input) == 1
		then return stdin
		else openFile (head (tail input)) ReadMode

-- split the whole line which is separated by comma
commaSplit string = split string []
	where
		split [] last = [last]
		split (x:xs) last = if x == ','
			then [last] ++ (split xs [])
			else split xs (last ++ [x])

parseNonTerminals lines = Set.fromList (commaSplit (lines !! 0))
parseTerminals lines = Set.fromList (commaSplit (lines !! 1))
parseInitialState lines = lines !! 2
-- parseRules = "a"

-- parse input format
parseInput lines = BKG
	{
		nonTerminals = (parseNonTerminals lines),
		terminals = (parseTerminals lines),
		initialState = (parseInitialState lines)
		-- rules = (parseRules lines)
	}
	
printBkg
	show BKG
-- printStepOne bkg = bkg
-- printStepTwo bkg = bkg
	
-- main
main = do 
	input <- getArgs
	if (length input) > 2 || (length input < 1)
		then error "[Error] Invalid number of arguments"
		else do
			handle <- getInput input
			contents <- hGetContents handle
			let bkg = parseInput (lines contents)
			case (head input) of
				"-i" -> printBkg
				-- "-1" -> printStepOne bkg
				-- "-2" -> printStepTwo bkg			
				otherwise -> error "Invalid argument"

			hClose handle
	-- putStrLn input

-- reading from file example
-- readFromFile = do
-- 	theFile <- openFile "input.txt" ReadMode
-- 	contents <- hGetContents theFile
-- 	putStr contents
-- 	hClose theFile