-- SimplifyBKG Core Functions
-- FLP Functional Project
-- Michal Ormos (xormos00)
-- xormos00@stud.fit.vutbr.cz
-- February 2019

module SimplifyBKGCore (getBKG, step1, step2) where

import System.IO
import Data.List
import Data.List.Split
import Data.String

import SimplifyBKGData

--------------------------------------------------------------------------------
--------------------------- Helper functions -----------------------------------
--------------------------------------------------------------------------------
isTerminal :: String -> Bool
isTerminal [x] = x `elem` ['a'..'z']
isTerminal _ = False

isNonTerminal :: String -> Bool
isNonTerminal [x] = x `elem` ['A'..'Z']
isNonTerminal _ = False

removeDuplicates :: [NonTerminal] -> [NonTerminal]
removeDuplicates = foldl (\seen x -> if x `elem` seen
                                      then seen
                                      else seen ++ [x]) []

intersect' :: [NonTerminal] -> [NonTermTerminal] -> [NonTerminal]
intersect' [] _ = []
intersect' (x:xs) l | elem x l = x : intersect xs l
                   | otherwise = intersect xs l

orderBKG :: BKG -> BKG
orderBKG grammar@BKG{
                    nonTerminals = nonTerminals, 
                    terminals = terminals,
                    startState = startState,
                    rules = rules } = BKG newNonterminals newTerminals startState rules
    where
        newNonterminals = checkOrder (sort nonTerminals)
        newTerminals = sort terminals

checkOrder :: [NonTerminal] -> [NonTerminal]
checkOrder nonTerminals = if (last (nonTerminals) == "S")
                            then (last (nonTerminals) : init (nonTerminals))
                            else nonTerminals

allDifferent :: (Ord a) => [a] -> Bool
allDifferent xs = length (nub xs) == length xs
--------------------------------------------------------------------------------
--------------- Input parsing and grammar validity checks ----------------------
--------------------------------------------------------------------------------
getBKG :: String -> IO BKG
getBKG content = do
    let lns = lines content
    let bkg = parseBKG lns
    return bkg

parseBKG :: [String] -> BKG
parseBKG (nonTerminals:terminals:startState:rules) =
    if null rules then
        error "[ERROR] BKG do not contain rules"
    else
        BKG parseNonTerminals parseTerminals parseStartState parseRules
    where
        -- Non Terminals syntax check
        parseNonTerminals = map processNonTerminal (splitOn "," nonTerminals)
        processNonTerminal :: String -> NonTerminal
        processNonTerminal nonterminal =
            if (length nonterminal == 1) && ((head nonterminal) `elem` ['A'..'Z']) then
                nonterminal
            else 
                error "[ERROR] nonTerminal syntax, bad input"

        -- Terminals syntax check
        parseTerminals = map processTerminal (splitOn "," terminals)
        processTerminal :: String -> Terminal
        processTerminal terminal =
            if (length terminal == 1) && ((head terminal) `elem` ['a'..'z']) then
                terminal
            else
                error "[ERROR] Terminal syntax, bad input"

        -- Start state syntax check
        parseStartState = processStartState startState
        processStartState :: String -> NonTerminal
        processStartState startState =
            if (length startState == 1) && ((head startState) `elem` ['A'..'Z']) then
                startState
            else
                error "[ERROR] Start state syntax, bad input"   
        
        -- Rules syntax check
        parseRules = map processRule rules
        processRule :: String -> Rule
        processRule rule = generateRule (splitOn "->" rule)
            where
                generateRule [leftSide,rightSide] =
                    if (leftCheck leftSide) && (rightCheck rightSide) then
                        Rule leftSide ([x | x <- rightSide])
                    else
                        error "[ERROR] Rules syntax, bad input"
                    where
                        leftCheck :: String -> Bool
                        leftCheck [x] = x `elem` [head y | y <- parseNonTerminals]
                        leftCheck _ = False
                        rightCheck :: String -> Bool
                        rightCheck [] = False
                        rightCheck [x] = x `elem` ([head x | x <- parseTerminals] ++ [head y | y <- parseNonTerminals])
                        rightCheck (x:xs) = x `elem` ([head x | x <- parseTerminals] ++ [head y | y <- parseNonTerminals]) && rightCheck xs
                generateRule _ = error "[ERROR] Rule syntax, rules is empty, bad iput"
parseBKG _ = error "[ERROR] Grammar syntax, bad input"

--------------------------------------------------------------------------------
------------------------------ Core Logic --------------------------------------
--------------------------------------------------------------------------------

step1 :: String -> IO BKG
step1 content = do
    let lns = lines content
    let bkgParsed = parseBKG lns
    let bkgStep1 = applyFirstAlgorithm bkgParsed
    let orderedBKG = orderBKG bkgStep1
    return orderedBKG

applyFirstAlgorithm :: BKG -> BKG
applyFirstAlgorithm grammar@BKG{
                nonTerminals = nonTerminals, 
                terminals = terminals,
                startState = startState,
                rules = rules } = BKG newNonterminals terminals startState newRules 
    where
        nonTerminalSet = getNewNonTerminalSet grammar []
        newNonterminals = removeDuplicates ((nonTerminalSet) ++ (startState:[]))   
        newRules = [r | r <- rules, (((leftSide r) `elem` nonTerminalSet) && (isTermNonterm nonTerminalSet terminals (rightSide r))) ]   

getNewNonTerminalSet :: BKG -> [NonTerminal] -> [NonTerminal]
getNewNonTerminalSet grammar previousNonTerminal =
    if (length(previousNonTerminal) == length(newNonTerminal)) 
        then previousNonTerminal 
        else getNewNonTerminalSet grammar (removeDuplicates (previousNonTerminal ++ newNonTerminal))
        where
            newNonTerminal = createNonTerminalSet grammar (filterRules grammar previousNonTerminal)

createNonTerminalSet :: BKG -> [Rule] -> [NonTerminal]
createNonTerminalSet BKG{nonTerminals = nonTerminals, 
                terminals = terminals,
                startState = startState,
                rules = rules } 
                vr = [nt | nt <- nonTerminals , any (\r -> nt == (leftSide r)) vr ] 

filterRules :: BKG -> [NonTerminal] -> [Rule]
filterRules BKG{nonTerminals = nonTerminals, 
                terminals = terminals,
                startState = startState,
                rules = rules } 
                previousNonTerminal = [r | r <- rules, ((isTermNonterm  previousNonTerminal terminals (rightSide r)) ) ]

isTermNonterm :: [NonTerminal] -> [Terminal] -> NonTerminal -> Bool
isTermNonterm rightSideTerms terminals [] = True 
isTermNonterm rightSideTerms terminals (x:xs) = 
    if ((x:[]) `elem` terminals || ((x:[]) `elem` rightSideTerms) || (x == '#')) 
        then isTermNonterm rightSideTerms terminals xs 
        else False 

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

step2 :: String -> IO BKG
step2 content = do
    let lns = lines content
    let bkgParsed = parseBKG lns
    let bkgStep1 = applyFirstAlgorithm bkgParsed
    let bkgStep2 = applySecondAlgorithm bkgStep1
    let orderedBKG = orderBKG bkgStep2
    return orderedBKG

applySecondAlgorithm :: BKG -> BKG
applySecondAlgorithm grammar@BKG{
                    nonTerminals = nonTerminals, 
                    terminals = terminals,
                    startState = startState,
                    rules = rules } = BKG newNonterminals newTerminals startState newRules
    where
        newRulesSet = getNewRules grammar (startState:[])
        newNonterminals = intersect' newRulesSet nonTerminals
        newTerminals = intersect' newRulesSet terminals
        newRules = filterRulesSet rules newRulesSet

getNewRules :: BKG -> [NonTerminal] -> [NonTerminal]  
getNewRules grammar@BKG{nonTerminals = nonTerminals, 
                terminals = terminals,
                startState = startState,
                rules = rules } 
                previousRules = 
    if (length(previousRules) == length(newRulesSet)) 
        then previousRules 
        else (getNewRules grammar newRulesSet) 
        where
            newRulesSet = removeDuplicates (previousRules ++ (createNewRulesSet grammar previousRules) )

createNewRulesSet :: BKG -> [NonTerminal] -> [NonTerminal]
createNewRulesSet BKG{nonTerminals = nonTerminals, 
                terminals = terminals,
                startState = startState,
                rules = rules } 
                previousRules = [vi | vi <- termNontermSet, any (\r -> (not (null (intersect vi (rightSide r))))) filterRules] 
    where
        filterRules = filterRulesSet rules previousRules
        termNontermSet = nonTerminals ++ terminals
    
filterRulesSet :: [Rule] -> [NonTerminal] -> [Rule]
filterRulesSet rules previousRules = [r | r <- rules, ((leftSide r) `elem` previousRules) ]