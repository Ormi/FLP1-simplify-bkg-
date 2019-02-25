-- SimplifyBKG Core Functions
-- FLP Functional Project
-- Michal Ormos (xormos00)
-- xormos00@stud.fit.vutbr.cz
-- February 2019


-- @TODO parse epsylon on rightSide side add rules add parseepsylon

module SimplifyBKGCore (getBKG, step1) where

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

-- isSameTerminal :: String -> String -> Bool
-- isSameTerminal [x] [y] = x == y 
-- isSameTerminal [x] _ = False
-- isSameTerminal _ [y] = False
-- isSameTerminal :: String -> String
-- isSameTerminal [x] = print x

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

-- doStep1 :: [String] -> [String]
-- doStep1 (nonTerminals:terminals:startState:rules) = [nonTerminals] ++ [terminals] ++ [startState] ++ step1rules (map (splitOn "->") (reverse rules))

-- -- "[[\"C\",\"c\"],[\"A\",\"AC\"],[\"B\",\"Bb\"],[\"B\",\"b\"],[\"S\",\"B\"],[\"S\",\"A\"]]"
-- step1rules :: [[String]] -> String
-- step1rules x = 
--     if (isTerminal(head x))
--         then 


-- step1rules :: [String] -> String
-- step1rules (x:xs) = (processRule x) + (step1rules xs) 

-- processRule :: String -> String
-- processRule x = (splitOn "->" x)

step1 :: String -> IO BKG
step1 content = do
    let lns = lines content
    let bkg = parseBKG lns
    let newbkg = alg1 bkg
    return newbkg

alg1 :: BKG -> BKG
alg1 grammar@BKG{nonTerminals = nonTerminals, 
                terminals = terminals,
                startState = startState,
                rules = rules } = BKG newNonterminals terminals startState newRules 
    where
        ntSet = getNtSet grammar []
        -- create new set of nonteminal and remove duplicate. 
        newNonterminals = nub $ ((ntSet) ++ (startState:[]))   
        -- creat new set of rules, if left side of rule is in Nt set and right side is combination of terminal and nonterminal symbols 
        newRules = [r | r <- rules, ((elem (leftSide r) ntSet) && (isTermNonterm ntSet terminals (rightSide r))) ]   

-- Iterates until the final set of Nt formed. Nt is set ..
getNtSet :: BKG -> [NonTerminal] -> [NonTerminal]
getNtSet g prevNt = if (length(prevNt) == length(newNt)) then prevNt else getNtSet g $ nub (prevNt ++ newNt) 
    where
        -- filter rules and create Ni set from left side of rules. 
        newNt = createNiSet g $ filterRules g prevNt

-- Create Ni set from left side of filter rules. 
createNiSet :: BKG -> [Rule] -> [NonTerminal]
createNiSet BKG{nonTerminals = nonTerminals, 
                terminals = terminals,
                startState = startState,
                rules = rules } vr = [nt | nt <- nonTerminals , any (\r -> nt == (leftSide r)) vr ] 

filterRules :: BKG -> [NonTerminal] -> [Rule]
filterRules BKG{nonTerminals = nonTerminals, 
                terminals = terminals,
                startState = startState,
                rules = rules } prevNt = [r | r <- rules, ((isTermNonterm  prevNt terminals (rightSide r)) ) ]

-- Checks characters from right side of the rule. 
isTermNonterm :: [NonTerminal] -> [Terminal] -> NonTerminal -> Bool
isTermNonterm prevNt terminals [] = True 
isTermNonterm prevNt terminals (n:ns) = if (elem (n:[]) terminals || (elem (n:[]) prevNt) || (n == '#')) then isTermNonterm prevNt terminals ns else False 






-- step1 :: String -> IO BKG
-- step1 content = do
--     putStr "====\n"
--     print content
--     putStr "===\n"    
--     bkg <- getBKG content
--     putStr "====\n"
--     print bkg
--     putStr "===\n"
--     putStr "Debug\n======\n"
--     putStr (showNonTerminals (nonTerminals bkg))
--     putStr "\n"
--     putStr (showTerminals (terminals bkg))
--     putStr "\n"    
--     putStr (startState bkg)
--     putStr "\n"  
--     -- let newRules = checkRules (rules bkg)  
--     -- let newBKG = BKG (nonTerminals bkg) (terminals bkg) (startState bkg) newRules

--     putStr (showRules (rules bkg))
--     -- showRulesPrime (rules bkg)
--     putStr "\n========\n\n"
--     -- let showNonTerminals (nonTerminals bkg) 
--     -- let newBKG = BKG (nonTerminals bkg) (terminals bkg) (startState bkg) (rules bkg) 
--     return bkg
--     -- bkg <- getBKG content
--     -- let test = getGroups (nonTerminals bkg) (rules bkg)
--     -- -- let newRules = getNewRules test (rules bkg)
--     -- let newBkg = BKG (nonTerminals bkg) (terminals bkg) (startState bkg) (rules bkg) --newRules
--     -- return newBkg

-- -- checkRules :: [Rule] -> [Rule]
-- -- checkRules 

-- showNonTerminals :: [NonTerminal] -> String
-- showNonTerminals x = show x

-- showTerminals :: [Terminal] -> String
-- showTerminals x = show x

-- showRules :: [Rule] -> String
-- showRules x = showMoreRules (head (reverse x)) -- ++ showMoreRules (tail x)
--     -- if showMoreRules (head x) then do
--     --             show "rovnaju sa"
--     --             showRules (tail x)
--     --           else
--     --             showRules (tail x)
--                 -- if (showMoreRules (head x))then
--                 --     Rule leftSide ([[x] | x <- rightSide])
--                 -- else
--                 --     error "[ERROR] Rules syntax, bad input"                

-- showMoreRules :: Rule -> String
-- showMoreRules x = (show ((leftSide x) ++ (showNonTerminals (rightSide x))))
-- showMoreRules x = (leftSide x) `elem` (rightSide x)
-- showMoreRules _ = False

-- ++ showNonTerminals(rightSide (head x)))

-- -- Gets context-free grammar without simple rules from input
-- getBKGWithoutSimpleRules :: String -> IO BKG
-- getBKGWithoutSimpleRules content = do
--     bkg <- getBKG content
--     let test = getGroups (nonTerminals bkg) (rules bkg)
--     let newRules = getNewRules test (rules bkg)
--     let newBkg = BKG (nonTerminals bkg) (terminals bkg) (start bkg) newRules
--     return newBkg

-- -- Gets list of nonTerminals generated by specified nonTerminals
-- getGroups :: [Nonterminal] -> [Rule] -> [(Nonterminal, [Nonterminal])]
-- getGroups [] _ = []
-- getGroups (x:xs) rules = [(x, [x] ++ (generate ([x]) rules))] ++ getGroups xs rules
--     where
--         generate :: [Nonterminal] -> [Rule] -> [Nonterminal]
--         generate [] _ = []
--         generate (x:xs) rules = (generatedNonterminals x) ++ (generate (generatedNonterminals x) rules)
--             where
--                 generatedNonterminals :: Nonterminal -> [Nonterminal]
--                 generatedNonterminals x = [head (right y) | y <- (getSimpleRules (getNonterminalRules x rules))]

-- -- Gets new rules for context-free grammar without simple rules
-- getNewRules :: [(Nonterminal, [Nonterminal])] -> [Rule] -> [Rule]
-- getNewRules [] rules = []
-- getNewRules (x:xs) rules = generate (snd x) (fst x) rules ++ getNewRules xs rules
--     where
--         generate :: [Nonterminal] -> Nonterminal -> [Rule] -> [Rule]
--         generate [] _ _ = []
--         generate (y:ys) nonterminal rules = [Rule nonterminal (right z) | z <- (getNotSimpleRules (getNonterminalRules y rules))] ++ generate ys nonterminal rules

-- -- Gets rules generated by specified nonterminal
-- getNonterminalRules :: Nonterminal -> [Rule] -> [Rule]
-- getNonterminalRules nonterminal rules = [x | x <- rules, (left x) == nonterminal]    