-- SimplifyBKG Core Functions
-- FLP Functional Project
-- Michal Ormos (xormos00)
-- xormos00@stud.fit.vutbr.cz
-- February 2019

module SimplifyBKGCore (getBKG) where

import System.IO
import Data.List
import Data.List.Split
import Data.String

import SimplifyBKGData

-- PARAMETER "-i" - LOAD INTO THE INTERNAL STRUCTURE
-- Gets context-free grammar from input
getBKG :: String -> IO BKG
getBKG content = do
    let lns = lines content
    let bkg = parseBKG lns
    return bkg

-- Parse context-free grammar from input
parseBKG :: [String] -> BKG
parseBKG (nonTerminals:terminals:startState:rules) =
    if null rules then
        error "[ERROR] BKG do not contain rules"
    else
        BKG parseNonTerminals parseTerminals (processNonTerminal startState) parseRules
    where
        parseNonTerminals = map processNonTerminal (splitOn "," nonTerminals)
        processNonTerminal :: String -> NonTerminal
        processNonTerminal nonterminal =
            if (length nonterminal == 1) && ((head nonterminal) `elem` ['A'..'Z']) then
                nonterminal
            else 
                error "[ERROR] nonTerminal syntax, bad input"

        parseTerminals = map processTerminal (splitOn "," terminals)
        processTerminal :: String -> Terminal
        processTerminal terminal =
            if (length terminal == 1) && ((head terminal) `elem` ['a'..'z']) then
                terminal
            else
                error "[ERROR] Terminal syntax, bad input"

        parseRules = map processRule rules
        processRule :: String -> Rule
        processRule rule = generateRule (splitOn "->" rule)
            where
                generateRule [left,right] =
                    if (leftCheck left) && (rightCheck right) then
                        Rule left ([[x] | x <- right])
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
                generateRule _ = error "[ERROR] Rule syntax, bad iput"
parseBKG _ = error "[ERROR] Grammar syntax, bad input"