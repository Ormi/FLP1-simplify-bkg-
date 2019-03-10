-- simplify-bkg Data module
-- FLP Functional Project
-- Michal Ormos (xormos00)
-- xormos00@stud.fit.vutbr.cz
-- February 2019

module SimplifyBKGData where

import Data.List

-- rename of existring data tyoes
-- type xxx = [Int]
type NonTerminal = String -- <=> [Char]
type Terminal = String -- <=> [Char]
type NonTermTerminal = String

data Rule = Rule { 
    leftSide :: NonTerminal,  -- [Char]
    rightSide :: NonTermTerminal -- x -- [[Char]]
	} deriving (Eq)

data BKG = BKG { 
    nonTerminals :: [NonTerminal], -- [[Char]]
    terminals :: [Terminal], -- [[Char]]
    startState :: NonTerminal, -- [Char]
    rules :: [Rule]
	} deriving (Eq)    

-- intersperse :: Char -> Text -> Text
instance Show Rule where
    show (Rule left right) = left ++ "->" ++ right --concat (intersperse "" right)

-- intersperse :: Char -> Text -> Text
instance Show BKG where
    show (BKG nonTerminals terminals startState rules) = concat (intersperse "," nonTerminals) ++ "\n" ++ concat (intersperse "," terminals) ++ "\n" ++ startState ++ "\n" ++ concat (intersperse "\n" (map show rules))