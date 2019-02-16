-- simplify-bkg Data module
-- FLP Functional Project
-- Michal Ormos (xormos00)
-- xormos00@stud.fit.vutbr.cz
-- February 2019

module SimplifyBKGData where

import Data.List

type NonTerminal = String
type Terminal = String

data Rule = Rule
{ 
    leftSide :: NonTerminal, 
    rightSide :: [String]
} deriving (Eq)

data BKG = BKG
{ 
    nonTerminals :: [NonTerminal],
    terminals :: [Terminal],
    startState :: NonTerminal,
    rules :: [Rule]
} deriving (Eq)    

-- intersperse :: Char -> Text -> Text
instance Show Rule where
    show (Rule left right) = left ++ "->" ++ concat (intersperse "" right)

-- intersperse :: Char -> Text -> Text
instance Show BKG where
    show (BKG nonTerminals terminals startState rules) = concat (intersperse "," nonTerminals) ++ "\n" ++ concat (intersperse "," terminals) ++ "\n" ++ startState ++ "\n" ++ concat (intersperse "\n" (map show rules))