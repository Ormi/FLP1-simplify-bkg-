-- SimplifyBKG
-- FLP Functional Project
-- Michal Ormos (xormos00)
-- xormos00@stud.fit.vutbr.cz
-- February 2019

-- @TODO osetri stdin ale aj std file

module Main where

import System.Environment
import System.IO
import System.Exit

import SimplifyBKGCore
import SimplifyBKGData

main :: IO ()
main = do
    args <- getArgs
    let (option, input) = processOptions args
    content <- parseInput input
    bkg <- eliminateRulesBKG option content
    putStrLn $ show bkg
    return ()

-- Gets context-free grammar from input in the form specified by param
eliminateRulesBKG :: Int -> String -> IO BKG
eliminateRulesBKG param content
    | param==0 = getBKG content
    | param==1 = step1 content
    | param==2 = step2 content

-- Reads input into a string
parseInput :: [Char] -> IO String
parseInput [] = getContents
parseInput x = readFile x

-- Parse list of arguments into couple
processOptions :: [String] -> (Int,String)
processOptions [] = error "[Error] Input expect one or two arguments -i with combination of -1/-2 see README for more information"
processOptions [x] = processOptions [x, ""]
processOptions [x,y]
    | x=="-i" = (0, y)
    | x=="-1" = (1, y)
    | x=="-2" = (2, y)
    | otherwise = error "[Error] Unknown argument"
processOptions _ = error "[Error] Expects 2 arguments"