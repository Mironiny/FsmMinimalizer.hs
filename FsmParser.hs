-- Project: dka2mka
-- Autor: Bc. Miroslav xnovak1k
-- Project description: Application which convert deterministic finite state machine to
--                      minimal finita state machine.
-- Module description: This module handles all operation with Fsm - parsing, validation and printing

module FsmParser where

import Data.List
import qualified Data.Set as Set
import Data.Char (isDigit)
import Data.List.Split
    
data Fsm = Fsm { states ::  Set.Set String
               , initialState :: Set.Set String
               , finiteState ::  Set.Set String
               , rules :: Set.Set [String]
               , alphabet :: Set.Set String
               } deriving (Show)


parse:: String -> Fsm
parse fsm = Fsm { states = Set.fromList (parsedFsm !! 0)
                , initialState = Set.fromList (parsedFsm !! 1)
                , finiteState = Set.fromList (parsedFsm !! 2)
                , rules = Set.fromList (drop 3 parsedFsm)
                , alphabet = Set.fromList $ map (!! 1) (drop 3 parsedFsm)
                }
                where fsmByLines = lines fsm
                      parsedFsm = map (splitOn ",") fsmByLines

isFsmValid:: Fsm -> (Bool, String)
isFsmValid fsm
            | not $ all isStringDigital (Set.toList (states fsm)) = (False, "States must be digital")
            | not $ all isStringDigital (Set.toList (initialState fsm)) = (False, "Initial states must be digital")
            | not $ all isStringDigital (Set.toList (finiteState fsm)) = (False, "Finite states must be digital")
            | otherwise = (True, "Ok")

printFsm:: Fsm -> IO ()
printFsm x = do
    putStrLn $ getString $ Set.toList (states x)
    putStrLn $ getString $ Set.toList (initialState x)
    putStrLn $ getString $ Set.toList (finiteState x)
    mapM_ putStrLn (map getString $ Set.toList (rules x))

getString:: [String] -> String
getString = id $ intercalate ","

readStdin:: IO String
readStdin = do
    contents <- getContents
    return contents

readFromFile :: String -> IO String
readFromFile filename = do
    contents <- readFile filename
    return contents

isStringDigital :: String -> Bool
isStringDigital xs = ((length (filter isDigit xs )) == length (xs))