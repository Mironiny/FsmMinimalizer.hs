import System.IO
import System.Environment
import Data.List
import Control.Monad
import Data.List.Split
import Data.Char (isDigit)
import qualified Data.Set as Set

import ArgumentParser

data Fsm = Fsm { states ::  [String]
               , initialState :: [String]
               , finiteState ::  [String]
               , rules :: [[String]]
               } deriving (Show)

main :: IO ()
main = do
    args <- getArgs
    let parsedArgs = parseArgs args
    when (not $ isValid parsedArgs) (error "Error in input parameters")

    fsm <- if null (fileName parsedArgs)
                then
                    readStdin
                else
                    readFromFile (fileName parsedArgs)

    let parsedFsm = parse fsm

    when (not $ isFsmValid parsedFsm) (error "Fsm is not valid")

    when (isI parsedArgs) (printFsm parsedFsm)

    when (isT parsedArgs) (print $ Set.toList $ minimalizeFsm parsedFsm)

printFsm:: Fsm -> IO ()
printFsm x = do
    putStrLn $ getString (states x)
    putStrLn $ getString (initialState x)
    putStrLn $ getString (finiteState x)
    mapM_ putStrLn (map getString (rules x))

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

parse:: String -> Fsm
parse fsm = Fsm { states = (parsedFsm !! 0)
                , initialState = (parsedFsm !! 1)
                , finiteState = (parsedFsm !! 2)
                , rules = (drop 3 parsedFsm)
                }
    where fsmByLines = lines fsm
          parsedFsm = map (splitOn ",") fsmByLines

isFsmValid:: Fsm -> Bool
isFsmValid fsm
            | not $ all isStringDigital (states fsm) = False
            | not $ all isStringDigital (initialState fsm) = False
            | not $ all isStringDigital (finiteState fsm) = False
            | otherwise = True

isStringDigital :: String -> Bool
isStringDigital xs = ((length (filter isDigit xs )) == length (xs))


-- minimalizeFsm
------------------------------------------
minimalizeFsm :: Fsm -> Set.Set String
minimalizeFsm fsm = Set.fromList $ states fsm
