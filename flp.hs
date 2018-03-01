import System.IO
import System.Environment
import Data.List
import Control.Monad
import Data.List.Split
import Data.Char (isDigit)
import qualified Data.Set as Set

import ArgumentParser

data Fsm = Fsm { states ::  Set.Set String
               , initialState :: Set.Set String
               , finiteState ::  Set.Set String
               , rules :: Set.Set [String]
               , alphabet :: Set.Set String
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

    print parsedFsm

    when (not $ isFsmValid parsedFsm) (error "Fsm is not valid")

    when (isI parsedArgs) (printFsm parsedFsm)

    when (isT parsedArgs) (print $ minimalizeFsm parsedFsm)

    when (isT parsedArgs) (printFsm $ minimalizeFsm parsedFsm)

printFsm:: Fsm -> IO ()
printFsm x = do
    putStrLn $ getString $ Set.toList $(states x)
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

parse:: String -> Fsm
parse fsm = Fsm { states = Set.fromList (parsedFsm !! 0)
                , initialState = Set.fromList (parsedFsm !! 1)
                , finiteState = Set.fromList (parsedFsm !! 2)
                , rules = Set.fromList (drop 3 parsedFsm)
                , alphabet = Set.fromList $ map (!! 1) (drop 3 parsedFsm)
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

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

getSynkName :: Fsm -> String
getSynkName fsm = show (succ (read (maximum (states fsm))) :: Int) :: String

-- minimalizeFsm
------------------------------------------
minimalizeFsm :: Fsm -> Fsm
minimalizeFsm fsm = if isFsmTotal fsm
                        then minimalizeTotalFsm fsm
                        else minimalizeTotalFsm $ makeTotalFsm fsm

makeTotalFsm :: Fsm -> Fsm
makeTotalFsm fsm = Fsm { states = newStates
                       , initialState = initialState fsm
                       , finiteState = finiteState fsm
                       , rules = Set.fromList $ [generateIfNotExist (rules fsm) (getSynkName fsm) x y | x <- (Set.toList newStates), y <- (Set.toList $ alphabet fsm)]
                       , alphabet = alphabet fsm
                       }
                       where newStates = Set.insert (getSynkName fsm) (states fsm)
                             isExistTransitionsInRules = isExistTransition (rules fsm)


-- TODO
minimalizeTotalFsm :: Fsm -> Fsm
minimalizeTotalFsm fsm = fsm

getAllSource :: Set.Set [String] -> Set.Set String
getAllSource transitions = Set.map (!! 0) transitions

getSymbolBySource ::  Set.Set [String] -> String -> Set.Set String
getSymbolBySource transitions state = Set.map (!! 1) (Set.filter (\x -> (x !! 0) == state ) transitions)

generateIfNotExist :: Set.Set [String] -> String -> String -> String -> [String]
generateIfNotExist rules synk state symbol = if isExistTransition rules state symbol
                                            then [state, symbol, getDestinationBySourceAndSymbol rules state symbol]
                                            else [state, symbol, synk]

isExistTransition :: Set.Set [String] -> String -> String -> Bool
isExistTransition rules state symbol = not $ Set.null $ Set.filter (\x -> (x !! 0) == state && (x !! 1) == symbol) (rules)

getDestinationBySourceAndSymbol:: Set.Set [String] -> String -> String -> String
getDestinationBySourceAndSymbol rules state symbol = map (!! 0) $ Set.toList $ Set.map (!! 2) (Set.filter (\x -> (x !! 0) == state && (x !! 1) == symbol) (rules))

isFsmTotal :: Fsm -> Bool
isFsmTotal fsm = getAllSource (rules fsm) == states fsm
                 && Set.filter (\x -> getSymbolBySource (rules fsm) x == alphabet fsm) (states fsm) == states fsm
