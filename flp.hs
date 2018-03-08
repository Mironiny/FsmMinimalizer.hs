import System.IO
import System.Environment
import Data.List
import Control.Monad
import Data.List.Split
import Debug.Trace
import Data.Char (isDigit)
import Data.Maybe
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

    print $ alphabet parsedFsm

    -- print parsedFsm
    --
    when (not $ isFsmValid parsedFsm) (error "Fsm is not valid")

    when (isI parsedArgs) (printFsm parsedFsm)

    when (isT parsedArgs) (printFsm $ minimalizeFsm parsedFsm)

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
            | not $ all isStringDigital (Set.toList (states fsm)) = False
            | not $ all isStringDigital (Set.toList (initialState fsm)) = False
            | not $ all isStringDigital (Set.toList (finiteState fsm)) = False
            | otherwise = True

isStringDigital :: String -> Bool
isStringDigital xs = ((length (filter isDigit xs )) == length (xs))

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

getSynkName :: Fsm -> String
getSynkName fsm = show (succ (read (maximum (Set.toList $ states fsm))) :: Int) :: String


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
                       , rules = rules'
                       , alphabet = alphabet fsm
                       }
                       where newStates = Set.insert (getSynkName fsm) (states fsm)
                             isExistTransitionsInRules = isExistTransition (rules fsm)
                             rules' = Set.fromList $ [generateIfNotExist (rules fsm) (getSynkName fsm) x y
                               | x <- (Set.toList newStates), y <- (Set.toList $ alphabet fsm)]


-- TODO
minimalizeTotalFsm :: Fsm -> Fsm
minimalizeTotalFsm fsm = Fsm { states = newStates
                             , initialState = newInitialStates
                             , finiteState = newFiniteState
                             , rules = newRules
                             , alphabet = alphabet fsm
                             }
-- minimalizeTotalFsm fsm = trace ("minimalizeTotalFsm" ++ show partitions ++ " new states " ++ show newStates ++ " new rules " ++ show newRules) fsm
                         where p = Set.insert (Set.toList $ finiteState fsm) (Set.insert (Set.toList $ Set.difference (states fsm) (finiteState fsm)) (Set.empty))
                               w = Set.insert (Set.toList $ finiteState fsm) Set.empty
                               partitions = getPartions fsm p w
                               newStates = Set.fromList $ map (show) (map (fst) (zip [1..] . sort . map sort $ (Set.toList partitions)))
                               initialStates =  Set.toList $ initialState fsm
                               finiteStates = Set.toList $ finiteState fsm
                               convertStateToMimilizedStateByPartions = convertStateToMimilizedState partitions
                               afterMinPart = afterMin partitions
                               newInitialStates = Set.map (convertStateToMimilizedStateByPartions) (Set.filter (\x -> length (x `intersect` initialStates) /= 0) (partitions))
                               newFiniteState = Set.map (convertStateToMimilizedStateByPartions) (Set.filter (\x -> length (x `intersect` finiteStates) /= 0) (partitions))
                               newRules = Set.map (\x -> (afterMinPart $ x !! 0):(x !! 1):(afterMinPart $ x !! 2):[]) (rules fsm)

convertStateToMimilizedState :: Set.Set [String] -> [String] -> String
convertStateToMimilizedState partitions state = show ((fromJust $ elemIndex state (Set.toList partitions)) + 1)

afterMin :: Set.Set [String] -> String -> String
afterMin partions state = x
                        where onePartion = filter (\x -> state `elem` x) (Set.toList partions)
                              x = convertStateToMimilizedState partions (head onePartion)

getPartions :: Fsm -> Set.Set [String] -> Set.Set [String] -> Set.Set [String]
getPartions fsm p w = if Set.null w
                        then p
                        -- else trace ("\n getPartions " ++ show (alphabet fsm) ) getPartions fsm p' w'
                        else getPartions fsm p' w'
                        where a = head $ Set.toList w
                              ww = Set.delete a w
                              alpha = Set.toList $ alphabet fsm
                              x = newValues fsm alpha p ww a
                              p' = fst x
                              w' = snd x

---------- Toto urcite zle
newValues :: Fsm -> [String] -> Set.Set [String] -> Set.Set [String] -> [String] -> (Set.Set [String], Set.Set [String])
newValues fsm alpha p ww a = if length alpha == 0
                            then (p, ww)
                            -- else trace ("newValues" ++ show alpha) (newValues fsm alpha' p' ww' a)
                            else  newValues fsm alpha' p' ww' a
                            where letter = head alpha
                                  alpha' = delete letter alpha
                                  x = innerFor fsm letter a p ww
                                  p' = fst x
                                  ww' = snd x

innerFor :: Fsm -> String -> [String] -> Set.Set [String] -> Set.Set [String] -> (Set.Set [String], Set.Set [String])
innerFor fsm letter a p ww = (p', ww')
                             where x = Set.toList $ getSourceBySymbolAndDest fsm letter a
                                   yy = Set.filter (\y -> length (y `intersect` x) /= 0 && length (y \\ x) /= 0 ) p
                                   xx = lastFor p ww x yy
                                   p' = fst xx
                                   ww' = snd xx

lastFor :: Set.Set [String] -> Set.Set [String] -> [String] -> Set.Set [String] -> (Set.Set [String], Set.Set [String])
lastFor p ww x y = if Set.null y
                    then (p, ww)
                    -- else trace ("fuu") (p'', w')
                    else (p'', w')
                    where yy = head $ Set.toList y
                          p' = Set.delete yy p
                          p'' = Set.insert (yy \\ x) (Set.insert (intersect x yy) p')
                          w' = if (yy `Set.member` ww)
                                then Set.insert (yy \\ x) (Set.insert (intersect x yy) (Set.delete yy ww))
                                else if length (intersect x yy ) <= length (yy \\ x)
                                        then Set.insert (intersect x yy ) ww
                                        else Set.insert (yy \\ x) ww


try :: Fsm -> [String]
try fsm = alpha
          where p = Set.insert (Set.toList $ finiteState fsm) (Set.insert (Set.toList $ Set.difference (states fsm) (finiteState fsm)) (Set.empty))
                w = Set.insert (Set.toList $ finiteState fsm) Set.empty
                alpha = Set.toList $ alphabet fsm

getAllSource :: Set.Set [String] -> Set.Set String
getAllSource transitions = Set.map (head) transitions

getSymbolBySource ::  Set.Set [String] -> String -> Set.Set String
getSymbolBySource transitions state = Set.map (!! 1) (Set.filter (\x -> (x !! 0) == state ) transitions)

getSourceBySymbolAndDest :: Fsm -> String -> [String] -> Set.Set String
getSourceBySymbolAndDest fsm symbol dest = Set.map (!! 0) (Set.filter (\x -> (x !! 1) == symbol && (x !! 2) `elem` dest) (rules fsm))

generateIfNotExist :: Set.Set [String] -> String -> String -> String -> [String]
generateIfNotExist rules synk state symbol = if isExistTransition rules state symbol
                                            then [state, symbol, getDestinationBySourceAndSymbol rules state symbol]
                                            else [state, symbol, synk]

isExistTransition :: Set.Set [String] -> String -> String -> Bool
isExistTransition rules state symbol = not $ Set.null $ Set.filter (\x -> (x !! 0) == state && (x !! 1) == symbol) (rules)

getDestinationBySourceAndSymbol:: Set.Set [String] -> String -> String -> String
getDestinationBySourceAndSymbol rules state symbol = map (!! 0) $ Set.toList $ Set.map (!! 2) $
                                                           Set.filter (\x -> (x !! 0) == state && (x !! 1) == symbol)(rules)



isFsmTotal :: Fsm -> Bool
isFsmTotal fsm = getAllSource (rules fsm) == states fsm
                 && Set.filter (\x -> getSymbolBySource (rules fsm) x == alphabet fsm) (states fsm) == states fsm
