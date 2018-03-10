-- Project: dka2mka
-- Autor: Bc. Miroslav xnovak1k
-- Project description: Application which convert deterministic finite state machine to
--                      minimal finita state machine.
-- Module description: This module handles all operation with Fsm - parsing, validation and printing

module FsmMinimalizer (minimalizeFsm) where

import Data.List
import Debug.Trace
import Data.Maybe
import FsmParser
import qualified Data.Set as Set

getSynkName :: Fsm -> String
getSynkName fsm = "x"


-- Minimalizing input Fsm based on Hopcroft's algorithm : https://en.wikipedia.org/wiki/DFA_minimization
------------------------------------------
minimalizeFsm :: Fsm -> Fsm
minimalizeFsm fsm = if isFsmTotal fsm
                        then minimalizeTotalFsm $ fsm
                        else minimalizeTotalFsm $ makeTotalFsm fsm

-- Convert input Fsm to total Fsm
------------------------------------------
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

-- Generate new rules if not exist transition from source to dest
------------------------------------------
generateIfNotExist :: Set.Set [String] -> String -> String -> String -> [String]
generateIfNotExist rules synk state symbol = if isExistTransition rules state symbol
                                              then [state, symbol, (getDestinationBySourceAndSymbol rules state symbol)]
                                              else [state, symbol, synk]

-- Check if input Fsm is Total
------------------------------------------
isFsmTotal :: Fsm -> Bool
isFsmTotal fsm = if Set.size (alphabet fsm) == 0
                    then True
                    else getAllSource (rules fsm) == states fsm
                         && Set.filter (\x -> getSymbolBySource (rules fsm) x == alphabet fsm) (states fsm) == states fsm


-- Minimalization of total Fsm (minimalization is possible only to total Fsm)
------------------------------------------
minimalizeTotalFsm :: Fsm -> Fsm
minimalizeTotalFsm fsm = Fsm { states = newStates
                             , initialState = newInitialStates
                             , finiteState = newFiniteState
                             , rules = newRules
                             , alphabet = alphabet fsm
                             }
                         where p = Set.insert (Set.toList $ finiteState fsm) (Set.insert (Set.toList $ Set.difference (states fsm) (finiteState fsm)) (Set.empty))
                               w = Set.insert (Set.toList $ finiteState fsm) Set.empty
                               partitions = getPartions fsm p w
                               newStates = Set.fromList $ map (show) (map (fst) (zip [1..] . sort . map sort $ (Set.toList partitions)))
                               initialStates =  Set.toList $ initialState fsm
                               finiteStates = Set.toList $ finiteState fsm
                               getPartionIndexByPartions = getPartionIndex partitions
                               convertStateToMimilizedStatePart = convertStateToMimilizedState partitions
                               newInitialStates = Set.map (getPartionIndexByPartions) (Set.filter (\x -> length (x `intersect` initialStates) /= 0) (partitions))
                               newFiniteState = Set.map (getPartionIndexByPartions) (Set.filter (\x -> length (x `intersect` finiteStates) /= 0) (partitions))
                               newRules = Set.map (\x -> (convertStateToMimilizedStatePart $ x !! 0):(x !! 1):(convertStateToMimilizedStatePart $ x !! 2):[]) (rules fsm)

-- Get index of partion in set of partions
------------------------------------------
getPartionIndex :: Set.Set [String] -> [String] -> String
getPartionIndex partitions state = show ((fromJust $ elemIndex state (Set.toList partitions)) + 1)

-- Converts state to state representation after minimalization
------------------------------------------
convertStateToMimilizedState :: Set.Set [String] -> String -> String
convertStateToMimilizedState partions state = x
                             where onePartion = filter (\x -> state `elem` x) (Set.toList partions)
                                   x = getPartionIndex partions (head onePartion)

-- Get new partions of minimal Fsm
------------------------------------------
getPartions :: Fsm -> Set.Set [String] -> Set.Set [String] -> Set.Set [String]
getPartions fsm p w = if Set.null w
                        then p
                        -- else trace ("\n getPartions " ++ show (alphabet fsm) ) getPartions fsm p' w'
                        else getPartions fsm p' w'
                        where a = head $ Set.toList w
                              ww = Set.delete a w
                              alpha = Set.toList $ alphabet fsm
                              x = forEachLetterInAlphabet fsm alpha p ww a
                              p' = fst x
                              w' = snd x

-- Do for each letter iteration in Hopcroft algoritm
------------------------------------------
forEachLetterInAlphabet :: Fsm -> [String] -> Set.Set [String] -> Set.Set [String] -> [String] -> (Set.Set [String], Set.Set [String])
forEachLetterInAlphabet fsm alpha p ww a = if length alpha == 0
                            then (p, ww)
                            -- else trace ("forEachLetterInAlphabet" ++ show alpha) (forEachLetterInAlphabet fsm alpha' p' ww' a)
                            else  forEachLetterInAlphabet fsm alpha' p' ww' a
                            where letter = head alpha
                                  alpha' = delete letter alpha
                                  x = doInnerFor fsm letter a p ww
                                  p' = fst x
                                  ww' = snd x

-- Do statement in for each letter loop
------------------------------------------
doInnerFor :: Fsm -> String -> [String] -> Set.Set [String] -> Set.Set [String] -> (Set.Set [String], Set.Set [String])
doInnerFor fsm letter a p ww = (p', ww')
                             where x = Set.toList $ getSourceBySymbolAndDest fsm letter a
                                   yy = Set.filter (\y -> length (y `intersect` x) /= 0 && length (y \\ x) /= 0 ) p
                                   xx = forEachY p ww x yy
                                   p' = fst xx
                                   ww' = snd xx

-- Do last loop in Hopcroft algoritm
------------------------------------------
forEachY :: Set.Set [String] -> Set.Set [String] -> [String] -> Set.Set [String] -> (Set.Set [String], Set.Set [String])
forEachY p ww x y = if Set.null y
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

-- Get all source (left side) of the rules
------------------------------------------
getAllSource :: Set.Set [String] -> Set.Set String
getAllSource rules = Set.map (head) rules

-- Gett all symbols (letters) by single source
------------------------------------------
getSymbolBySource ::  Set.Set [String] -> String -> Set.Set String
getSymbolBySource transitions state = Set.map (!! 1) (Set.filter (\x -> (x !! 0) == state ) transitions)

-- Get all source (left side) by symbol and destination (rigth side)
------------------------------------------
getSourceBySymbolAndDest :: Fsm -> String -> [String] -> Set.Set String
getSourceBySymbolAndDest fsm symbol dest = Set.map (!! 0) (Set.filter (\x -> (x !! 1) == symbol && (x !! 2) `elem` dest) (rules fsm))


-- Check if exist this rule (transition)
------------------------------------------
isExistTransition :: Set.Set [String] -> String -> String -> Bool
isExistTransition rules state symbol = not $ Set.null $ Set.filter (\x -> (x !! 0) == state && (x !! 1) == symbol) (rules)

-- Get all destination by source symbol
------------------------------------------
getDestinationBySourceAndSymbol:: Set.Set [String] -> String -> String -> String
getDestinationBySourceAndSymbol rules state symbol = head $ Set.toList $ Set.map (!! 2) $
                                                           Set.filter (\x -> (x !! 0) == state && (x !! 1) == symbol)(rules)
