-- Project: dka2mka
-- Autor: Bc. Miroslav xnovak1k
-- Description: Application which convert deterministic finite state machine to
--              minimal finita state machine.

import System.IO
import System.Environment
import Data.List
import Control.Monad
import Debug.Trace
import Data.Maybe
import qualified Data.Set as Set

import ArgumentParser
import FsmParser
import FsmMinimalizer

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
    
    let status = isFsmValid parsedFsm

    when (not $ fst status) (error $ snd status)

    when (isI parsedArgs) (printFsm parsedFsm)

    when (isT parsedArgs) (printFsm $ minimalizeFsm parsedFsm)
