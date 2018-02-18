import System.IO
import System.Environment
import Data.List

main = do
    args <- getArgs
    fsm <- if length args == 0
                then
                    readStdin
                else
                    readFromFile "fsm.txt"
    print fsm

readStdin:: IO String
readStdin = do
    contents <- getContents
    return contents

readFromFile :: String -> IO String
readFromFile filename = do
    contents <- readFile filename 
    return contents
