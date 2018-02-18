import System.IO
import System.Environment
import Data.List

main = do
    args <- getArgs
    print args
    fsm <- if length args == 0
                then
                    readStdin
                else
                    readFromFile (head args)
    print fsm

readStdin:: IO String
readStdin = do
    contents <- getContents
    return contents

readFromFile :: String -> IO String
readFromFile filename = do
    contents <- readFile filename
    return contents
