import System.IO
import System.Environment
import Data.List
import Data.List.Split

data Fsm = Fsm { states ::  [String]
               , initialState :: [String]
               , finiteState ::  [String]
               , rules :: [[String]]
               } deriving (Show)

main = do
    args <- getArgs
    print args
    fsm <- if length args == 0
                then
                    readStdin
                else
                    readFromFile (head args)
    let x = parse fsm
    printFsm
    -- print(splitOn "," "my,comma,separated,list")

printFsm = do
    print "helloWorld"
    
readStdin:: IO String
readStdin = do
    contents <- getContents
    return contents

readFromFile :: String -> IO String
readFromFile filename = do
    contents <- readFile filename
    return contents

-- parse:: String -> Fsm
parse fsm = Fsm { states = (parsedFsm !! 0)
                , initialState = (parsedFsm !! 1)
                , finiteState = (parsedFsm !! 2)
                , rules = (drop 3 parsedFsm)
                }
    where fsmByLines = lines fsm
          parsedFsm = map (splitOn ",") fsmByLines


-- parse fsm =
--     let fsmByLines = lines fsm
--         parsedFsm = map (splitOn ",") fsmByLines
--     in map (splitOn ",") fsmByLines
    -- in Fsm {states = (head parsedFsm),
    --         initialState = [],
    --         finiteState = [],
    --         rules = [[]]
    --         }
