import System.IO
import System.Environment
import Data.List
import Data.List.Split

data Fsm = Fsm { states ::  [String]
               , initialState :: [String]
               , finiteState ::  [String]
               , rules :: [[String]]
               } deriving (Show)

main :: IO ()
main = do
    args <- getArgs
    print args
    fsm <- if length args == 0
                then
                    readStdin
                else
                    readFromFile (head args)
    let parsedFsm = parse fsm
    print $ rules parsedFsm
    printFsm parsedFsm
    -- print(splitOn "," "my,comma,separated,list")

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
