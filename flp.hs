import System.IO
import System.Environment
import Data.List
import Control.Monad
import Data.List.Split

data ParsedArgs = ParsedArgs { isValid :: Bool
                             , fileName :: String
                             , isI :: Bool
                             , isT :: Bool
                             } deriving (Show)

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

    when (isI parsedArgs) (printFsm parsedFsm)

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

parseArgs:: [String] -> ParsedArgs
parseArgs ["-t"] = ParsedArgs { isValid = True
                              , fileName = []
                              , isI = False
                              , isT = True
                              }
parseArgs ["-i"] = ParsedArgs { isValid = True
                              , fileName = []
                              , isI = True
                              , isT = False
                              }
parseArgs ["-t", "-i"] = ParsedArgs { isValid = True
                              , fileName = []
                              , isI = True
                              , isT = True
                              }
parseArgs ["-i", "-t"] = ParsedArgs { isValid = True
                              , fileName = []
                              , isI = True
                              , isT = True
                              }
parseArgs ["-t", filename] = ParsedArgs { isValid = True
                              , fileName = filename
                              , isI = False
                              , isT = True
                              }
parseArgs ["-i", filename] = ParsedArgs { isValid = True
                              , fileName = filename
                              , isI = True
                              , isT = False
                              }
parseArgs ["-t", "-i", filename] = ParsedArgs { isValid = True
                              , fileName = filename
                              , isI = True
                              , isT = True
                              }
parseArgs ["-i", "-t", filename] = ParsedArgs { isValid = True
                              , fileName = filename
                              , isI = True
                              , isT = True
                              }
parseArgs others = ParsedArgs { isValid = False
                              , fileName = []
                              , isI = False
                              , isT = False
                              }
