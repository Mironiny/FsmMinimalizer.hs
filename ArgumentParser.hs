module ArgumentParser where

data ParsedArgs = ParsedArgs { isValid :: Bool
                             , fileName :: String
                             , isI :: Bool
                             , isT :: Bool
                             } deriving (Show)

-- parseArgs
------------------------------------------
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
