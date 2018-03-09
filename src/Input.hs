module Input where

import System.Exit

input :: [String] -> IO String
input args =
    -- Get lines from input file name (arg 1)
    getLines (args !! 0)

output :: [FilePath] -> String -> IO()
output args contents =
    -- Write to output file name (arg 2)
    writeFile (args !! 1) contents

getLines :: FilePath -> IO String
getLines fileName = readFile fileName

outputError :: FilePath -> String -> IO()
outputError fileName errorMsg
    | errorMsg == ""        = writeFile fileName errorMsg
    | otherwise             = do
        writeFile fileName errorMsg
        exitFailure