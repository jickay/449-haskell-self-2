module Main where

    import Input
    import Parser
    import HardConstraints
    import SoftConstraints
    
    import System.Environment
    import Data.List
    
    main :: IO()
    main = do
        -- Store arguments in arg
        args <- getArgs
        -- Get input
        contents <- input args
        let linesOfFile = lines contents
            outFileName = args !! 1
        print linesOfFile
    
        -- Parser
        let forcedPairs = getTupleSection linesOfFile "forced partial assignment:"
            forbidPairs = getTupleSection linesOfFile "forbidden machine:"
            tooNearPairs = getTupleSection linesOfFile "too-near tasks:"
            grid = getGridSection linesOfFile "machine penalties:"
            tooNearPen = getTripleSection linesOfFile "too-near penalities"
        print forcedPairs
        print forbidPairs
        print tooNearPairs
        print grid
        print tooNearPen

        outputError outFileName (validMachTask forcedPairs)
        outputError outFileName (validMachTask forbidPairs)
        outputError outFileName (validTaskTask tooNearPairs)
        outputError outFileName (validGrid grid)
        outputError outFileName (validTask tooNearPen)
        outputError outFileName (forcedDoubles forcedPairs)

        -- Hard Constraints
        let matches = "xxxxxxxx"
            forcedMatches = makeForced forcedPairs forbidPairs tooNearPairs matches
        outputError outFileName (forcedMatchValid forcedMatches)
        print ("Forced matches: " ++ forcedMatches)
    
        -- Soft Constraints
        let finalMatches = iterateMatches forcedMatches grid tooNearPen forbidPairs
        -- outputError outFileName finalMatches
        let quality = getQual finalMatches grid tooNearPen
    
        -- Solution filler
        let solution = makeSolution finalMatches quality
        print solution
    
        -- Print output file
        output args solution
    
    -- Make solution string for output
    makeSolution :: String -> Int -> String
    makeSolution matches quality = "Solution " ++ matchWithSpaces ++ "; Quality: " ++ qualityString
        where matchWithSpaces = intersperse ' ' matches
              qualityString = show quality

    forcedMatchValid :: String -> String
    forcedMatchValid [] = ""
    forcedMatchValid matches =
        if matches == "No valid solution possible!" then matches else ""

    solutionValid :: String -> String
    solutionValid [] = ""
    solutionValid (x:xs)
        | x == 'x'          = "No valid solution possible!"
        | otherwise         = solutionValid xs