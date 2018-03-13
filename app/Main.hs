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

        -- Check for comments and spelling
        let commentErrorCheck = checkComments linesOfFile
        outputError outFileName commentErrorCheck

        let labels = ["Name:","forced partial assignment:","forbidden machine:","too-near tasks:","machine penalties:","too-near penalities"]
            labelErrorCheck = checkLabels linesOfFile labels
        outputError outFileName labelErrorCheck

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
        print ("Final Matches: " ++ finalMatches)
        outputError outFileName (solutionValid finalMatches)
        let quality = getQual finalMatches finalMatches grid tooNearPen
    
        -- Solution filler
        let solution = makeSolution finalMatches quality
        print solution
    
        -- Print output file
        output args solution

    -- Check for comments in file
    checkComments :: [String] -> String
    checkComments [] = ""
    checkComments (aLine:linesOfFile)
        | hasCommentChar aLine commentChars       = "Error while parsing input file"
        | otherwise                               = checkComments linesOfFile
        where commentChars = "!@#$%^&*/"
    
    hasCommentChar :: String -> String -> Bool
    hasCommentChar [] _ = False
    hasCommentChar _ [] = False
    hasCommentChar aLine (char:chars)
        | char `elem` aLine     = True
        | otherwise             = hasCommentChar aLine chars

    -- Check labels for correct order and spelling
    checkLabels :: [String] -> [String] -> String
    checkLabels [] _ = ""
    checkLabels _ [] = ""
    checkLabels linesOfFile (aLabel:labels)
        | foundLabel linesOfFile aLabel          = checkLabels linesOfFile labels
        | otherwise                             = "Error while parsing input file"
    
    foundLabel :: [String] -> String -> Bool
    foundLabel [] _ = False
    foundLabel (x:xs) label
        | x == label        = True
        | otherwise         = foundLabel xs label
    
    -- Make solution string for output
    makeSolution :: String -> Int -> String
    makeSolution matches quality = "Solution " ++ matchWithSpaces ++ "; Quality: " ++ qualityString
        where matchWithSpaces = intersperse ' ' matches
              qualityString = show quality
    
    -- Check if forced match is valid
    forcedMatchValid :: String -> String
    forcedMatchValid [] = ""
    forcedMatchValid x =
        if x == "No valid solution possible!" then x else ""

    -- Check if solution is valid
    solutionValid :: String -> String
    solutionValid [] = ""
    solutionValid (x:xs)
        | x == 'x'          = "No valid solution possible!"
        | otherwise         = solutionValid xs