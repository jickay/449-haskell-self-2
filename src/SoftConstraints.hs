module SoftConstraints where

    import HardConstraints
    import Data.List
    
    machines :: [Char]
    machines = "12345678"
    
    tasks :: [Char]
    tasks = "ABCDEFGH"
    
    -- Get quality based on available machines
    getQual :: [Char] -> [Char] -> [[Int]] -> [(Char,Char,Int)] -> Int
    getQual _ [] _ _ = 0
    getQual oMatches matches grid tooNearPen
        | x `elem` tasks    = xPen + nearPen + getQual oMatches xs grid tooNearPen
        | otherwise         = 0 + nearPen + (getQual oMatches xs grid tooNearPen)
        where (x:xs) = matches
              machInt = getIndex (elemIndex x oMatches)
              taskInt = getIndex (elemIndex x tasks)
              xPen = (grid !! machInt) !! taskInt
              nearPen = getNearPen machInt x tooNearPen matches
    
    getNearPen :: Int -> Char -> [(Char,Char,Int)] -> [Char] -> Int
    getNearPen _ _ _ [] = 0
    getNearPen _ _ [] _ = 0
    getNearPen mach task tooNearPen matches
        | task == taskL         = checkNeighbor matches machR taskR pen + getNearPen mach task xs matches
        | task == taskR         = checkNeighbor matches machL taskL pen + getNearPen mach task xs matches
        | otherwise             = getNearPen mach task xs matches
        where (x:xs) = tooNearPen
              taskL = triplefst x
              taskR = triplesnd x
              pen = tripletrd x
              machL = getMachLInt mach
              machR = getMachRInt mach
    
    checkNeighbor :: [Char] -> Int -> Char -> Int -> Int
    checkNeighbor [] _ _ _ = 0
    checkNeighbor matches mach task pen
        | index == mach         = pen
        | otherwise             = 0
        where index = getIndex (elemIndex task matches)
    
    triplefst :: (a,b,c) -> a
    triplefst (a,_,_) = a
    
    triplesnd :: (a,b,c) -> b
    triplesnd (_,b,_) = b
    
    tripletrd :: (a,b,c) -> c
    tripletrd (_,_,c) = c
    
    -- Get neighbour machine for too-near checks
    getMachLInt :: Int -> Int
    getMachLInt mach
        | mach <= 0         = 7
        | otherwise         = mach - 1
    
    getMachRInt :: Int -> Int
    getMachRInt mach
        | mach >= 7         = 0
        | otherwise         = mach + 1
    
    -- Iterate through different possible matches and return best option
    iterateMatches :: [Char] -> [[Int]] -> [(Char,Char,Int)] -> [(Char,Char)] ->[Char]
    iterateMatches [] _ _ _ = []
    iterateMatches matches grid tooNearPen forbidden
        | freeTasks == []           = matches
        | expandedMatches == []     = matches
        | otherwise                 = iterateMatches bestIteration grid tooNearPen forbidden
        where freeTasks = getFreeTasks tasks matches
              expandedMatches = expandMatches 0 matches freeTasks forbidden
              rounds = getRounds expandedMatches grid forbidden
              index = compareMatches rounds grid tooNearPen
              bestIteration = rounds !! index
    
    -- Fill first unmatches row with all possible free tasks
    -- Creates a set of potential matches to compare
    expandMatches :: Int -> [Char] -> [Char] -> [(Char,Char)] -> [[Char]]
    expandMatches _ [] _ _ = []
    expandMatches _ _ [] _ = []
    expandMatches index matches freeTasks forbidden
        | notValid                  = expandMatches 0 matches (tail freeTasks) forbidden
        | task == 'x'               = [matches'] ++ expandMatches 0 matches (tail freeTasks) forbidden
        | otherwise                 = expandMatches (index+1) matches freeTasks forbidden
        where task = matches !! index
              notValid = invalidMatch index (head freeTasks) forbidden
              (as,bs) = splitAt index matches
              matches' = as ++ [(head freeTasks)] ++ (tail bs)
    
    -- Methods to fill out expanded matches
    getRounds :: [[Char]] -> [[Int]] -> [(Char,Char)] -> [[Char]]
    getRounds [] _ _ = []
    getRounds expandedMatches grid forbidden
        | x /= []           = [fillRound x x grid freeTasks forbidden] ++ getRounds xs grid forbidden
        | otherwise         = getRounds xs grid forbidden
        where (x:xs) = expandedMatches
              freeTasks = getFreeTasks tasks x
    
    fillRound :: [Char] -> [Char] -> [[Int]] -> [Char] -> [(Char,Char)] -> [Char]
    fillRound _ [] _ _ _ = []
    fillRound oMatches matches grid freeTasks forbidden
        | x == 'x'              = [fillTask] ++ fillRound oMatches xs grid freeTasks' forbidden
        | otherwise             = [x] ++ fillRound oMatches xs grid freeTasks forbidden
        where (x:xs) = matches
              taskIndex = getIndex (elemIndex x oMatches)
              row = grid !! taskIndex
              fillTask = fillX taskIndex row freeTasks forbidden
              freeTasks' = delete fillTask freeTasks
              
    
    fillX :: Int -> [Int] -> [Char] -> [(Char,Char)] -> Char
    fillX mach row freeTasks forbidden
        | freeTasks == []           = 'x'
        | notValid                  = fillX mach row freeTasks' forbidden
        | validMin                  = task
        | otherwise                 = fillX mach row freeTasks' forbidden
        where task = head freeTasks
              taskIndex = getIndex (elemIndex task tasks)
              notValid = invalidMatch mach task forbidden
              freeTasks' = tail freeTasks
              validMin = isMin row taskIndex
    
    isMin :: [Int] -> Int -> Bool
    isMin row index
        | index > length row                        = False
        | (row !! index) == (minimum row)           = True
        | otherwise                                 = False
    
    -- Compare a list of matches to return the one with lowest penalties
    compareMatches :: [[Char]]-> [[Int]] -> [(Char,Char,Int)] -> Int
    compareMatches _ [] [] = 0
    compareMatches listOfMatches grid tooNearPen =
        let bestMatch = foldl1 (\acc x -> if (getQual x x grid tooNearPen) > (getQual acc acc grid tooNearPen) then acc else x) listOfMatches
            index = getIndex (elemIndex bestMatch listOfMatches)
        in index
    
    -- Return all unmatched tasks that are free to be assigned
    getFreeTasks :: [Char] -> [Char] -> [Char]
    getFreeTasks [] _ = []
    getFreeTasks _ [] = []
    getFreeTasks (x:xs) matches
        | x `notElem` matches       = [x] ++ getFreeTasks xs matches
        | otherwise                 = getFreeTasks xs matches
    
    -- Return index of an element in a list
    getIndex :: Maybe Int -> Int
    getIndex mx = case mx of
        Just x -> x
        Nothing -> 0

    -- Check if pair to match is forbidden; returns True if forbidden
    invalidMatch :: Int -> Char -> [(Char,Char)] -> Bool
    invalidMatch _ _ [] = False
    invalidMatch machInt task forbidden
        | match `elem` forbidden    = True
        | otherwise                 = False
        where mach = head (show (machInt+1))
              match = (mach,task)