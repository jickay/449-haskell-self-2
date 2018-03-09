module HardConstraints where

-- Check if same machine/task forced more than once
forcedDoubles :: [(Char,Char)] -> String
forcedDoubles [] = ""
forcedDoubles (x:xs)
    | mach `elem` mach'     = "partial assignment error"
    | task `elem` task'     = "partial assignment error"
    | otherwise             = forcedDoubles xs
    where mach = fst x
          task = snd x
          mach' = getMachs xs
          task' = getTasks xs

getMachs :: [(Char,Char)] -> [Char]
getMachs [] = []
getMachs (x:xs) =
    let mach = fst x
    in [mach] ++ getMachs xs

getTasks :: [(Char,Char)] -> [Char]
getTasks [] = []
getTasks (x:xs) =
    let task = fst x
    in [task] ++ getTasks xs

-- Make forced pairs
makeForced :: [(Char,Char)] -> [(Char,Char)] -> [(Char,Char)] -> [Char] -> [Char]
makeForced [] _ _ matches = matches
makeForced (x:xs) forbidden tooNear matches
    | checkForbid x forbidden       = "No valid solution possible!"
    | otherwise                     = makeForced xs newForbid tooNear newMatches
    where newMatches = makePair x matches
          newForbid = forbidden ++ (checkTooNear tooNear (fst x) (snd x))

-- Check if pair to match is forbidden; returns True if forbidden
checkForbid :: (Char,Char) -> [(Char,Char)] -> Bool
checkForbid x forbidden
    | x `elem` forbidden    = True
    | otherwise             = False

-- Make a matched pair
makePair :: (Char,Char) -> [Char] -> [Char]
makePair x matches =
    let mach = fst x     -- turn num string into int index, eg. "1" -> 0
        task = snd x
        index = (read [mach]::Int) - 1
        (as,bs) = splitAt index matches
    in as ++ [task] ++ (tail bs)

-- Check too-near for new forbidden pairs
checkTooNear :: [(Char,Char)] -> Char -> Char -> [(Char,Char)]
checkTooNear [] _ _ = []
checkTooNear (x:xs) task mach
    | task == taskL         = [(machR,taskR)] ++ (checkTooNear xs task mach)
    | task == taskR         = [(machL,taskL)] ++ (checkTooNear xs task mach)
    | otherwise             = checkTooNear xs task mach
    where taskL = fst x
          taskR = snd x
          machL = getMachL mach
          machR = getMachR mach

-- Get neighbour machine for too-near checks
getMachL :: Char -> Char
getMachL mach
    | mach == '1'       = '8'
    | otherwise         = head newMach
    where machInt = (read [mach]::Int) - 1
          newMach = show machInt

getMachR :: Char -> Char
getMachR mach
    | mach == '8'       = '1'
    | otherwise         = head newMach
    where machInt = (read [mach]::Int) + 1
          newMach = show machInt