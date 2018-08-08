
type Position = (Int, Int)
type Board = [[Char]]
type BoggleWord = [Char]

boardSize :: Int
boardSize = 4

board :: Board
board = ["HELA",
    "AOLA",
    "AQAA",
    "AQAA"]

(!!!) :: [[a]] -> (Int, Int) -> a
(!!!) l (x, y) = (!!x) . (!!y) $ l

isValidWord :: BoggleWord -> Bool
isValidWord "HE" = True
isValidWord "HELL" = True
isValidWord "HELLO" = True
isValidWord _ = False

isValidPos :: Position -> Bool
isValidPos (x, y) = x >= 0 && x < boardSize && y >= 0 && y < boardSize

getNextChar :: [Position] -> Position -> [(Position, Char)]
getNextChar seen p = above ++ below ++ left ++ right
    where
        above = getPosCommon (\(x, y) -> (x, y-1))
        below = getPosCommon (\(x, y) -> (x, y+1))
        left = getPosCommon (\(x, y) -> (x-1, y))
        right = getPosCommon (\(x, y) -> (x+1, y))
        getPosCommon f = let p' = f p in
            if isValidPos p' && (not . elem p') seen then
                [(p', board !!! p')]
            else
                []

getNextStates :: ([Position], BoggleWord) -> [([Position], BoggleWord)]
getNextStates ((p:ps), cs) = [state p' c' | (p', c') <- getNextChar (p:ps) p]
    where
        state p' c' = (p':p:ps, cs ++ [c'])

getAllPaths :: [([Position], BoggleWord)] -> [([Position], BoggleWord)]
getAllPaths [] = []
getAllPaths states = states ++ getAllPaths nextStates
    where
        nextStates = concat [getNextStates state | state <- states]

generateStartStates :: [([Position], BoggleWord)]
generateStartStates = [state x y | x <- [0..n], y <- [0..n]]
    where
        state x y = ([(x,y)], [board !!! (x,y)])
        n = boardSize - 1

findAllValidPaths :: [([Position], BoggleWord)]
findAllValidPaths = filter isValidState $ getAllPaths generateStartStates
    where
        isValidState (_, cs) = isValidWord cs        

getLongestWord :: Maybe ([Position], BoggleWord)
getLongestWord = case findAllValidPaths of
    [] -> Nothing
    xs -> Just $ last xs -- The last in the list is guaranteed to be longest

main = do
     solution <- return getLongestWord
     case solution of
        Nothing -> putStrLn "No valid words were found"
        Just (ps, word) ->
            putStrLn $ "The longest word is " ++ word ++
                " and its path is " ++ (show . reverse $ ps)
