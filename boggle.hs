
type Position = (Int, Int)
type Board = [[Char]]

boardSize :: Int
boardSize = 4

board :: Board
board = ["HELA",
    "AOLA",
    "AQAA",
    "AQAA"]

(!!!) :: [[a]] -> (Int, Int) -> a
(!!!) l (x, y) = (!!x) . (!!y) $ l

isValidWord :: [Char] -> Bool
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

getNextStates :: ([Position], [Char]) -> [([Position], [Char])]
getNextStates ((p:ps), cs) = [(p':p:ps, cs ++ [c']) | (p', c') <- getNextChar (p:ps) p]

getAllPaths :: [([Position], [Char])] -> [([Position], [Char])]
getAllPaths [] = []
getAllPaths states = states ++ nextStates ++ getAllPaths nextStates
    where
        nextStates = concat [getNextStates state | state <- states]

startState :: ([Position], [Char])
startState = ([(0, 0)], "H")

isValidSate :: ([Position], [Char]) -> Bool
isValidSate (_, cs) = isValidWord cs
