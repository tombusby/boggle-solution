
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

isValidPos :: Position -> Bool
isValidPos (x, y) = x >= 0 && x < boardSize && y >= 0 && y < boardSize

getNextChar :: [Position] -> Position -> Board -> [(Position, Char)]
getNextChar seen p board = above ++ below ++ left ++ right
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

