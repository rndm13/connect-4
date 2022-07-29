module Game
    ( Player , Cell , Board , Move , Strategy
    , gameLoop
    , colCnt , rowCnt
    , checkWin , checkDraw 
    , startingBoard
    , tryMove, doMove
    , showPlayer) 
    where

import Data.Maybe
import Data.List
import Control.Monad
import Text.Read
import Text.Printf

type Player   = Bool
type Cell     = Maybe Player
type Board    = [[Cell]]
type Move     = (Player, Int)
type Strategy = Player -> Board -> IO Move

showPlayer :: Player -> String
showPlayer True  = "Player 1"
showPlayer False = "Player 2"

showBoard :: Board -> String
showBoard brd = intercalate "\n" $ map (map (maybe '.' (\b -> if b then 'x' else 'o'))) brd

rowCnt :: Int
rowCnt = 6
colCnt :: Int
colCnt = 7

replace :: [a] -> Int -> a -> [a]
replace arr ind element = (fst splits) ++ (element : (tail . snd $ splits))
    where
  splits = splitAt ind arr

startingBoard :: Board
startingBoard = replicate rowCnt row
    where
  row = replicate colCnt Nothing

tryMove :: Move -> Board -> Maybe Board
tryMove (player, col) brd = (\r -> replace brd r (replace (brd !! r) col (Just player))) <$> row
    where
  row = ((rowCnt - 1) -) <$> findIndex (isNothing) (reverse ((transpose brd) !! col))

doMove :: Move -> Board -> IO (Board, Player) 
doMove (player, col) brd
  | isNothing nextBrd = do 
    putStrLn "Illegal move, the column is already filled."
    return (brd, player)
  | isJust    nextBrd = do 
    putStrLn . showBoard . fromJust $ nextBrd
    return (fromJust nextBrd, not player)
      where
    nextBrd = tryMove (player, col) brd 

check4InRow :: [Cell] -> Maybe Player
check4InRow [] = Nothing
check4InRow arr
  | fgLen == 4 = fgElem
  | otherwise  = check4InRow . tail $ arr
      where
    fg     = head . group . take 4 $ arr
    fgElem = head fg
    fgLen  = length fg

shiftArray :: [Maybe a] -> Int -> [Maybe a]
shiftArray arr n 
  | n > 0  = replicate n Nothing ++ arr
  | n < 0  = arr ++ (replicate (-n) Nothing)
  | n == 0 = arr

checkWin :: Board -> Maybe Player
checkWin brd = join . (find isJust) . map (join . (find isJust) . (map check4InRow)) $
        [ (brd)           -- Rows
        , (transpose brd) -- Columns
        , transpose $ zipWith shiftArray (transpose brd) [0..]
        , transpose . reverse $ zipWith shiftArray (reverse . transpose $ brd) [0..]
        ]

checkDraw :: Board -> Bool
checkDraw brd = isNothing $ find (\x -> Nothing == head x) $ transpose brd

gameLoop :: Player -> Board -> Strategy -> Strategy -> IO ()
gameLoop player brd strat1 strat2 = do
    let isWin  = checkWin brd     
    if isJust isWin
    then putStrLn $ printf "Player %c has won!" (if (fromJust isWin) then '1' else '2')
    else do
        let isDraw = checkDraw brd 
        if isDraw
        then putStrLn $ "Draw !"
        else do
            move <- (if player then strat1 else strat2) player brd
            (newBrd, newPlayer) <- doMove move brd
            gameLoop newPlayer newBrd strat1 strat2
