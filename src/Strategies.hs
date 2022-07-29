module Strategies
    ( humanPlayer
    , columnFillAI
    , rowFillAI
    , minMaxAI
    , randomMoveAI)
      where

import Game
import Data.Maybe
import Data.List
import Control.Monad
import Text.Read
import Text.Printf
import System.Random
import Data.Time.Clock.POSIX

humanPlayer :: Strategy
humanPlayer player brd = do
    putStrLn $ printf "%s select column for move (1-%d): " (showPlayer player) colCnt
    input <- getLine
    let colMaybe = readMaybe input
    if isJust colMaybe
    then do
        let col = fromJust colMaybe
        if 1 > col  || col >  colCnt 
        then do 
            putStrLn "Invalid move."
            newMove <- humanPlayer player brd
            return newMove
        else return (player, col - 1)
    else do
        putStrLn "Input is not a number."
        newMove <- humanPlayer player brd
        return newMove

columnFillAI :: Strategy
columnFillAI player brd = do
    putStrLn $ printf "AI (%s) moves: " (showPlayer player)
    return (player, (possibleMoves brd) !! 0)

rowFillAI :: Strategy
rowFillAI player brd = do
    putStrLn $ printf "AI (%s) moves: " (showPlayer player)
    let cols = (map (findIndex isNothing)) . (map reverse) . transpose $ brd
    return (player, (fromJust $ findIndex (== minimum cols) cols))

randomMoveAI :: Strategy
randomMoveAI player brd = do
    t <- ((round . (* 1000000)) <$> getPOSIXTime)
    let gen = mkStdGen t
    let pms = possibleMoves brd
    let col = pms !! (fst $ randomR (0, (length pms) - 1) gen)
    putStrLn $ printf "AI (%s) moves: " (showPlayer player)
    return (player, col)

possibleMoves :: Board -> [Int]
possibleMoves brd = map fst $ filter (\(i,x) -> isJust $ find isNothing x) $ zip [0..] (transpose brd)

biasedTranslator :: Player -> Maybe Player -> Int
biasedTranslator _  Nothing = 0
biasedTranslator p1 (Just p2)
  | p1 == p2  =  1
  | otherwise = -1

translateBack :: Player -> Int -> Maybe Player
translateBack p n
  | n == 1  = Just p
  | n == -1 = Just (not p)
  | n == 0  = Nothing

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just x) _       = Just x
firstJust Nothing (Just x) = Just x
firstJust Nothing Nothing  = Nothing

getRecursiveResult :: Int -> Player -> Board -> (Int, Maybe Player)
getRecursiveResult 0 _ _ = (0,Nothing)
getRecursiveResult depth player brd = (fst result, translateBack player $ snd result)
    where
  pms = possibleMoves brd
  boards  = map (\x -> fromJust $ tryMove (player,x) brd) pms
  results = map (\x -> firstJust (checkWin x) (snd $ getRecursiveResult (depth - 1) (not player) x)) boards
  result = ((sortBy (\(col1,x) (col2,y) -> if (x /= y) then (compare y x) else (compare (abs (col1 - 3)) (abs (col2 - 3)))) $ zip pms $ map (biasedTranslator player) results) !! 0)

minMaxAI :: Int -> Strategy
minMaxAI depth player brd = do
    putStrLn $ printf "AI (%s) moves: " (showPlayer player)
    return (player, fst result)
      where
    result = getRecursiveResult depth player brd
