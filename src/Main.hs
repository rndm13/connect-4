module Main 
    ( main
    ) where

import Game
import Strategies
import Text.Printf
import Text.Read
import Data.Maybe

selectMenu :: String -> [(String, a)] -> IO a
selectMenu msg arr = do
    putStrLn msg
    let newArr = zipWith (\x (y,z) -> (printf "%d - %s" x y, z)) [(1 :: Int)..] arr 
    mapM_ (putStrLn . fst) newArr
    input <- getLine
    let selected = readMaybe input 
    if isJust selected && fromJust selected >= 1 && fromJust selected <= length arr
    then return . snd $ arr !! ((fromJust selected) - 1)
    else do
        putStrLn "Invalid input."
        r <- selectMenu msg arr
        return r

menuAI :: Player -> IO Strategy
menuAI player = selectMenu (printf "Select Strategy for %s" (showPlayer player))
                [ ("Human player.",humanPlayer)
                , ("Fill all columns (AI)", columnFillAI)
                , ("Fill all rows (AI)", rowFillAI)
                , ("Random moves (AI)", randomMoveAI)
                , ("Minmax AI (depth 5)", minMaxAI 5)
                ]

main :: IO ()
main = do 
    p1strat <- (menuAI True)
    p2strat <- (menuAI False)
    gameLoop True startingBoard p1strat p2strat

