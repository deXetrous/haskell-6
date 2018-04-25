module Game where

import Data.Array

data Player = PlayerB | PlayerW deriving (Eq, Show)
data Cell = Empty | Full Player deriving (Eq, Show)
data State = Running | GameOver (Maybe Player) deriving (Eq, Show)

type Board = Array (Int, Int) Cell

data Game = Game { gameBoard :: Board
                 , gamePlayer :: Player
                 , gameState :: State
                 } deriving (Eq, Show)

n :: Int           -- no of blocks(rows or columns)
n = 8

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 640

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n

initialGame = Game { gameBoard = (array indexRange $ zip (range indexRange) (cycle [Empty])) // [ ((3, 3), Full PlayerB)
                                                                                               , ((3, 4), Full PlayerW)
                                                                                               , ((4, 4), Full PlayerB)
                                                                                               , ((4, 3), Full PlayerW)]
                   , gamePlayer =  PlayerW
                   , gameState = Running
                   }
    where indexRange = ((0, 0), (n-1, n-1))