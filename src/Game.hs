module Game where

import Data.Array


data Player = PlayerB | PlayerW deriving (Eq, Show)
data Cell = Empty | Full Player | Text1 | Text2 | Text3 | Text4 | Text5 | Text6 | Text7 | Text8 deriving (Eq, Show)
data State = Running | GameOver (Maybe Player) deriving (Eq, Show)

type Board = Array (Int, Int) Cell

data Game = Game { gameBoard :: Board
                 , gamePlayer :: Player
                 , gameState :: State
                 } deriving (Eq, Show)

n :: Int           -- no of blocks(rows or columns)
n = 9

screenWidth :: Int
screenWidth = 720

screenWidth1 :: Int
screenWidth1 =  720

screenHeight :: Int
screenHeight = 720

screenHeight1 :: Int
screenHeight1 = 720


cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n

initialGame = Game { gameBoard = (array indexRange $ zip (range indexRange) (cycle [Empty])) // [ ((3, 3), Full PlayerB)
                                                                                               , ((3, 4), Full PlayerW)
                                                                                               , ((4, 4), Full PlayerB)
                                                                                               , ((4, 3), Full PlayerW)
                                                                                               , ((8,0), Text1)
                                                                                               , ((8,1), Text2)
                                                                                               , ((8,2), Text3)
                                                                                               , ((8,3), Text4)
                                                                                               , ((8,4), Text5)
                                                                                               , ((8,5), Text6)
                                                                                               , ((8,6), Text7)
                                                                                               , ((8,7), Text8)
                                                                                               , ((7,8), Text8)
                                                                                               , ((6,8), Text7)
                                                                                               , ((5,8), Text6)
                                                                                               , ((4,8), Text5)
                                                                                               , ((3,8), Text4)
                                                                                               , ((2,8), Text3)
                                                                                               , ((1,8), Text2)
                                                                                               , ((0,8), Text1)]
                   , gamePlayer =  PlayerB
                   , gameState = Running
                   }
    where indexRange = ((0, 0), (n-1, n-1))
