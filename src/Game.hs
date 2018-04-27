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
screenWidth = 810

screenWidth1 :: Int
screenWidth1 =  720

screenHeight :: Int
screenHeight = 810

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
                                                                                               , ((8,1), Text1)
                                                                                               , ((8,2), Text2)
                                                                                               , ((8,3), Text3)
                                                                                               , ((8,4), Text4)
                                                                                               , ((8,5), Text5)
                                                                                               , ((8,6), Text6)
                                                                                               , ((8,7), Text7)
                                                                                               , ((8,8), Text8)
                                                                                               , ((0,0), Text8)
                                                                                               , ((1,0), Text7)
                                                                                               , ((2,0), Text6)
                                                                                               , ((3,0), Text5)
                                                                                               , ((4,0), Text4)
                                                                                               , ((5,0), Text3)
                                                                                               , ((6,0), Text2)
                                                                                               , ((7,0), Text1)]
                   , gamePlayer =  PlayerB
                   , gameState = Running
                   }
    where indexRange = ((0, 0), (n-1, n-1))
