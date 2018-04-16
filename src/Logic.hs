module Logic where

import Data.Array

import Game
import Graphics.Gloss.Interface.Pure.Game

isCoordCorrect = inRange ((0, 0), (n - 1, n - 1))

isCoordAdjacent :: Game -> (Int, Int) -> Bool
isCoordAdjacent game (x, y) |    x+1 < n && board!(x+1, y) == (Full $ player)
                              || x-1 >=0 && board!(x-1, y) == (Full $ player)
                              || y-1 >=0 && board!(x, y-1) == (Full $ player)
                              || y+1 < n && board!(x, y+1) == (Full $ player)
                              || x+1 < n && y+1 < n && board!(x+1, y+1) == (Full $ player) 
                              || x-1 >=0 && y-1 >= 0 && board!(x-1, y-1) == (Full $ player)
                              || x-1 >=0 && y+1 < n && board!(x-1, y+1) == (Full $ player)
                              || x+1 < n && y-1 >= 0 && board!(x+1, y-1) == (Full $ player) 
                               = True
                            | otherwise = False
                            where board = gameBoard game
                                  player = 
                                  	if gamePlayer game == PlayerB
                                  		then PlayerW
                                  	else PlayerB


switchPlayer game = 
	case gamePlayer game of
		PlayerB -> game { gamePlayer = PlayerW}
		PlayerW -> game { gamePlayer = PlayerB}


playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
    | (isCoordCorrect cellCoord) && (isCoordAdjacent game cellCoord) && board ! cellCoord == Empty =
    	switchPlayer 
    	$ game { gameBoard = board // [(cellCoord, Full $ player)]}
    | otherwise = game
    where board = gameBoard game
          player = gamePlayer game



mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
	                         , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
	                         )
	

transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game = 
	case gameState game of
		Running -> playerTurn game $ mousePosAsCellCoord mousePos

transformGame _  game = game