module Rendering where

import Data.Array

import Graphics.Gloss

import Game

boardGridColor = makeColorI 128 128 128 255
playerBColor = makeColorI 0 0 0 255
playerWColor = makeColorI 255 255 255 255

boardAsRunningPicture board = 
	pictures [ color playerBColor $ bCellsOfBoard board
	         , color playerWColor $ wCellsOfBoard board
	         , color boardGridColor $ boardGrid
	         ]

snapPictureToCell picture (row, column) = translate x y picture
    where x = fromIntegral column * cellWidth + cellWidth * 0.5
          y = fromIntegral row * cellHeight + cellHeight * 0.5


bCell :: Picture
bCell = circleSolid radius
	where radius = cellWidth * 0.25

wCell :: Picture
wCell = circleSolid radius
	where radius = cellWidth * 0.25

cellsOfBoard :: Board -> Cell -> Picture -> Picture
cellsOfBoard board cell cellPicture = 
	pictures
	$ map (snapPictureToCell cellPicture . fst)
	$ filter (\(_, e) -> e == cell)
	$ assocs board

bCellsOfBoard :: Board -> Picture
bCellsOfBoard board = cellsOfBoard board (Full PlayerB) bCell

wCellsOfBoard :: Board -> Picture
wCellsOfBoard board = cellsOfBoard board (Full PlayerW) wCell

boardGrid :: Picture
boardGrid = 
	pictures
	$ concatMap (\i -> [ line [ (i * cellWidth, 0.0)
		                      , (i * cellWidth, fromIntegral screenHeight)
		                      ]
		               , line [ (0.0, i * cellHeight)
		                      , (fromIntegral screenWidth, i * cellHeight)
		                      ]
		               	])
	  [0.0 .. fromIntegral n]

boardAsPicture board = 
	pictures [ bCellsOfBoard board
	         , wCellsOfBoard board
	         , boardGrid
	         ]


gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                                frame
     where frame = case gameState game of
     	            Running -> boardAsRunningPicture (gameBoard game)