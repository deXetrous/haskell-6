module Rendering where

import Data.Array

import Graphics.Gloss
import Game

import Logic

boardGridColor = makeColorI 128 128 128 255
playerBColor = makeColorI 0 0 0 255
playerWColor = makeColorI 255 255 255 255
stringColor = makeColorI 0 0 0 255
tieColor = greyN 0.5

boardAsRunningPicture board =
	pictures [ color playerBColor $ bCellsOfBoard board
	         , color playerWColor $ wCellsOfBoard board
	         , color boardGridColor $ boardGrid
					 , color stringColor $ s1CellsOfBoard board
					 , color stringColor $ s2CellsOfBoard board
					 , color stringColor $ s3CellsOfBoard board
					 , color stringColor $ s4CellsOfBoard board
					 , color stringColor $ s5CellsOfBoard board
					 , color stringColor $ s6CellsOfBoard board
					 , color stringColor $ s7CellsOfBoard board
					 , color stringColor $ s8CellsOfBoard board
	         ]

outcomeColor (Just PlayerB) = playerBColor
outcomeColor (Just PlayerW) = playerWColor
outcomeColor Nothing = tieColor

snapPictureToCell picture (row, column) = translate x y picture
    where x = fromIntegral column * cellWidth + cellWidth * 0.5
          y = fromIntegral row * cellHeight + cellHeight * 0.5


bCell :: Picture
bCell = circleSolid radius
	where radius = cellWidth * 0.25

wCell :: Picture
wCell = circleSolid radius
	where radius = cellWidth * 0.25


sCell1 ::  Picture
sCell1 = text str
	where str = "1"

sCell2 ::  Picture
sCell2 = text str
	where str = "2"

sCell3 ::  Picture
sCell3 = text str
	where str = "3"

sCell4 ::  Picture
sCell4 = text str
	where str = "4"

sCell5 ::  Picture
sCell5 = text str
	where str = "5"

sCell6 ::  Picture
sCell6 = text str
	where str = "6"

sCell7 ::  Picture
sCell7 = text str
	where str = "7"

sCell8 ::  Picture
sCell8 = text str
	where str = "8"

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

s1CellsOfBoard :: Board -> Picture
s1CellsOfBoard board = cellsOfBoard board (Text1) $ scale 0.25 0.25 sCell1

s2CellsOfBoard :: Board -> Picture
s2CellsOfBoard board = cellsOfBoard board (Text2) $ scale 0.25 0.25 sCell2

s3CellsOfBoard :: Board -> Picture
s3CellsOfBoard board = cellsOfBoard board (Text3) $ scale 0.25 0.25 sCell3

s4CellsOfBoard :: Board -> Picture
s4CellsOfBoard board = cellsOfBoard board (Text4) $ scale 0.25 0.25 sCell4

s5CellsOfBoard :: Board -> Picture
s5CellsOfBoard board = cellsOfBoard board (Text5) $ scale 0.25 0.25 sCell5

s6CellsOfBoard :: Board -> Picture
s6CellsOfBoard board = cellsOfBoard board (Text6) $ scale 0.25 0.25 sCell6

s7CellsOfBoard :: Board -> Picture
s7CellsOfBoard board = cellsOfBoard board (Text7) $ scale 0.25 0.25 sCell7

s8CellsOfBoard :: Board -> Picture
s8CellsOfBoard board = cellsOfBoard board (Text8) $ scale 0.25 0.25 sCell8


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
					 , s1CellsOfBoard board
					 , s2CellsOfBoard board
					 , s3CellsOfBoard board
					 , s4CellsOfBoard board
					 , s5CellsOfBoard board
					 , s6CellsOfBoard board
					 , s7CellsOfBoard board
					 , s8CellsOfBoard board
	         ]

boardAsGameOverPicture winner board = color (outcomeColor winner) (boardAsPicture board)

gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                                frame
     where frame = case gameState game of
     	            Running -> boardAsRunningPicture (gameBoard game)
     	            GameOver winner -> boardAsGameOverPicture winner (gameBoard game)
