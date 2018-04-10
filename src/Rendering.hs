module Rendering where

import Data.Array

import Graphics.Gloss

import Game

boardGridColor = makeColorI 128 128 128 255
playerBColor = makeColorI 0 0 0 255
playerWColor = makeColorI 255 255 255 255

boardAsRunningPicture board = 
	pictures [ --color playerBColor $ bCellsOfBoard board
	         --, color playerWColor $ wCellsOfBoard board
	          color boardGridColor $ boardGrid
	         ]

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
	pictures [boardGrid]


gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                                frame
     where frame = case gameState game of
     	            Running -> boardAsRunningPicture (gameBoard game)