module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Game
import Rendering
import Logic

window :: Display
window = InWindow "Othello/Reversi" (screenWidth, screenHeight) (100, 100)

backgroundColor :: Color
backgroundColor = makeColor 0 102 15 255

drawing :: Picture
drawing = circle 80


main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (const id)