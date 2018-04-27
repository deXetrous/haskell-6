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

isendSymbolDown :: Game -> (Int,Int) ->Bool
isendSymbolDown game (x,y) | x>=0 && (board!(x, y) == Empty) = False
                      | x >=0 && (board!(x, y) == (Full $ player) || isendSymbolDown game (x-1, y)) = True
                      | otherwise = False
                      where board = gameBoard game
                            player = gamePlayer game

isDown :: Game -> (Int, Int) -> Bool
isDown game (x,y) | x-1 >=0 && board!(x-1, y) == (Full $ player) && isendSymbolDown game (x-1,y) = True
                  | otherwise = False
                  where board = gameBoard game
                        player = 
                          if gamePlayer game == PlayerB
                              then PlayerW
                          else PlayerB


isendSymbolUp :: Game -> (Int,Int) ->Bool
isendSymbolUp game (x,y) | x<n && (board!(x, y) == Empty) = False
                      | x <n && (board!(x, y) == (Full $ player) || isendSymbolUp game (x+1, y)) = True
                      | otherwise = False
                      where board = gameBoard game
                            player = gamePlayer game

isUp :: Game -> (Int, Int) -> Bool
isUp game (x,y) | x+1 <n && board!(x+1, y) == (Full $ player) && isendSymbolUp game (x+1,y) = True
                  | otherwise = False
                  where board = gameBoard game
                        player = 
                          if gamePlayer game == PlayerB
                              then PlayerW
                          else PlayerB

isendSymbolLeft :: Game -> (Int,Int) ->Bool
isendSymbolLeft game (x,y) | y >= 0 && (board!(x, y) == Empty) = False
                      | y >= 0 && (board!(x, y) == (Full $ player) || isendSymbolLeft game (x, y-1)) = True
                      | otherwise = False
                      where board = gameBoard game
                            player = gamePlayer game

isLeft :: Game -> (Int, Int) -> Bool
isLeft game (x,y) | y-1 >=0 && board!(x, y-1) == (Full $ player) && isendSymbolLeft game (x,y-1) = True
                  | otherwise = False
                  where board = gameBoard game
                        player = 
                          if gamePlayer game == PlayerB
                              then PlayerW
                          else PlayerB

isendSymbolRight :: Game -> (Int,Int) ->Bool
isendSymbolRight game (x,y) | y<n && (board!(x, y) == Empty) = False
                      | y <n && (board!(x, y) == (Full $ player) || isendSymbolRight game (x, y+1)) = True
                      | otherwise = False
                      where board = gameBoard game
                            player = gamePlayer game

isRight :: Game -> (Int, Int) -> Bool
isRight game (x,y) | y+1 <n && board!(x, y+1) == (Full $ player) && isendSymbolRight game (x,y+1) = True
                  | otherwise = False
                  where board = gameBoard game
                        player = 
                          if gamePlayer game == PlayerB
                              then PlayerW
                          else PlayerB

isendSymbolNW :: Game -> (Int,Int) ->Bool
isendSymbolNW game (x,y) | x<n && y>=0 && (board!(x, y) == Empty) = False
                      | x <n && y>=0 && (board!(x, y) == (Full $ player) || isendSymbolNW game (x+1, y-1)) = True
                      | otherwise = False
                      where board = gameBoard game
                            player = gamePlayer game

isNW :: Game -> (Int, Int) -> Bool
isNW game (x,y) | x+1 <n && y-1 >0 && board!(x+1, y-1) == (Full $ player) && isendSymbolNW game (x+1,y-1) = True
                  | otherwise = False
                  where board = gameBoard game
                        player = 
                          if gamePlayer game == PlayerB
                              then PlayerW
                          else PlayerB


isendSymbolNE :: Game -> (Int,Int) ->Bool
isendSymbolNE game (x,y) | x <n && y<n && (board!(x, y) == Empty) = False
                      | x <n && y<n && (board!(x, y) == (Full $ player) || isendSymbolNE game (x+1, y+1)) = True
                      | otherwise = False
                      where board = gameBoard game
                            player = gamePlayer game

isNE :: Game -> (Int, Int) -> Bool
isNE game (x,y) | x+1 <n && y+1 <n && board!(x+1, y+1) == (Full $ player) && isendSymbolNE game (x+1,y+1) = True
                  | otherwise = False
                  where board = gameBoard game
                        player = 
                          if gamePlayer game == PlayerB
                              then PlayerW
                          else PlayerB


isendSymbolSE :: Game -> (Int,Int) ->Bool
isendSymbolSE game (x,y) | x >=0 && y<n && (board!(x, y) == Empty) = False
                      | x >=0 && y<n && (board!(x, y) == (Full $ player) || isendSymbolSE game (x-1, y+1)) = True
                      | otherwise = False
                      where board = gameBoard game
                            player = gamePlayer game

isSE :: Game -> (Int, Int) -> Bool
isSE game (x,y) | x-1 >=0 && y+1 <n && board!(x-1, y+1) == (Full $ player) && isendSymbolSE game (x-1,y+1) = True
                  | otherwise = False
                  where board = gameBoard game
                        player = 
                          if gamePlayer game == PlayerB
                              then PlayerW
                          else PlayerB


isendSymbolSW :: Game -> (Int,Int) ->Bool
isendSymbolSW game (x,y) | x >=0 && y>=0 && (board!(x, y) == Empty) = False
                      | x >=0 && y>=0 && (board!(x, y) == (Full $ player) || isendSymbolSW game (x-1, y-1)) = True
                      | otherwise = False
                      where board = gameBoard game
                            player = gamePlayer game

isSW :: Game -> (Int, Int) -> Bool
isSW game (x,y) | x-1 >=0 && y-1 >=0 && board!(x-1, y-1) == (Full $ player) && isendSymbolSW game (x-1,y-1) = True
                  | otherwise = False
                  where board = gameBoard game
                        player = 
                          if gamePlayer game == PlayerB
                              then PlayerW
                          else PlayerB


genUp ((x,y),(z,w)) | x<=z = (x,y):genUp ((x+1,y),(z,w))
                    | otherwise = []


upIndex game (x,y) | board!(x,y) == (Full $ player) = (x, y)
                   | otherwise = upIndex game (x+1, y) 
                   where player = gamePlayer game
                         board = gameBoard game

flipUp game (x,y) = zip (genUp indexRange) (repeat (Full $ player))
  where player = gamePlayer game
        indexRange = ((x,y),(upIndex game (x+1,y)))

checkflipUp game cellCoord  | isUp game cellCoord = flipUp game cellCoord
                            | otherwise = []


genDown ((x,y),(z,w)) | x<=z = (x,y):genDown ((x+1,y),(z,w))
                      | otherwise = []


downIndex game (x,y) | board!(x,y) == (Full $ player) = (x,y)
                     | otherwise = downIndex game (x-1,y)
                     where player = gamePlayer game
                           board = gameBoard game

flipDown game (x,y) = zip (genDown indexRange) (repeat (Full $ player))
  where player = gamePlayer game
        indexRange = ((downIndex game (x-1,y)),(x,y))


checkflipDown game cellCoord  | isDown game cellCoord = flipDown game cellCoord
                              | otherwise = []




genLeft ((x,y),(z,w)) | y<=w = (x,y):genLeft ((x,y+1),(z,w))
                      | otherwise = []

leftIndex game (x,y) | board!(x,y) == (Full $ player) = (x, y)
                     | otherwise = leftIndex game (x, y-1) 
                   where player = gamePlayer game
                         board = gameBoard game

flipLeft game (x,y) = zip (genLeft indexRange) (repeat (Full $ player))
  where player = gamePlayer game
        indexRange = ((leftIndex game (x,y-1)), (x,y))

checkflipLeft game cellCoord  | isLeft game cellCoord = flipLeft game cellCoord
                              | otherwise = []



genRight ((x,y),(z,w)) | y<=w = (x,y):genRight ((x,y+1),(z,w))
                       | otherwise = []


rightIndex game (x,y) | board!(x,y) == (Full $ player) = (x, y)
                      | otherwise = rightIndex game (x, y+1) 
                   where player = gamePlayer game
                         board = gameBoard game

flipRight game (x,y) = zip (genRight indexRange) (repeat (Full $ player))
  where player = gamePlayer game
        indexRange = ((x,y),(rightIndex game (x,y+1)))

checkflipRight game cellCoord  | isRight game cellCoord = flipRight game cellCoord
                               | otherwise = []


genNW ((x,y),(z,w)) | x<=z && y>=w = (x,y):genNW ((x+1,y-1),(z,w))
                    | otherwise = []


nwIndex game (x,y) | board!(x,y) == (Full $ player) = (x, y)
                   | otherwise = nwIndex game (x+1, y-1) 
                   where player = gamePlayer game
                         board = gameBoard game

flipNW game (x,y) = zip (genNW indexRange) (repeat (Full $ player))
  where player = gamePlayer game
        indexRange = ((x,y),(nwIndex game (x+1,y-1)))

checkflipNW game cellCoord  | isNW game cellCoord = flipNW game cellCoord
                            | otherwise = []



genNE ((x,y),(z,w)) | x<=z && y<=w = (x,y):genNE ((x+1,y+1),(z,w))
                    | otherwise = []

neIndex game (x,y) | board!(x,y) == (Full $ player) = (x, y)
                   | otherwise = neIndex game (x+1, y+1) 
                   where player = gamePlayer game
                         board = gameBoard game

flipNE game (x,y) = zip (genNE indexRange) (repeat (Full $ player))
  where player = gamePlayer game
        indexRange = ((x,y),(neIndex game (x+1,y+1)))

checkflipNE game cellCoord  | isNE game cellCoord = flipNE game cellCoord
                            | otherwise = []


genSW ((x,y),(z,w)) | x<=z && y<=w = (x,y):genSW ((x+1,y+1),(z,w))
                    | otherwise = []

swIndex game (x,y) | board!(x,y) == (Full $ player) = (x, y)
                   | otherwise = swIndex game (x-1, y-1) 
                   where player = gamePlayer game
                         board = gameBoard game

flipSW game (x,y) = zip (genSW indexRange) (repeat (Full $ player))
  where player = gamePlayer game
        indexRange = ((swIndex game (x-1,y-1)), (x,y))

checkflipSW game cellCoord  | isSW game cellCoord = flipSW game cellCoord
                            | otherwise = []


genSE ((x,y),(z,w)) | x<=z && y>=w = (x,y):genSE ((x+1,y-1),(z,w))
                    | otherwise = []

seIndex game (x,y) | board!(x,y) == (Full $ player) = (x, y)
                   | otherwise = seIndex game (x-1, y+1) 
                   where player = gamePlayer game
                         board = gameBoard game

flipSE game (x,y) = zip (genSE indexRange) (repeat (Full $ player))
  where player = gamePlayer game
        indexRange = ((seIndex game (x-1,y+1)), (x,y))

checkflipSE game cellCoord  | isSE game cellCoord = flipSE game cellCoord
                            | otherwise = []



switchPlayer :: Game->Game
switchPlayer game = 
	case gamePlayer game of
		PlayerB -> game { gamePlayer = PlayerW}
		PlayerW -> game { gamePlayer = PlayerB}



calculate :: Game-> Cell -> [(Int,Int)] -> Int
calculate game tempCell [] = 0
calculate game tempCell ((x,y):xs) | board!(x,y) == tempCell = 1 + calculate game tempCell xs
                                   | otherwise = calculate game tempCell xs
                                   where board = gameBoard game

  

checkGameOver :: Game->Game
checkGameOver game
    | calculate game Empty (range indexRange) == 0 && calculate game (Full PlayerB) (range indexRange) > calculate game (Full PlayerW) (range indexRange) = game { gameState = GameOver (Just PlayerB)}
    | calculate game Empty (range indexRange) == 0 && calculate game (Full PlayerW) (range indexRange) > calculate game (Full PlayerB) (range indexRange) = game { gameState = GameOver (Just PlayerW)}
    | calculate game Empty (range indexRange) == 0 && calculate game (Full PlayerW) (range indexRange) == calculate game (Full PlayerB) (range indexRange) = game { gameState = GameOver Nothing}
    | otherwise = game
    where indexRange = ((0,0),(n-1,n-1))


checkValidMoves :: Game -> [(Int,Int)] -> Bool
checkValidMoves game [] = False
checkValidMoves game ((x,y):xs) 
                | (isCoordCorrect (x,y)) && (isCoordAdjacent game (x,y)) && (isDown game (x,y) || isUp game (x,y) || isLeft game (x,y) || isRight game (x,y)|| isNW game (x,y)|| isSE game (x,y)|| isSW game (x,y) || isNE game (x,y)) && board ! (x,y) == Empty = True
                | otherwise = checkValidMoves game xs
                 where board = gameBoard game

isValidMovesLeft :: Game -> Bool
isValidMovesLeft game
    | checkValidMoves game (range indexRange) = True
    | otherwise = False
    where indexRange = ((0,0),(n-1,n-1))


checkValidPositon :: Game -> (Int,Int) -> Bool
checkValidPositon game (x,y)
                | (isCoordCorrect (x,y)) && (isCoordAdjacent game (x,y)) && (isDown game (x,y) || isUp game (x,y) || isLeft game (x,y) || isRight game (x,y)|| isNW game (x,y)|| isSE game (x,y)|| isSW game (x,y) || isNE game (x,y)) && board ! (x,y) == Empty = True
                | otherwise = False
                where board = gameBoard game

setPoints :: Game -> (Int,Int) -> Int
setPoints game cellCoord | (isCoordAdjacent game cellCoord) && (isDown game cellCoord || isUp game cellCoord || isLeft game cellCoord || isRight game cellCoord|| isNW game cellCoord|| isSE game cellCoord|| isSW game cellCoord || isNE game cellCoord) && board ! cellCoord == Empty =
                       length(checkflipUp game cellCoord ++ checkflipDown game cellCoord ++ checkflipLeft game cellCoord ++ checkflipRight game cellCoord
                                     ++checkflipNW game cellCoord ++ checkflipNE game cellCoord ++ checkflipSW game cellCoord ++ checkflipSE game cellCoord)
                     | otherwise = 0
                     where board = gameBoard game

findIndex :: Int-> [Int]->Int->(Int,Int)
findIndex i [] ele = (-1,-1)
findIndex i (x:xs) ele | x == ele = (i `div` 8, i `mod` 8)
                       | otherwise = findIndex (i+1) xs ele

findMaximum ::[Int] -> (Int,Int)
findMaximum mylist = findIndex 0 mylist $ maximum mylist 

getPos :: Game -> (Int,Int)
getPos game | snd (findMaximum (map (setPoints game) $ (range indexRange))) /= -1 = findMaximum (map (setPoints game) $ (range indexRange))
            | otherwise = (0,0)
            where indexRange = ((0,0),(n-1,n-1))


botMove :: Game -> (Int,Int)
botMove game | checkValidPositon game (0,0)==True = (0,0)
             | checkValidPositon game (0,7)==True = (0,7)
             | checkValidPositon game (7,0)==True = (7,0) 
             | checkValidPositon game (7,7)==True = (7,7)
             | getPos game /= (-1,-1) = (getPos game)
             

botMotion:: Game -> Game
botMotion game = game {gameBoard = board // (checkflipUp game (botMove game) ++ checkflipDown game (botMove game) ++ checkflipLeft game (botMove game) ++ checkflipRight game (botMove game)
                                     ++checkflipNW game (botMove game) ++ checkflipNE game (botMove game) ++ checkflipSW game (botMove game) ++ checkflipSE game (botMove game))}
                where board = gameBoard game



playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
    | isValidMovesLeft game == False = switchPlayer game
    | (isCoordCorrect cellCoord) && (isCoordAdjacent game cellCoord) && (isDown game cellCoord || isUp game cellCoord || isLeft game cellCoord || isRight game cellCoord|| isNW game cellCoord|| isSE game cellCoord|| isSW game cellCoord || isNE game cellCoord) && board ! cellCoord == Empty =
       checkGameOver
      $ switchPlayer
      $ botMotion
      $ switchPlayer 
    	$ game { gameBoard = board // (checkflipUp game cellCoord ++ checkflipDown game cellCoord ++ checkflipLeft game cellCoord ++ checkflipRight game cellCoord
                                     ++checkflipNW game cellCoord ++ checkflipNE game cellCoord ++ checkflipSW game cellCoord ++ checkflipSE game cellCoord)}
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

transformGame _ game = game