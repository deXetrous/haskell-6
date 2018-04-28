module Logic where

import Data.Array

import Game
import Graphics.Gloss.Interface.Pure.Game

m :: Int
m = n-1

-- | This 'isCoordCorrect' function checks if the cell coord in the board range or not.
isCoordCorrect = inRange ((0, 0), (m - 1, m - 1))

-- | This 'isCoordAdjacent' function checks if the adjacent position contains discs of the opposite player.
isCoordAdjacent :: Game -> (Int, Int) -> Bool
isCoordAdjacent game (x, y) |    x+1 < m && board!(x+1, y) == (Full $ player)
                              || x-1 >=0 && board!(x-1, y) == (Full $ player)
                              || y-1 >=0 && board!(x, y-1) == (Full $ player)
                              || y+1 < m && board!(x, y+1) == (Full $ player)
                              || x+1 < m && y+1 < m && board!(x+1, y+1) == (Full $ player) 
                              || x-1 >=0 && y-1 >= 0 && board!(x-1, y-1) == (Full $ player)
                              || x-1 >=0 && y+1 < m && board!(x-1, y+1) == (Full $ player)
                              || x+1 < m && y-1 >= 0 && board!(x+1, y-1) == (Full $ player) 
                               = True
                            | otherwise = False
                            where board = gameBoard game
                                  player = 
                                  	if gamePlayer game == PlayerB
                                  		then PlayerW
                                  	else PlayerB

-- | This 'isendSymbolDown' function checks if on the down side we have a disc of same color
-- to form a straight line enclosing discs of opposite color to flank them.
isendSymbolDown :: Game -> (Int,Int) ->Bool
isendSymbolDown game (x,y) | x>=0 && (board!(x, y) == Empty) = False
                      | x >=0 && (board!(x, y) == (Full $ player) || isendSymbolDown game (x-1, y)) = True
                      | otherwise = False
                      where board = gameBoard game
                            player = gamePlayer game

-- This 'isDown' symbol returns true if we are able to flank the opponents discs in the down direction else false
isDown :: Game -> (Int, Int) -> Bool
isDown game (x,y) | x-1 >=0 && board!(x-1, y) == (Full $ player) && isendSymbolDown game (x-1,y) = True
                  | otherwise = False
                  where board = gameBoard game
                        player = 
                          if gamePlayer game == PlayerB
                              then PlayerW
                          else PlayerB


isendSymbolUp :: Game -> (Int,Int) ->Bool
isendSymbolUp game (x,y) | x<m && (board!(x, y) == Empty) = False
                      | x <m && (board!(x, y) == (Full $ player) || isendSymbolUp game (x+1, y)) = True
                      | otherwise = False
                      where board = gameBoard game
                            player = gamePlayer game

isUp :: Game -> (Int, Int) -> Bool
isUp game (x,y) | x+1 <m && board!(x+1, y) == (Full $ player) && isendSymbolUp game (x+1,y) = True
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
isendSymbolRight game (x,y) | y<m && (board!(x, y) == Empty) = False
                      | y <m && (board!(x, y) == (Full $ player) || isendSymbolRight game (x, y+1)) = True
                      | otherwise = False
                      where board = gameBoard game
                            player = gamePlayer game

isRight :: Game -> (Int, Int) -> Bool
isRight game (x,y) | y+1 <m && board!(x, y+1) == (Full $ player) && isendSymbolRight game (x,y+1) = True
                  | otherwise = False
                  where board = gameBoard game
                        player = 
                          if gamePlayer game == PlayerB
                              then PlayerW
                          else PlayerB

isendSymbolNW :: Game -> (Int,Int) ->Bool
isendSymbolNW game (x,y) | x<m && y>=0 && (board!(x, y) == Empty) = False
                      | x <m && y>=0 && (board!(x, y) == (Full $ player) || isendSymbolNW game (x+1, y-1)) = True
                      | otherwise = False
                      where board = gameBoard game
                            player = gamePlayer game

isNW :: Game -> (Int, Int) -> Bool
isNW game (x,y) | x+1 <m && y-1 >0 && board!(x+1, y-1) == (Full $ player) && isendSymbolNW game (x+1,y-1) = True
                  | otherwise = False
                  where board = gameBoard game
                        player = 
                          if gamePlayer game == PlayerB
                              then PlayerW
                          else PlayerB


isendSymbolNE :: Game -> (Int,Int) ->Bool
isendSymbolNE game (x,y) | x <m && y<m && (board!(x, y) == Empty) = False
                      | x <m && y<m && (board!(x, y) == (Full $ player) || isendSymbolNE game (x+1, y+1)) = True
                      | otherwise = False
                      where board = gameBoard game
                            player = gamePlayer game

isNE :: Game -> (Int, Int) -> Bool
isNE game (x,y) | x+1 <m && y+1 <m && board!(x+1, y+1) == (Full $ player) && isendSymbolNE game (x+1,y+1) = True
                  | otherwise = False
                  where board = gameBoard game
                        player = 
                          if gamePlayer game == PlayerB
                              then PlayerW
                          else PlayerB


isendSymbolSE :: Game -> (Int,Int) ->Bool
isendSymbolSE game (x,y) | x >=0 && y<m && (board!(x, y) == Empty) = False
                      | x >=0 && y<m && (board!(x, y) == (Full $ player) || isendSymbolSE game (x-1, y+1)) = True
                      | otherwise = False
                      where board = gameBoard game
                            player = gamePlayer game

isSE :: Game -> (Int, Int) -> Bool
isSE game (x,y) | x-1 >=0 && y+1 <m && board!(x-1, y+1) == (Full $ player) && isendSymbolSE game (x-1,y+1) = True
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

-- | This 'isgenUp' function returns a list of [(Int,Int)] which is a list of cellcoords to be flipped in the up direction
genUp :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
genUp ((x,y),(z,w)) | x<=z = (x,y):genUp ((x+1,y),(z,w))
                    | otherwise = []

-- | This 'upIndex' function returns (Int,Int) which is an extreme end of cellcoords to be flipped in the up direction
upIndex :: Game -> (Int,Int) -> (Int,Int)
upIndex game (x,y) | board!(x,y) == (Full $ player) = (x, y)
                   | otherwise = upIndex game (x+1, y) 
                   where player = gamePlayer game
                         board = gameBoard game

-- | This 'flipUp' function returns a list of [((Int,Int),Cell)] which is a list of cellcoords to be flipped
-- along with the player type for each cellcoord in the up direction.
flipUp :: Game -> (Int,Int) -> [((Int,Int),Cell)]
flipUp game (x,y) = zip (genUp indexRange) (repeat (Full $ player))
  where player = gamePlayer game
        indexRange = ((x,y),(upIndex game (x+1,y)))

-- | This 'checkflipUp' function returns a list of [((Int,Int),Cell)] which is a list of cellcoords to be flipped in the up direction
-- or [] if no flipping is possible in this direction.
checkflipUp :: Game -> (Int,Int) -> [((Int,Int),Cell)]
checkflipUp game cellCoord  | isUp game cellCoord = flipUp game cellCoord
                            | otherwise = []


genDown :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
genDown ((x,y),(z,w)) | x<=z = (x,y):genDown ((x+1,y),(z,w))
                      | otherwise = []

downIndex :: Game -> (Int,Int) -> (Int,Int)
downIndex game (x,y) | board!(x,y) == (Full $ player) = (x,y)
                     | otherwise = downIndex game (x-1,y)
                     where player = gamePlayer game
                           board = gameBoard game


flipDown :: Game -> (Int,Int) -> [((Int,Int),Cell)]
flipDown game (x,y) = zip (genDown indexRange) (repeat (Full $ player))
  where player = gamePlayer game
        indexRange = ((downIndex game (x-1,y)),(x,y))

checkflipDown :: Game -> (Int,Int) -> [((Int,Int),Cell)]
checkflipDown game cellCoord  | isDown game cellCoord = flipDown game cellCoord
                              | otherwise = []



genLeft :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
genLeft ((x,y),(z,w)) | y<=w = (x,y):genLeft ((x,y+1),(z,w))
                      | otherwise = []

leftIndex :: Game -> (Int,Int) -> (Int,Int)
leftIndex game (x,y) | board!(x,y) == (Full $ player) = (x, y)
                     | otherwise = leftIndex game (x, y-1) 
                   where player = gamePlayer game
                         board = gameBoard game

flipLeft :: Game -> (Int,Int) -> [((Int,Int),Cell)]
flipLeft game (x,y) = zip (genLeft indexRange) (repeat (Full $ player))
  where player = gamePlayer game
        indexRange = ((leftIndex game (x,y-1)), (x,y))

checkflipLeft :: Game -> (Int,Int) -> [((Int,Int),Cell)]
checkflipLeft game cellCoord  | isLeft game cellCoord = flipLeft game cellCoord
                              | otherwise = []


genRight :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
genRight ((x,y),(z,w)) | y<=w = (x,y):genRight ((x,y+1),(z,w))
                       | otherwise = []

rightIndex :: Game -> (Int,Int) -> (Int,Int)
rightIndex game (x,y) | board!(x,y) == (Full $ player) = (x, y)
                      | otherwise = rightIndex game (x, y+1) 
                   where player = gamePlayer game
                         board = gameBoard game

flipRight :: Game -> (Int,Int) -> [((Int,Int),Cell)]
flipRight game (x,y) = zip (genRight indexRange) (repeat (Full $ player))
  where player = gamePlayer game
        indexRange = ((x,y),(rightIndex game (x,y+1)))

checkflipRight :: Game -> (Int,Int) -> [((Int,Int),Cell)]
checkflipRight game cellCoord  | isRight game cellCoord = flipRight game cellCoord
                               | otherwise = []

genNW :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
genNW ((x,y),(z,w)) | x<=z && y>=w = (x,y):genNW ((x+1,y-1),(z,w))
                    | otherwise = []

nwIndex :: Game -> (Int,Int) -> (Int,Int)
nwIndex game (x,y) | board!(x,y) == (Full $ player) = (x, y)
                   | otherwise = nwIndex game (x+1, y-1) 
                   where player = gamePlayer game
                         board = gameBoard game


flipNW :: Game -> (Int,Int) -> [((Int,Int),Cell)]
flipNW game (x,y) = zip (genNW indexRange) (repeat (Full $ player))
  where player = gamePlayer game
        indexRange = ((x,y),(nwIndex game (x+1,y-1)))

checkflipNW :: Game -> (Int,Int) -> [((Int,Int),Cell)]
checkflipNW game cellCoord  | isNW game cellCoord = flipNW game cellCoord
                            | otherwise = []


genNE :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
genNE ((x,y),(z,w)) | x<=z && y<=w = (x,y):genNE ((x+1,y+1),(z,w))
                    | otherwise = []

neIndex :: Game -> (Int,Int) -> (Int,Int)
neIndex game (x,y) | board!(x,y) == (Full $ player) = (x, y)
                   | otherwise = neIndex game (x+1, y+1) 
                   where player = gamePlayer game
                         board = gameBoard game

flipNE :: Game -> (Int,Int) -> [((Int,Int),Cell)]
flipNE game (x,y) = zip (genNE indexRange) (repeat (Full $ player))
  where player = gamePlayer game
        indexRange = ((x,y),(neIndex game (x+1,y+1)))

checkflipNE :: Game -> (Int,Int) -> [((Int,Int),Cell)]
checkflipNE game cellCoord  | isNE game cellCoord = flipNE game cellCoord
                            | otherwise = []

genSW :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
genSW ((x,y),(z,w)) | x<=z && y<=w = (x,y):genSW ((x+1,y+1),(z,w))
                    | otherwise = []

swIndex :: Game -> (Int,Int) -> (Int,Int)
swIndex game (x,y) | board!(x,y) == (Full $ player) = (x, y)
                   | otherwise = swIndex game (x-1, y-1) 
                   where player = gamePlayer game
                         board = gameBoard game

flipSW :: Game -> (Int,Int) -> [((Int,Int),Cell)]
flipSW game (x,y) = zip (genSW indexRange) (repeat (Full $ player))
  where player = gamePlayer game
        indexRange = ((swIndex game (x-1,y-1)), (x,y))

checkflipSW :: Game -> (Int,Int) -> [((Int,Int),Cell)]
checkflipSW game cellCoord  | isSW game cellCoord = flipSW game cellCoord
                            | otherwise = []

genSE :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
genSE ((x,y),(z,w)) | x<=z && y>=w = (x,y):genSE ((x+1,y-1),(z,w))
                    | otherwise = []

seIndex :: Game -> (Int,Int) -> (Int,Int)
seIndex game (x,y) | board!(x,y) == (Full $ player) = (x, y)
                   | otherwise = seIndex game (x-1, y+1) 
                   where player = gamePlayer game
                         board = gameBoard game

flipSE :: Game -> (Int,Int) -> [((Int,Int),Cell)]
flipSE game (x,y) = zip (genSE indexRange) (repeat (Full $ player))
  where player = gamePlayer game
        indexRange = ((seIndex game (x-1,y+1)), (x,y))

checkflipSE :: Game -> (Int,Int) -> [((Int,Int),Cell)]
checkflipSE game cellCoord  | isSE game cellCoord = flipSE game cellCoord
                            | otherwise = []


-- | This 'switchPlayer' function returns a game by taking input a game and switching the game Player
switchPlayer :: Game->Game
switchPlayer game = 
	case gamePlayer game of
		PlayerB -> game { gamePlayer = PlayerW}
		PlayerW -> game { gamePlayer = PlayerB}


-- | This 'calculate' function takes a game, cell and a list and returns the frequency of the cell in the game Board
calculate :: Game-> Cell -> [(Int,Int)] -> Int
calculate game tempCell [] = 0
calculate game tempCell ((x,y):xs) | board!(x,y) == tempCell = 1 + calculate game tempCell xs
                                   | otherwise = calculate game tempCell xs
                                   where board = gameBoard game

  
-- | This 'checkGameOver' function take a game and checks if the game is over and returns a new game with gamestate equal to gameOver
checkGameOver :: Game->Game
checkGameOver game
    | calculate game Empty (range indexRange) == 0 && playerBdiscs > playerWdiscs = game { gameState = GameOver (Just PlayerB)}
    | calculate game Empty (range indexRange) == 0 && playerWdiscs > playerBdiscs = game { gameState = GameOver (Just PlayerW)}
    | calculate game Empty (range indexRange) == 0 && playerWdiscs == playerBdiscs = game { gameState = GameOver Nothing}
    | condition1 == False && condition2 == False && playerBdiscs > playerWdiscs = game { gameState = GameOver (Just PlayerB)} 
    | condition1 == False && condition2 == False && playerBdiscs < playerWdiscs = game { gameState = GameOver (Just PlayerW)} 
    | condition1 == False && condition2 == False && playerBdiscs == playerWdiscs = game { gameState = GameOver Nothing} 
    | otherwise = game
    where indexRange = ((0,0),(m-1,m-1))
          playerBdiscs = calculate game (Full PlayerB) (range indexRange)
          playerWdiscs = calculate game (Full PlayerW) (range indexRange)
          condition1 = isValidMovesLeft game
          condition2 = isValidMovesLeft $ switchPlayer game


-- | This 'checkValidMoves' function takes a Game and a cell coord and checks if it is a valid move to place a disc at that cell coord.
checkValidMoves :: Game -> [(Int,Int)] -> Bool
checkValidMoves game [] = False
checkValidMoves game ((x,y):xs) 
                | (isCoordCorrect (x,y)) && (isCoordAdjacent game (x,y)) && (isDown game (x,y) || isUp game (x,y) || isLeft game (x,y) || isRight game (x,y)|| isNW game (x,y)|| isSE game (x,y)|| isSW game (x,y) || isNE game (x,y)) && board ! (x,y) == Empty = True
                | otherwise = checkValidMoves game xs
                 where board = gameBoard game

-- | This 'isValidMovesLeft' function takes a Game and returns a bool value if any possible move is available or not.
isValidMovesLeft :: Game -> Bool
isValidMovesLeft game
    | checkValidMoves game (range indexRange) = True
    | otherwise = False
    where indexRange = ((0,0),(m-1,m-1))

-- | This 'checkValidPosition' checks whether it is possible to place on cell coord (Int,Int)
checkValidPositon :: Game -> (Int,Int) -> Bool
checkValidPositon game (x,y)
                | (isCoordCorrect (x,y)) && (isCoordAdjacent game (x,y)) && (isDown game (x,y) || isUp game (x,y) || isLeft game (x,y) || isRight game (x,y)|| isNW game (x,y)|| isSE game (x,y)|| isSW game (x,y) || isNE game (x,y)) && board ! (x,y) == Empty = True
                | otherwise = False
                where board = gameBoard game

-- | Thiss 'setPoints' returns a int which is no of flips of opposite player which would occur on placing on the cellCoord input
setPoints :: Game -> (Int,Int) -> Int
setPoints game cellCoord | (isCoordAdjacent game cellCoord) && (isDown game cellCoord || isUp game cellCoord || isLeft game cellCoord || isRight game cellCoord|| isNW game cellCoord|| isSE game cellCoord|| isSW game cellCoord || isNE game cellCoord) && board ! cellCoord == Empty =
                       length(checkflipUp game cellCoord ++ checkflipDown game cellCoord ++ checkflipLeft game cellCoord ++ checkflipRight game cellCoord
                                     ++checkflipNW game cellCoord ++ checkflipNE game cellCoord ++ checkflipSW game cellCoord ++ checkflipSE game cellCoord)
                     | otherwise = 0
                     where board = gameBoard game

-- | This 'findIndex' function returns a cellCoords which corresponds to the index of last input in the input list
findIndex :: Int-> [Int]->Int->(Int,Int)
findIndex i [] ele = (-1,-1)
findIndex i (x:xs) ele | x == ele = (i `div` 8, i `mod` 8)
                       | otherwise = findIndex (i+1) xs ele

-- | This 'findMaximum' function returns a cell coord corresponding to the maximum value int the input list
findMaximum ::[Int] -> (Int,Int)
findMaximum mylist = findIndex 0 mylist $ maximum mylist 

-- | This 'getPos' function returns the cellcoord for the motion of bot.
getPos :: Game -> (Int,Int)
getPos game | snd (findMaximum (map (setPoints game) $ (range indexRange))) /= -1 = findMaximum (map (setPoints game) $ (range indexRange))
            | otherwise = (0,0)
            where indexRange = ((0,0),(m-1,m-1))

-- | This function return the position of bot motion.
botMove :: Game -> (Int,Int)
botMove game | checkValidPositon game (0,0)==True = (0,0)
             | checkValidPositon game (0,7)==True = (0,7)
             | checkValidPositon game (7,0)==True = (7,0) 
             | checkValidPositon game (7,7)==True = (7,7)
             | botCoordinates /= (0,0) = botCoordinates
             where botCoordinates = getPos game
             
-- | This 'botMotion' function takes a game applies the bot motion to it and returns the next state of game.
botMotion:: Game -> Game
botMotion game = game {gameBoard = board // (checkflipUp game (botMove game) ++ checkflipDown game (botMove game) ++ checkflipLeft game (botMove game) ++ checkflipRight game (botMove game)
                                     ++checkflipNW game (botMove game) ++ checkflipNE game (botMove game) ++ checkflipSW game (botMove game) ++ checkflipSE game (botMove game))}
                where board = gameBoard game

-- | This function checks for valid input by user, places its disc, flipps opponent discs and switches player
-- | This function also calls for botMotion if the player is PlayerB. 
playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
    | isValidMovesLeft game == False = switchPlayer game
    | (isCoordCorrect cellCoord) && (isCoordAdjacent game cellCoord) && player==PlayerB && (isDown game cellCoord || isUp game cellCoord || isLeft game cellCoord || isRight game cellCoord|| isNW game cellCoord|| isSE game cellCoord|| isSW game cellCoord || isNE game cellCoord) && board ! cellCoord == Empty =
       checkGameOver
     
      $ switchPlayer 
    	$ game { gameBoard = board // (checkflipUp game cellCoord ++ checkflipDown game cellCoord ++ checkflipLeft game cellCoord ++ checkflipRight game cellCoord
                                    ++checkflipNW game cellCoord ++ checkflipNE game cellCoord ++ checkflipSW game cellCoord ++ checkflipSE game cellCoord)}
    
     | player==PlayerW =
       checkGameOver
      $ switchPlayer
      $ botMotion game
    | otherwise = game

    where board = gameBoard game
          player = gamePlayer game


-- | This 'mousePosAsCellCoord' function takes the click position of user, maps it to a cell index and retuns the coordinates.
mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
	                         , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
	                         )
	
-- | This function transforms the state of the game according to the events done by the user.
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game = 
	   case gameState game of
		    Running -> playerTurn game $ mousePosAsCellCoord mousePos

transformGame _ game = game