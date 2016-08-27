------------------------------------------------------------------------------------------
--                                                                                      --
--                    The 2048 Game using Blank-Canvas                                  -- 
--                                                                                      --
--                           Jyothi Prasad.P.S                                          -- 
--		                    University of kansas                                        --
--             Functional Programming & DSLs - Dr. Andrew Gill                          --
--                             December 2015                                            -- 
--                                                                                      --
------------------------------------------------------------------------------------------
--  References : Hackage Tutorial on 2048 Game.                                         --
--               Blank Canvas Syntaxes from Hackage.                                    --
------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Graphics.Blank
import Control.Monad.Random
import Control.Monad.Writer
import Data.Maybe
import Data.List
import           Data.Text (pack)

--Complete board [[Maybe Int]] is defined by below types
type Cell  = Maybe Int
type Row   = [Cell]
type Board = [Row]

data Direction = East | West | North | South deriving (Show, Eq)

--Used to represent the state of the board and the result of each move
data MoveResult = MoveResult Board

main :: IO ()
main = do
  --Creates a 4X4 board of mostly nothing and very less random cells with value 2 and 4
   boardLayout <- makeStartBoard 4

   --Using the view controller methodolody as in the Tic Tac Toe taught in the class
   blankCanvas 3000 { events = ["keydown"] } $ \ context -> do
        operations context boardLayout (Just 2048)

--The Direction Data Type will be mapped based on the keypress event
getdirection :: Event -> Maybe Direction       
getdirection event = case eWhich event of      -- eWhich is the constructor for the Event
      Just 37 -> Just West
      Just 38 -> Just North
      Just 39 -> Just East
      Just 40 -> Just South
      _ -> Nothing

--'operations' prints the board on the screen and also the state on the screen
operations :: DeviceContext -> Board -> Cell -> IO ()  
operations context board goal= do
  send context $ do
      font "20pt Calibri"
      fillStyle "Red"
      fillText("2048 Game in Haskell", 40, 40)
      let (w,h) = (width context, height context)
      clearRect (45,45,w,h)
      beginPath()

      let sz = min w h
      save()

      translate (w / 2, h / 2)
      sequence_ [ do bigLine (-sz * 0.5,n) (sz * 0.5,n)
                     bigLine (n,-sz * 0.5) (n,sz * 0.5)
                     bigLine (-335 , 335 ) ( 335 , 335)
                     bigLine (335 , -335 ) (335 , 335)
                     bigLine (-335 , -335 ) (-335 , 335)
                     bigLine (-335 , -335 ) (335 , -335)
                | n <- [-sz * 0.25,sz * 0.25,0]
                ]
      restore()

      save()

      translate (w / 4,0)
      sequence_ [ do save()
                     translate (fromIntegral x * sz * 0.25,fromIntegral y * sz * 0.25)
                     case getelement (x,y) board of
                      Just n -> drawNum n sz
                      Nothing -> return ()
                     restore()
                | x <- [0,1,2,3]
                , y <- [0,1,2,3]
                ]
      restore()

  event <- wait context
  let direction = getdirection event
  case direction of
    Nothing -> operations context board goal
    Just dir -> do
      MoveResult newBoard <- liftIO $ gameState goal dir board
      operations context newBoard goal


drawNum :: Int ->Double-> Canvas ()                     
drawNum num size = do
    font (pack ((show (40))++"pt Calibri"))
    textAlign "center"
    textBaseline "middle"
    lineWidth 5
    fillStyle "red"
    fillText((pack (show num)),size/8,size/8)

-- Displays the grid lines inspired from Tic-Tac toe problem explained in class
bigLine :: (Double, Double) -> (Double, Double) -> Canvas ()    
bigLine (x,y) (x',y') = do                                       
        beginPath()
        moveTo(x,y)
        lineTo(x',y')
        lineWidth 10
        strokeStyle "black"
        lineCap "round"
        stroke()

available :: Board -> [(Int, Int)]                     -- Retrieves the empty cells
available = concat . zipWith (zip . repeat) [0..] . fmap (elemIndices Nothing)

-- Updates the cell on the board with new value
update :: Board -> (Int, Int) -> Cell -> Board         
update board (x, y) val = newBoard
    where (rs, r:rs') = splitAt x board
          (cs, _:cs') = splitAt y r
          newRow = cs ++ (val : cs')
          newBoard = rs ++ (newRow : rs')

--Moves rows and adds numbers and accumulates them to respective direction
moveRow :: Row -> Row                                           
moveRow row = (++ nothings) $ sumPairs justs
    where (justs, nothings) = partition isJust row
          sumPairs (Just x:Just y:zs) | x == y =
            let total = x + y
		rest = sumPairs zs
            in Just total : rest ++ [Nothing]
          sumPairs (x:xs) = (x :) $ sumPairs xs
          sumPairs [] = []

performMove :: Direction -> Board -> Board
performMove direction = case direction of
    West  -> goLeft
    East  -> goRight
    North -> transpose . goLeft . transpose
    South -> transpose . goRight . transpose
    where goLeft = map moveRow
          goRight = map $ reverse . moveRow . reverse

-- gets the element present at (x,y) co-ordinates in the board
getelement :: (Int,Int) -> Board -> Cell                           
getelement (x,y) board = retrieve
  where (rs, r:rs') = splitAt y board
        (cs, target:cs') = splitAt x r
        retrieve = target

emptyBoard :: Int -> Board
emptyBoard n = replicate n $ replicate n Nothing

-- Makes an initial board with some random values
makeStartBoard :: MonadRandom m => Int -> m Board       
makeStartBoard size = do
    Just board  <- insertRandom (emptyBoard size)
    Just board' <- insertRandom board
    return board'

-- Inserts random values, 2 with probability 0.9 and 4 with probability 0.1	
insertRandom :: MonadRandom m => Board -> m (Maybe Board)          
insertRandom board
    | null holes = return Nothing
    | otherwise = do
        pos <- liftM (holes !!) $ getRandomR (0, length holes - 1)
        coin <- getRandomR (0 :: Float, 1)
        let newCell = Just $ if coin < 0.9 then 2 else 4
        return . Just $ update board pos newCell
    where holes = available board

result :: Cell -> Board -> Bool
result winning = elem winning . concat

-- It produces the Board state after each user action.
gameState :: MonadRandom m => Cell -> Direction -> Board -> m MoveResult     
gameState goal direction board =
    let newBoard = performMove direction board
        change = board /= newBoard
    in if not change
        then return $ if null $ available newBoard
            then MoveResult newBoard
        else MoveResult board
		else if result goal newBoard
            then return $ MoveResult newBoard
				else do
						randomBoard' <- insertRandom newBoard
						case randomBoard' of
							Nothing -> return $ MoveResult newBoard
							Just b  -> return $ MoveResult b
