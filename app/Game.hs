{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
module Game where
import Data.Array.IO
import Control.Monad
import Data.List.Split
import Data.Foldable
import Text.Printf
import Control.Lens hiding (Empty)

data Player = X | O | Empty deriving (Show, Eq, Ord)

data Turn = Winner Player | Tie | Turn Player deriving (Show)
data BoardState = BoardState 
  { _board :: IO (IOArray Int Player)
  , _turn :: Turn
  , _turnCount :: Int
  , _coordAction :: IO (Int, Int)
  , _printAction :: String -> IO ()
  }
makeLenses ''BoardState

defaultBoard = BoardState (newArray (0, 8) Empty) (Turn X) 0 ((\(x:y:_) -> (x, y)) . map read . words <$> getLine) putStr

displayPlayer X = "X"
displayPlayer O = "O"
displayPlayer Empty = " "

displayRow [x,y,z] = printf "%s | %s | %s\n" x y z
displayBoard :: BoardState -> IO ()
displayBoard BoardState{..} = do
  b <- _board
  s <- map (map displayPlayer).chunksOf 3<$>getElems b
  mapM_ (_printAction . displayRow) s 
  pure ()
makeTurn :: BoardState -> IO BoardState
makeTurn bs@(BoardState{..}) = do
  b <- _board
  let
    playerStr = displayPlayer $ curTurn
    getValidInput x = do
      x' <- (\(x,y) -> x * 3 + y) <$> x
      if not $ inRange (0, 8) x'
        then _printAction ("Input outside of grid.\nYou can do this, " <> playerStr <> ":") >> getValidInput x
        else do
          e <- readArray b x'
          if e /= Empty then _printAction ("Already Occupied.\nStill " <> playerStr <> "'s turn: ") >> getValidInput x else pure x'
    curTurn = fromTurn _turn
    nCount = succ _turnCount
  _printAction $ printf "%s' Turn: " playerStr
  accessr <- getValidInput _coordAction
  atPlace <- readArray b accessr
  writeArray b accessr curTurn
  let
    rows = map (\x -> x + div accessr 3 * 3) [0..2]
    cols = map (+ mod accessr 3) [0,3,6]
    diagl = [0,4,8]
    diagr = [2,4,6]
  pls <- mapM (traverse $ readArray b) [rows, cols, diagl, diagr]
  let
    z = map (foldl1 (\x y -> if x /= Empty && x == y then x else Empty)) pls
    newTurn = case filter (/= Empty) z of
      (x:_) -> Winner x
      _ -> if nCount >= 9 then Tie else Turn $ nextTurn curTurn
  pure $ bs { _board = pure b, _turn = newTurn, _turnCount = nCount}


main = gameLoop defaultBoard

gameLoop x@BoardState{_turn,..} = do
  displayBoard x
  s@BoardState{_turn} <- makeTurn x
  let
    (isOver, overMsg) = shouldDo _turn
  if isOver then displayBoard x >> _printAction overMsg else gameLoop s

shouldDo Tie = (True, "Tied")
shouldDo (Winner x) = (True, mconcat ["Congrats ", displayPlayer x, ": another win for democracy\n"])
shouldDo _ = (False, mempty)

fromTurn (Turn x) = x
fromTurn _ = Empty

nextTurn X = O
nextTurn O = X
nextTurn _ = undefined
