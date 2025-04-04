import System.Random (randomRIO)
import System.IO (hFlush, stdout)

-- Data type representing each cell in the grid
data Cell = Cell
  { isBomb  :: Bool    -- Whether the cell has a bomb
  , revealed :: Bool   -- Has the cell been revealed
  , flagged  :: Bool   -- Is the cell flagged
  , count    :: Int    -- Number of adjacent bombs
  } deriving (Eq)

-- Custom display for a cell:
-- - Hidden cells are shown as "." (or "F" if flagged)
-- - Revealed bombs are shown as "*"
-- - Revealed empty cells show a blank (" ") or the number of adjacent bombs
instance Show Cell where
  show cell
    | not (revealed cell) = if flagged cell then "F" else "."
    | isBomb cell         = "*"
    | count cell == 0     = " "
    | otherwise           = show (count cell)

type Board = [[Cell]]

-- Board parameters (change these to customize grid size and bomb count)
boardRows, boardCols, bombCount :: Int
boardRows = 10
boardCols = 10
bombCount = 10

main :: IO ()
main = do
  putStrLn "Welcome to Minesweeper in Haskell!"
  board <- initBoard boardRows boardCols bombCount
  gameLoop board

-- Initialize the board: randomly place bombs and compute neighbor counts
initBoard :: Int -> Int -> Int -> IO Board
initBoard rows cols bombs = do
  bombPositions <- getBombPositions rows cols bombs
  let board0 = [ [ Cell { isBomb = (r, c) `elem` bombPositions
                         , revealed = False
                         , flagged = False
                         , count = 0 }
                 | c <- [0 .. cols - 1] ]
               | r <- [0 .. rows - 1] ]
  let board1 = computeCounts board0
  return board1

-- Randomly select unique positions for bombs
getBombPositions :: Int -> Int -> Int -> IO [(Int, Int)]
getBombPositions rows cols bombs = go bombs []
  where
    go 0 acc = return acc
    go n acc = do
      r <- randomRIO (0, rows - 1)
      c <- randomRIO (0, cols - 1)
      let pos = (r, c)
      if pos `elem` acc
        then go n acc
        else go (n - 1) (pos : acc)

-- Compute and update the bomb count for each cell based on neighboring bombs
computeCounts :: Board -> Board
computeCounts board =
  [ [ cell { count = cellCount (r, c) currentBoard }
    | (c, cell) <- zip [0 ..] row ]
  | (r, row) <- zip [0 ..] currentBoard ]
  where
    currentBoard = board
    cellCount (r, c) b =
      length [ () | (nr, nc) <- getNeighbors r c
                  , inBounds (nr, nc)
                  , isBomb (b !! nr !! nc) ]
    inBounds (r, c) =
      r >= 0 && r < length board && c >= 0 && c < length (head board)
    getNeighbors row col =
      [ (row + dr, col + dc) | dr <- [-1 .. 1]
                             , dc <- [-1 .. 1]
                             , (dr, dc) /= (0, 0) ]

-- Main game loop: print board, accept commands, update game state
gameLoop :: Board -> IO ()
gameLoop board = do
  printBoard board
  if checkWin board then
    putStrLn "Congratulations! You win!"
  else do
    putStr "Enter command (r row col to reveal, f row col to flag/unflag): "
    hFlush stdout
    command <- getLine
    let ws = words command
    case ws of
      ("r":rStr:cStr:_) -> do
        let row = read rStr :: Int
            col = read cStr :: Int
        if not (validCoord board (row, col)) then do
          putStrLn "Invalid coordinates!"
          gameLoop board
        else do
          let cell = (board !! row) !! col
          if flagged cell then do
            putStrLn "Cell is flagged, unflag it before revealing."
            gameLoop board
          else if isBomb cell then do
            putStrLn "Boom! You hit a bomb. Game Over."
            printBoard (revealAll board)
          else do
            let board' = revealCell board (row, col)
            gameLoop board'
      ("f":rStr:cStr:_) -> do
        let row = read rStr :: Int
            col = read cStr :: Int
        if not (validCoord board (row, col)) then do
          putStrLn "Invalid coordinates!"
          gameLoop board
        else do
          let board' = toggleFlag board (row, col)
          gameLoop board'
      _ -> do
        putStrLn "Invalid command."
        gameLoop board

-- Check if coordinates are within board boundaries
validCoord :: Board -> (Int, Int) -> Bool
validCoord board (r, c) =
  r >= 0 && r < length board && c >= 0 && c < length (head board)

-- Reveal all cells (used when game over)
revealAll :: Board -> Board
revealAll board =
  [ [ cell { revealed = True } | cell <- row ] | row <- board ]

-- Reveal a cell; if its count is 0, reveal adjacent cells recursively
revealCell :: Board -> (Int, Int) -> Board
revealCell board pos@(row, col)
  | not (validCoord board pos) = board
  | otherwise =
      let cell = (board !! row) !! col
      in if revealed cell then board else
           let board' = updateBoard board pos (cell { revealed = True })
           in if count cell == 0
                 then foldl revealCell board' (getNeighbors row col)
                 else board'
  where
    getNeighbors r c =
      [ (nr, nc) | nr <- [r - 1 .. r + 1]
                 , nc <- [c - 1 .. c + 1]
                 , (nr, nc) /= (r, c)
                 , validCoord board (nr, nc) ]

-- Toggle the flag on a cell
toggleFlag :: Board -> (Int, Int) -> Board
toggleFlag board pos@(r, c)
  | not (validCoord board pos) = board
  | otherwise =
      let cell = (board !! r) !! c
      in updateBoard board pos (cell { flagged = not (flagged cell) })

-- Utility: update a cell at a given coordinate in the board
updateBoard :: Board -> (Int, Int) -> Cell -> Board
updateBoard board (r, c) newCell =
  take r board ++
  [ take c (board !! r) ++ [newCell] ++ drop (c + 1) (board !! r) ] ++
  drop (r + 1) board

-- Check win condition: all non-bomb cells must be revealed
checkWin :: Board -> Bool
checkWin board = all rowCleared board
  where
    rowCleared row = all cellCleared row
    cellCleared cell = isBomb cell || revealed cell

-- Print the board with row and column indices
printBoard :: Board -> IO ()
printBoard board = do
  putStrLn $ "   " ++ concat [ show c ++ " " | c <- [0 .. (length (head board) - 1)] ]
  mapM_ (printRow board) (zip [0 ..] board)
  where
    printRow :: Board -> (Int, [Cell]) -> IO ()
    printRow _ (r, row) = putStrLn $ show r ++ "  " ++ unwords (map show row)
