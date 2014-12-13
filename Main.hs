-- Sudoku solver
import Euler
import Data.Map.Strict (Map, fromList, (!), mapWithKey, keys)
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Tuple
import qualified Data.Traversable as Traversable
import qualified Data.Foldable as Foldable
import Control.Monad

type Coords = (Int,Int)
type Grid a = Map Coords a
data Digit = Empty|One|Two|Three|Four|Five|Six|Seven|Eight|Nine
  deriving (Eq, Show, Enum)
type Sudoku = Grid Digit

-------------------------------------------------------------------------------
--- Fundamental sudoku functions                                            ---
-------------------------------------------------------------------------------
row :: Sudoku -> Coords -> [Digit]
row sd (i,_) = [sd!(i,j) | j<-[1..9]]

col :: Sudoku -> Coords -> [Digit]
col sd (_,j) = [sd!(i,j) | i<-[1..9]]

box :: Sudoku -> Coords -> [Digit]
box sd (i,j) = [sd!(i',j') | i'<-range i, j'<-range j] where
  range n | n < 4 = [1..3] | n > 6 = [7..9] | otherwise = [4..6]

possibleDigits :: Sudoku -> Coords -> [Digit]
possibleDigits sd p = case sd!p of
  Empty -> [One .. Nine] \\ concat [section sd p | section<-[row,col,box]]
  d -> [d]

isSolved :: Sudoku -> Bool
isSolved = Foldable.all (/= Empty)

-- The first square with multiple possible digits
firstBranching :: Sudoku -> Maybe Coords
firstBranching sd = find ((> 1) . length . possibleDigits sd) $ keys sd

isValid :: Sudoku -> Bool
isValid sd = all (not . hasDuplicates . filter (/= Empty)) 
    $ concat [rows,cols,boxes] where
  rows  = [row sd (i,1) | i<-[1..9]]
  cols  = [col sd (1,j) | j<-[1..9]]
  boxes = [box sd (i,j) | i<-[1,4,7], j<-[1,4,7]]

-------------------------------------------------------------------------------
--- Functions for solving sudoku                                            ---
-------------------------------------------------------------------------------
-- Maps an individual digit to its deduced value from the state of the puzzle
deducedDigit :: Sudoku -> Coords -> Digit -> Maybe Digit
deducedDigit sd p d = case (d, possibleDigits sd p) of
  (_    ,[] ) -> Nothing
  (Empty,[n]) -> Just n
  (m    ,_  ) -> Just m

-- Maps an individual digit to its possible values when branching
branchDigits :: Sudoku -> Coords -> Digit -> [Digit]
branchDigits sd p d =
  if firstBranching sd == Just p then possibleDigits sd p else return d

mapMSudoku :: Monad m =>
  (Sudoku -> Coords -> Digit -> m Digit) -> Sudoku -> m Sudoku
mapMSudoku f sd = Traversable.sequence $ mapWithKey (f sd) sd

-- Returns the fixed point of f found by iterating on x
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f x = head . filter (id <==> f) $ iterate f x

-- Solve the puzzle as much as possible by decuction, with no branching
attempt :: Sudoku -> Maybe Sudoku
attempt sd = fixedPoint (>>= mfilter isValid . mapMSudoku deducedDigit) 
  $ return sd

solve :: Sudoku -> [Sudoku]
solve sd = do
  attempted <- maybeToList $ attempt sd
  if isSolved attempted then return attempted else do
    possibleSolution <- mapMSudoku branchDigits attempted
    solve possibleSolution

-------------------------------------------------------------------------------
--- IO and Project Euler solution                                           ---
-------------------------------------------------------------------------------
charValues :: [(Char,Digit)]
charValues = [('0',Empty),('1',One),('2',Two),('3',Three),('4',Four),
              ('5',Five),('6',Six),('7',Seven),('8',Eight),('9',Nine)]

charToDigit :: Char -> Maybe Digit
charToDigit = (`lookup` charValues)

digitToChar :: Digit -> Char
digitToChar = fromJust . (`lookup` map swap charValues)

linesToGrid :: [String] -> Sudoku
linesToGrid = rowsToGrid . fromMaybe e . mapM (mapM charToDigit) where
  e = error "Invalid grid input"
  rowsToGrid = fromList . ([(i,j) | i<-[1..9], j<-[1..9]] `zip`) . concat

topLeftNum :: Sudoku -> Integer
topLeftNum sd = readDecimal $ map (digitToChar . \j -> sd!(1,j)) [1..3]

answer :: String -> Integer
answer = sum . map topLeftNum . solutions where
  solutions = (>>= solve . linesToGrid . tail) . chunksOf 10 . lines

main :: IO ()
main = readFile "resources/p096_sudoku.txt" >>= print . answer
