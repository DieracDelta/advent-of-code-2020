import           Data.Foldable                  ( foldl' )
import qualified Data.Set                      as S
import qualified Data.List                     as L
import qualified Data.Map                      as M
import qualified Control.Monad                 as CM
import qualified Data.Char                     as C

-- invariant: head of list is ALWAYS current cup
-- cups shift_distance [cups]
data Cups = Cups Int Int [Int] deriving Show

rotateBack :: Cups -> [Int]
rotateBack (Cups sd _ cups) =
  let len         = length cups
      num_to_drop = len - (sd `rem` len)
  in  take len $ drop num_to_drop (cycle cups)

main :: IO ()
main = interact part_2

part_1 :: String -> String
part_1 s =
  let parsedLine = parseInput s
      Cups _ _ cups =
          iterate performRound (Cups 0 (maximum parsedLine) parsedLine) !! 100
      (tl, hd) = span (/= 1) cups
  in  show $ tail hd ++ tl

part_2 :: String -> String
part_2 s =
  let parsedLine = parseInput s ++ [10 .. 1000000]
      Cups _ _ cups =
          iterate performRound (Cups 0 (maximum parsedLine) parsedLine) !! 100
          -- iterate performRound (Cups 0 (maximum parsedLine) parsedLine)
          --   !! 10000000
      (tl, hd) = span (/= 1) cups
  in  show $ take 2 $ (tail hd ++ tl)

correctEle :: Int -> Int -> Int
correctEle cur_attempt max_ele =
  if cur_attempt == 0 then max_ele else cur_attempt

getDestCup :: Int -> Int -> S.Set Int -> Int
getDestCup cur_attempt max_ele cups = if S.member cur_attempt cups
  then getDestCup (correctEle (cur_attempt - 1) max_ele) max_ele cups
  else cur_attempt


performRound :: Cups -> Cups
performRound (Cups sd max_ele cups) =
  let (cur_cup, remaining) = splitAt 1 cups
      (three_cups, rest) = splitAt 3 remaining
      destination_cup = getDestCup (correctEle (head cur_cup - 1) max_ele)
                                   max_ele
                                   (S.fromList three_cups)
      (s, e)      = span (/= destination_cup) (cur_cup ++ rest)
      end_portion = case L.uncons e of
        Nothing     -> three_cups
        Just (a, b) -> a : (three_cups ++ b)
      total_unrotated = s ++ end_portion
  in  Cups (sd + 1) max_ele (tail total_unrotated ++ [head total_unrotated])

parseInput :: String -> [Int]
parseInput s = read . CM.return <$> filter C.isDigit s
