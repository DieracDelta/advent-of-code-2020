import           Data.Foldable                 as F
import qualified Data.Set                      as S
import qualified Data.List                     as L
import qualified Data.Map                      as M
import qualified Control.Monad                 as CM
import qualified Data.Char                     as C
import qualified Data.Maybe                    as DM
-- should probably change out to a mutable vector, but I'm lazy and it's "fast enough"...
-- import qualified Data.Vector                   as V

data Cups = Cups Int Int (M.Map Int Int) [Int] deriving Show

main :: IO ()
main = interact part_2

part_1 :: String -> String
part_1 s =
  let parsedLine = parseInput s
  in  show $ iterate performRound parsedLine !! 100

part_2 :: String -> String
part_2 s =
  let (Cups h _ omap _) = parseInput s
      start_cups        = Cups h 1000000 (append_to_input omap) []
      (Cups _ _ mmap _) = iterate performRound start_cups !! 10000000
      first             = mmap M.! 1
      second            = mmap M.! first
  in  show $ first * second

-- should probably also automate this so input not hardcoded...
appendToInput :: M.Map Int Int -> M.Map Int Int
appendToInput omap =
  let nmap =
          F.foldl' (\acc ele -> M.insert ele (ele + 1) acc) omap [10 .. 999999]
      nnmap = M.insert 4 10 nmap
      f_map = M.insert 1000000 1 nnmap
  in  f_map

correctEle :: Int -> Int -> Int
correctEle cur_attempt max_ele =
  if cur_attempt == 0 then max_ele else cur_attempt

getDestCup :: Int -> Int -> S.Set Int -> Int
getDestCup cur_attempt max_ele cups =
  let corrected_ele = correctEle (cur_attempt - 1) max_ele
  in  if S.member corrected_ele cups
        then getDestCup corrected_ele max_ele cups
        else corrected_ele

findElement :: Int -> Int -> M.Map Int Int -> [Int]
findElement start_ele num_eles mmap = if num_eles == 0
  then []
  else
    let new_start_ele = DM.fromJust $ M.lookup start_ele mmap
    in  new_start_ele : findElement new_start_ele (num_eles - 1) mmap

performRound :: Cups -> Cups
performRound bleh@(Cups cur_ele max_ele cups _) =
  let three_cups    = findElement cur_ele 3 cups
      dest_cup      = getDestCup cur_ele max_ele (S.fromList three_cups)
      dest_next_cup = DM.fromJust $ M.lookup dest_cup cups
      first_cup     = head three_cups
      last_cup      = last three_cups
      end_cup       = DM.fromJust $ M.lookup last_cup cups
      updated_map_1 = M.insert dest_cup first_cup cups
      updated_map_2 = M.insert last_cup dest_next_cup updated_map_1
      final_map     = M.insert cur_ele end_cup updated_map_2
  in  Cups (DM.fromJust $ M.lookup cur_ele final_map)
           max_ele
           final_map
           three_cups


parseInput :: String -> Cups
parseInput s =
  let inputs_list = read . CM.return <$> filter C.isDigit s :: [Int]
      len         = L.length inputs_list
      mmap =
          F.foldl'
            (\acc indx ->
              let remainder = (indx + 1) `rem` len
              in  M.insert (inputs_list !! indx) (inputs_list !! remainder) acc
            )
            M.empty
            [0 .. len - 1] :: M.Map Int Int
  in  Cups (head inputs_list) (maximum inputs_list) mmap []
