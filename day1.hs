import Data.Maybe
import Control.Monad

  
is_valid :: [Int] -> Bool
is_valid a = sum a == 2020

find_satisfying_sum :: Int -> [Int] -> [Int]
find_satisfying_sum n l =
  [product x | x <- replicateM n l, is_valid x]
  
part_x n = 
                   let l = map read $ lines s in
                   let valids = find_satisfying_sum n l in
                   foldr (\x acc -> acc ++ "\n" ++ (show x)) "" valids

part_1 s = part_x 2
part_2 s = part_x 3

main = interact part_2
