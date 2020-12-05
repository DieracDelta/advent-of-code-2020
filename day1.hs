import           Data.Maybe
import           Control.Monad


isValid :: [Int] -> Bool
isValid a = sum a == 2020


findSatisfyingSum :: Int -> [Int] -> [Int]
findSatisfyingSum n l = [ product x | x <- replicateM n l, isValid x ]

partX :: Int -> (String -> String)
partX n s =
  let l = map read $ lines s
  in  let valids = findSatisfyingSum n l
      in  foldr (\x acc -> acc ++ "\n" ++ show x) "" valids

part_1 :: String -> String
part_1 = partX 2

part_2 :: String -> String
part_2 = partX 3

main :: IO ()
main = interact part_2
