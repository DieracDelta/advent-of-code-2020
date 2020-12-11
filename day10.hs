import           Data.List
import           Data.Maybe
import           Data.Map                       ( Map
                                                , union
                                                , empty
                                                , findWithDefault
                                                , singleton
                                                , member
                                                )

main :: IO ()
main = interact (part_2)

part_1 :: String -> Int
part_1 s =
  let l       = sort $ read <$> lines s :: [Int]
      max_ele = maximum l
      input   = 0 : l
      output  = l ++ [max_ele + 3]
      results = zipWith (-) output input
  in  length (filter (== 1) results) * length (filter (== 3) results)

part_2 :: String -> String
part_2 s =
  let l       = sort $ read <$> lines s
      max_ele = maximum l
  in  show $ fst $ searchFun 0 l max_ele empty

satisfiesConstraint :: Int -> Int -> Bool
satisfiesConstraint a b = let diff = b - a in diff > 0 && diff < 4

searchFun
  :: Int -> [Int] -> Int -> Map (Int, [Int]) Int -> (Int, Map (Int, [Int]) Int)
searchFun cur_ele l final_ele mmap = if cur_ele == final_ele
  then (1, mmap)
  else if member (cur_ele, l) mmap
    then (findWithDefault 0 (cur_ele, l) mmap, mmap)
    else
      let
        (sat, rest) = span (satisfiesConstraint cur_ele) l
        sat_len     = length sat
      in
        if sat_len == 0
          then (0, mmap)
          else
            let
              f x (acc, m) =
                (let
                   remaining_list = (drop (x + 1) sat ++ rest)
                   (r, map_new) =
                     searchFun (sat !! x) remaining_list final_ele m
                 in
                   ( acc + r
                   , Data.Map.union map_new
                                    (singleton (sat !! x, remaining_list) r)
                   )
                )
            in  foldr f (0, mmap) [0 .. (sat_len - 1)]
