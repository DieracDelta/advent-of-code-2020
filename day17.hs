import           Control.Monad                  ( replicateM, foldM )
import           Data.Maybe                     (fromJust)

import           Data.Foldable                  ( foldl', fold )
import           Data.Set                       ( Set
                                                , fromList
                                                , singleton
                                                , empty
                                                , union
                                                ,member
                                                ,notMember
                                                ,filter
                                                , map
                                                ,toList
                                                )
main :: IO ()
main = interact part_2

part_1 :: String -> String
part_1 = doPart 3

part_2 :: String -> String
part_2 = doPart 4

doPart :: Int -> String -> String
doPart dim s = show $ length $ fromJust $ foldM (\state _ -> Just $ doTurn dim state) (parseInput dim s)  [0..5]

doTurn :: Int -> Set [Int] -> Set [Int]
doTurn dim actives = handleActive dim actives `union` handleInactive dim actives

-- (x, y, z)
parseInput :: Int -> String -> Set [Int]
parseInput dim s = fold
  [ if char == '#' then singleton $ [x, y] ++ replicate (dim - 2) 0 else empty
  | (line, y) <- zip (lines s) [0 ..]
  , (char, x) <- zip line [0 ..]
  ]

getNeighbors :: Int -> [Int] -> Set [Int]
getNeighbors dim ele = fromList $ do
  neighbor_indx <- replicateM dim [-1, 0, 1]
  let neighbor = zipWith (+) ele neighbor_indx in [neighbor | ele /= neighbor]

remainActive :: Int -> Set Int -> Set [Int] -> [Int] -> Bool
remainActive dim allowable_counts actives ele =
  let adj_actives_count = length $ Data.Set.filter (`member` actives) $ getNeighbors dim ele in
    member adj_actives_count allowable_counts

handleActive :: Int -> Set [Int] -> Set [Int]
handleActive dim actives = Data.Set.filter (remainActive dim (fromList [2, 3]) actives) actives

handleInactive :: Int -> Set [Int] -> Set [Int]
handleInactive dim actives =
  fromList $ do
    active_ele <- toList actives
    toList $
      Data.Set.filter (remainActive dim (singleton 3) actives)
      $ Data.Set.filter (`notMember` actives) $ getNeighbors dim active_ele
