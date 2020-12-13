import           Data.Either                    ( fromLeft
                                                , isLeft
                                                )
import           Data.List                      ( sortOn
                                                , find
                                                , uncons
                                                )
import           Data.Bifunctor                 ( first )

main :: IO ()
main = interact part_2

hammer :: (Int -> Maybe Int) -> Int -> (Int, Int)
hammer f x = case f x of
  Just r_val -> (r_val, x)
  Nothing    -> hammer f (x + 1)

hammerP2 :: (Int -> Bool) -> Int -> Int
hammerP2 f x = if f x then x else hammerP2 f $ x + 1

splitBy :: (Char -> Bool) -> [Char] -> [[Char]]
splitBy f s = case dropWhile f s of
  [] -> []
  s' -> w : splitBy f s'' where (w, s'') = break f s'

part_1 :: String -> String
part_1 s =
  let ht                    = lines s
      earliest_time         = read $ head ht :: Int
      times = read <$> filter (/= "x") (splitBy (== ',') (ht !! 1))
      (bus_id, depart_time) = hammer (checkTimes times) earliest_time
  in  show $ bus_id * (depart_time - earliest_time)

checkTimes :: [Int] -> Int -> Maybe Int
checkTimes l_times time = find (\x -> time `rem` x == 0) l_times

mapToEither :: String -> Either Int String
mapToEither s = if s == "x" then Right s else Left $ read s

isValid :: Int -> Int -> Int -> Bool
isValid next_residue next_bid cur_attempt =
  (cur_attempt `mod` next_bid) == next_residue

getTimes :: [(Int, Int)] -> Int -> Int -> Int -> Int -> Int
getTimes time_list increments next_residue next_bid cur_attempt =
  if isValid next_residue next_bid cur_attempt
    then case uncons time_list of
      Just ((n_bid, n_indx), n_time_list) -> getTimes
        n_time_list
        (increments * next_bid)
        ((n_bid - n_indx) `mod` n_bid)
        n_bid
        cur_attempt
      Nothing -> cur_attempt
    else getTimes time_list
                  increments
                  next_residue
                  next_bid
                  (cur_attempt + increments)

part_2 :: String -> String
part_2 s =
  let times =
          reverse
            $   sortOn fst
            $   first (fromLeft 1)
            <$> ( filter (isLeft . fst)
                $ zip (mapToEither <$> splitBy (== ',') (lines s !! 1)) [0 ..]
                )
  in  show $ getTimes times 1 0 1 100000000000001
