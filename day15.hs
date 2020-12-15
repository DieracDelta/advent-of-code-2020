import           Data.Char                      ( isDigit )
import           Data.List                      ( uncons )
import           Data.Foldable                  ( foldl' )

import           Data.Bits
import           Data.Map                       ( Map
                                                , insert
                                                , empty
                                                , foldr
                                                , findWithDefault
                                                )


main :: IO ()
main = interact $ part_1

-- turnnum, last_spoken, map
data State = State Int Int (Map Int Int) deriving Show

performTurn :: State -> State
performTurn (State turnnum last_spoken_number last_spoken) =
  let new_spoken =
          (turnnum - 1)
            - findWithDefault (turnnum - 1) last_spoken_number last_spoken
      new_map = insert last_spoken_number (turnnum - 1) last_spoken
  in  State (turnnum + 1) new_spoken new_map

splitBy :: (Char -> Bool) -> [Char] -> [[Char]]
splitBy f s = case dropWhile f s of
  [] -> []
  s' -> w : splitBy f s'' where (w, s'') = break f s'


part_1 :: String -> String
part_1 s = show
  $ foldl' (\acc _ -> performTurn acc) (parseStarters s) [0 .. (30000000 - 8)]

parseStarters :: String -> State
parseStarters s =
  let nums = read <$> splitBy (== ',') s
      mmap =
          foldl' (\(idx, acc) x -> (idx + 1, insert x idx acc)) (1, empty) nums
  in  State (length nums + 1) (last nums) (snd mmap)
