import           Data.Maybe
import           Data.Set

main :: IO ()
main = interact part_2

data BSP = Up | Down deriving Show

calcBSP :: [BSP] -> Int -> Int -> Int
calcBSP str start_point end_point = fst $ Prelude.foldr
  (\x (s, e) ->
    let mid = (e - s) `div` 2
    in  case x of
          Down -> (s, s + mid)
          Up   -> (s + mid + 1, e)
  )
  (start_point, end_point)
  str

parseChar :: Char -> Maybe BSP
parseChar s = case s of
  'F' -> Just Down
  'B' -> Just Up
  'R' -> Just Up
  'L' -> Just Down
  _   -> Nothing

parseId :: String -> Int
parseId s =
  let (row, col) = Prelude.splitAt 7 $ mapMaybe parseChar s
      result_row = calcBSP (reverse row) 0 127
      result_col = calcBSP (reverse col) 0 7
  in  result_row * 8 + result_col

part_1 :: String -> String
part_1 s = show $ maximum [ parseId l | l <- lines s ]

part_2 :: String -> String
part_2 s =
  let ids   = fromList [ parseId l | l <- lines s ]
      range = fromList [0 .. maximum ids]
  in  show
        $  Data.Set.filter (\x -> member (x + 1) ids && member (x - 1) ids)
        $  range
        \\ ids
