main :: IO ()
main = interact part_2

parseString :: String -> [Bool]
parseString = map (== '#')

getTrees :: (Int, Int) -> [[Bool]] -> Int
getTrees slope l =
  let (slope_x, slope_y) = slope
      len                = length l - 1
      width              = length $ head l
      reducer =
          (\i acc ->
            if (l !! mod (i * slope_y) (len + 1)) !! mod (slope_x * i) width
              then acc + 1
              else acc
          )
  in  foldr reducer 0 [0, 1 .. div len slope_y]

part_2 :: String -> String
part_2 s =
  let l = map parseString $ lines s
  in  show $ product $ fmap (`getTrees` l)
                            [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

part_1 :: String -> String
part_1 s = show $ getTrees (3, 1) $ map parseString $ lines s
