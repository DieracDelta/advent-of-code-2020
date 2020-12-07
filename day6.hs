import           Data.Set

main :: IO ()
main = interact part_2

mergeLines :: ([a] -> Bool) -> ([[a]] -> [a]) -> [[a]] -> [[a]]
mergeLines f combine l = case span f l of
  (f1, []) -> [combine f1]
  (f1, f2) -> combine f1 : mergeLines f combine (tail f2)

combiner :: [String] -> String
combiner ls =
  toList $ Prelude.foldr intersection (fromList ['a' .. 'z']) $ fromList <$> ls

part_1 :: String -> String
part_1 s =
  show $ sum $ length . fromList <$> mergeLines (/= []) concat (lines s)

part_2 :: String -> String
part_2 s = show $ sum $ length <$> mergeLines (/= []) combiner (lines s)
