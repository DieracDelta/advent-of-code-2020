import           Data.Char
import           Data.Set                       ( fromList
                                                , member
                                                )

main :: IO ()
main = interact part_1

-- stolen from stdlib
splitBy :: (Char -> Bool) -> [Char] -> [[Char]]
splitBy f s = case dropWhile f s of
  [] -> []
  s' -> w : splitBy f s'' where (w, s'') = break f s'

mergeLines :: ([a] -> Bool) -> ([[a]] -> [a]) -> [[a]] -> [[a]]
mergeLines f combine l = case span f l of
  (f1, []) -> [combine f1]
  (f1, f2) -> combine f1 : mergeLines f combine (tail f2)

checkLenDigits :: Int -> String -> Bool
checkLenDigits len s =
  length s == len && foldr (\x acc -> acc && isDigit x) True s

checkRange :: Int -> Int -> String -> Bool
checkRange start end s = let num = read s in num <= end && num >= start

parseField :: String -> Bool
parseField s =
  let [f1, f2] = splitBy (== ':') s
  in  case f1 of
        "byr" -> checkLenDigits 4 f2 && checkRange 1920 2002 f2
        "iyr" -> checkLenDigits 4 f2 && checkRange 2010 2020 f2
        "eyr" -> checkLenDigits 4 f2 && checkRange 2020 2030 f2
        "hgt" ->
          let (len, typ) = span isDigit f2
          in  (case typ of
                "cm" -> checkLenDigits 3 len && checkRange 150 193 len
                "in" -> checkLenDigits 2 len && checkRange 59 76 len
                _    -> False
              )
        "hcl" ->
          let (hashtag, digits) = break isHexDigit f2
          in  length hashtag == 1 && head hashtag == '#' && length digits == 6
        "ecl" ->
          let eyeclrs =
                  fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
          in  member f2 eyeclrs
        "pid" -> checkLenDigits 9 f2
        "cid" -> False
        _     -> False

parseFieldLessAggressively :: String -> Bool
parseFieldLessAggressively s =
  let [f1, _] = splitBy (== ':') s
  in  member f1 $ fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]


part_1 :: String -> String
part_1 s = show $ length $ filter (== 7) $ fmap
  (length . filter id . map parseFieldLessAggressively . words)
  (mergeLines (/= []) unwords $ lines s)

part_2 :: String -> String
part_2 s = show $ length $ filter (== 7) $ fmap
  (length . filter id . map parseField . words)
  (mergeLines (/= []) unwords $ lines s)
