import           Data.List
import           Data.Maybe

main :: IO ()
main = interact part_2

checkEle :: [Int] -> Int -> Bool
checkEle p i =
  isJust
    $            Data.List.find (== i)
    $            ((+) <$> p <*> p)
    Data.List.\\ zipWith (+) p p

checkText :: [Int] -> [Int] -> Int
checkText p is = case is of
  []       -> -1
  [x     ] -> x
  (x : xs) -> if checkEle p x then checkText (tail p ++ [x]) xs else x

getRep :: (Int, Int) -> [Int] -> [Int]
getRep (s, e) nums = take (e - s + 1) $ drop s nums

incRep :: (Int, Int) -> Bool -> (Int, Int)
incRep (s, e) bail = if bail then (s + 1, s + 2) else (s, e + 1)

checkList :: Int -> (Int, Int) -> [Int] -> Int
checkList i (s, e) nums =
  let l = getRep (s, e) nums
  in  if sum l == i
        then minimum l + maximum l
        else if sum l > i || e == length nums - 2
          then checkList i (incRep (s, e) True) nums
          else checkList i (incRep (s, e) False) nums

part_1 :: Int -> String -> Int
part_1 preamble_len s =
  uncurry checkText $ splitAt preamble_len $ read <$> lines s


part_2 :: String -> String
part_2 s =
  let desired_num = part_1 25 s
      r           = checkList desired_num (0, 1) (read <$> lines s)
  in  show r
