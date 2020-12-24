import qualified Data.Foldable                 as F
import qualified Data.List                     as L
import qualified Data.Set                      as S

main :: IO ()
main = interact part_2

parseLine :: String -> [Int]
parseLine s =
  let f = L.zipWith (+)
  in  case s of
        ('e'       : xs) -> f [2, 0] $ parseLine xs
        ('w'       : xs) -> f [-2, 0] $ parseLine xs
        ('n' : 'e' : xs) -> f [1, -1] $ parseLine xs
        ('n' : 'w' : xs) -> f [-1, -1] $ parseLine xs
        ('s' : 'e' : xs) -> f [1, 1] $ parseLine xs
        ('s' : 'w' : xs) -> f [-1, 1] $ parseLine xs
        []               -> [0, 0]
        _                -> error "failed to parse"

updateSet :: S.Set [Int] -> [Int] -> S.Set [Int]
updateSet blacks coords =
  let f = if S.member coords blacks then S.delete else S.insert
  in  f coords blacks

getNeighbors :: [Int] -> S.Set [Int]
getNeighbors coords =
  S.fromList
    $   zipWith (+) coords
    <$> [[2, 0], [-2, 0], [1, -1], [-1, -1], [1, 1], [-1, 1]]

checkBlacks :: S.Set [Int] -> S.Set [Int]
checkBlacks state =
  S.filter (\ele -> S.size (state `S.intersection` getNeighbors ele) == 1) state

checkWhite :: S.Set [Int] -> [Int] -> S.Set [Int]
checkWhite state b =
  let neighbors = getNeighbors b
  in  F.foldl'
        (\acc ele -> if length (getNeighbors ele `S.intersection` state) == 2
          then S.insert ele acc
          else acc
        )
        S.empty
        neighbors

checkWhites :: S.Set [Int] -> S.Set [Int]
checkWhites state =
  F.foldl' (\acc ele -> acc `S.union` checkWhite state ele) S.empty state

part_1 :: String -> String
part_1 s = show $ S.size $ F.foldl' updateSet S.empty $ parseLine <$> lines s

performRound :: S.Set [Int] -> S.Set [Int]
performRound state = checkWhites state `S.union` checkBlacks state

part_2 :: String -> String
part_2 s =
  let state = F.foldl' updateSet S.empty $ parseLine <$> lines s
  in  show $ length $ iterate performRound state !! 100
