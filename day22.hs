import qualified Data.List                     as L
import qualified Data.Set                      as S
import qualified Data.Map                      as M
import qualified Data.Sort                     as DS

main :: IO ()
main = interact part_2

data Winner = Player1 | Player2

part_1 :: String -> String
part_1 s =
  let [p1, p2] = parseLines s in show $ calculateScore $ playGameP1 p1 p2

part_2 :: String -> String
part_2 s =
  let [p1, p2] = parseLines s
  in  show $ calculateScore $ snd $ playGameP2 S.empty p1 p2

parseLines :: String -> [[Int]]
parseLines s = parseLine <$> mergeLines (/= []) unlines (lines s)

parseLine :: String -> [Int]
parseLine s = read <$> drop 1 (lines s)

mergeLines :: ([a] -> Bool) -> ([[a]] -> [a]) -> [[a]] -> [[a]]
mergeLines f combine lli = case span f lli of
  (f1, []) -> [combine f1]
  (f1, f2) -> combine f1 : mergeLines f combine (tail f2)


playGameP1 :: [Int] -> [Int] -> [Int]
playGameP1 p1 p2 =
  let maybe_p1_c = L.uncons p1
      maybe_p2_c = L.uncons p2
  in  case (maybe_p1_c, maybe_p2_c) of
        (Just (p1_c, p1_rest), Just (p2_c, p2_rest)) -> if p1_c > p2_c
          then playGameP1 (p1_rest ++ [p1_c, p2_c]) p2_rest
          else playGameP1 p1_rest (p2_rest ++ [p2_c, p1_c])
        (Just _ , Nothing) -> p1
        (Nothing, Just _ ) -> p2
        (Nothing, Nothing) -> error "can't have no cards"

playGameP2 :: S.Set ([Int], [Int]) -> [Int] -> [Int] -> (Winner, [Int])
playGameP2 seen_states p1 p2 =
  let cur_state       = (p1, p2)
      new_seen_states = seen_states `S.union` S.singleton cur_state
      maybe_p1_c      = L.uncons p1
      maybe_p2_c      = L.uncons p2
  in  if S.member cur_state seen_states
        then (Player1, p1)
        else case (maybe_p1_c, maybe_p2_c) of
          (Just (p1_c, p1_rest), Just (p2_c, p2_rest)) ->
            let winner =
                    if p1_c <= L.length p1_rest && p2_c <= L.length p2_rest
                      then fst $ playGameP2 S.empty
                                            (L.take p1_c p1_rest)
                                            (L.take p2_c p2_rest)
                      else if p1_c > p2_c then Player1 else Player2
            in  case winner of
                  Player1 ->
                    playGameP2 new_seen_states (p1_rest ++ [p1_c, p2_c]) p2_rest
                  Player2 ->
                    playGameP2 new_seen_states p1_rest (p2_rest ++ [p2_c, p1_c])
          (Just _ , Nothing) -> (Player1, p1)
          (Nothing, Just _ ) -> (Player2, p2)
          (Nothing, Nothing) -> error "can't have no cards"


calculateScore :: [Int] -> Int
calculateScore stack =
  let len = L.length stack in sum $ zipWith (*) stack [len, len - 1 .. 1]
