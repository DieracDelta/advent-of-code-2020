import           Control.Monad                  ( replicateM
                                                , foldM
                                                )
import           Data.Maybe                     ( fromJust
                                                , Maybe
                                                )

import           Data.Foldable                  ( foldl'
                                                , fold
                                                )
import           Data.Set                       ( Set
                                                , fromList
                                                , singleton
                                                , empty
                                                , union
                                                , member
                                                , notMember
                                                , cartesianProduct
                                                -- , filter
                                                , map
                                                , toList
                                                )
import           Data.List                      ( uncons
                                                , find
                                                , elemIndex
                                                )
import           Data.Map                       ( insert
                                                , singleton
                                                , empty
                                                , Map
                                                , (!)
                                                )

data Rule = NT [Int] (Maybe [Int]) | T Char deriving Show

main :: IO ()
main = interact part_1

part_1 :: String -> String
part_1 s =
  let [rule_str, values] = mergeLines (/= []) unlines (lines s)
      ruleMap            = foldl' (\acc x -> uncurry insert x acc)
                                  Data.Map.empty
                                  (parseRule <$> lines rule_str)
      useful_vals = getValues ruleMap 0
      results     = filter (`member` useful_vals) $ lines values
  in  show $ length results

aggregateRule :: Map Int Rule -> [Int] -> Set String
aggregateRule rmap rule_keys =
  let values = getValues rmap <$> rule_keys
  in  foldl'
        (\acc ss -> Data.Set.map (uncurry (++)) (cartesianProduct acc ss))
        (Data.Set.singleton "")
        values

getValues :: Map Int Rule -> Int -> Set String
getValues rmap k = case rmap ! k of
  NT r1 (Just r2) -> aggregateRule rmap r1 `union` aggregateRule rmap r2
  NT r1 Nothing   -> aggregateRule rmap r1
  T c             -> Data.Set.singleton [c]

mergeLines :: ([a] -> Bool) -> ([[a]] -> [a]) -> [[a]] -> [[a]]
mergeLines f combine l = case span f l of
  (f1, []) -> [combine f1]
  (f1, f2) -> combine f1 : mergeLines f combine (tail f2)

splitBy :: (Char -> Bool) -> [Char] -> [[Char]]
splitBy f s = case dropWhile f s of
  [] -> []
  s' -> w : splitBy f s'' where (w, s'') = break f s'

readRule :: String -> [Int]
readRule r = read <$> splitBy (== ' ') r

parseRule :: String -> (Int, Rule)
parseRule s =
  let [ruleNum, rest] = splitBy (== ':') s
  in  case elemIndex '\"' rest of
        Nothing -> case splitBy (== '|') rest of
          [r1]     -> (read ruleNum, NT (readRule r1) Nothing)
          [r1, r2] -> (read ruleNum, NT (readRule r1) (Just $ readRule r2))
        Just indx -> (read ruleNum, T $ rest !! (indx + 1))
