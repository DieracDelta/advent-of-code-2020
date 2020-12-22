import           Data.Foldable                  ( foldl' )
import qualified Data.Set                      as S
import qualified Data.List                     as L
import qualified Data.Map                      as M

data Rule = NT [Int] (Maybe [Int]) | T Char deriving Show

main :: IO ()
main = interact part_1

part_1 :: String -> String
part_1 s =
  let [rule_str, values] = mergeLines (/= []) unlines (lines s)
      ruleMap            = foldl' (flip $ uncurry M.insert)
                                  M.empty
                                  (parseRule <$> lines rule_str)
      useful_vals = getValues ruleMap 0
      results     = filter (`S.member` useful_vals) $ lines values
  in  show $ length results

aggregateRule :: M.Map Int Rule -> [Int] -> S.Set String
aggregateRule rmap rule_keys =
  let values = getValues rmap <$> rule_keys
  in  foldl' (\acc ss -> S.map (uncurry (++)) (S.cartesianProduct acc ss))
             (S.singleton "")
             values

getValues :: M.Map Int Rule -> Int -> S.Set String
getValues rmap k = case rmap M.! k of
  NT r1 (Just r2) -> aggregateRule rmap r1 `S.union` aggregateRule rmap r2
  NT r1 Nothing   -> aggregateRule rmap r1
  T c             -> S.singleton [c]

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
  in  case L.elemIndex '\"' rest of
        Nothing -> case splitBy (== '|') rest of
          [r1]     -> (read ruleNum, NT (readRule r1) Nothing)
          [r1, r2] -> (read ruleNum, NT (readRule r1) (Just $ readRule r2))
          _        -> error "incorrect formatting"
        Just indx -> (read ruleNum, T $ rest !! (indx + 1))
