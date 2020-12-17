import           Data.Char                      ( isDigit )
import           Data.List                      ( uncons
                                                , find
                                                , transpose
                                                )
import           Data.Foldable                  ( foldl' )
import           Data.Maybe                     ( isJust
                                                , isNothing
                                                , Maybe(Just, Nothing)
                                                )

import           Data.Bits
import           Data.Foldable                  ( fold )
import           Data.Set                       ( Set
                                                , fromList
                                                , (\\)
                                                , singleton
                                                , empty
                                                , union
                                                )

import           Data.Map                       ( Map
                                                , (!)
                                                , empty
                                                , insert
                                                )


main :: IO ()
main = interact part_2

mergeLines :: ([a] -> Bool) -> ([[a]] -> [a]) -> [[a]] -> [[a]]
mergeLines f combine l = case span f l of
  (f1, []) -> [combine f1]
  (f1, f2) -> combine f1 : mergeLines f combine (tail f2)

splitBy :: (Char -> Bool) -> [Char] -> [[Char]]
splitBy f s = case dropWhile f s of
  [] -> []
  s' -> w : splitBy f s'' where (w, s'') = break f s'

data Range = Range Int Int deriving (Show, Eq, Ord)

inRange :: Int -> Range -> Bool
inRange i (Range s e) = s <= i && i <= e

inRanges :: [Range] -> Int -> Bool
inRanges rs i = isJust $ find (inRange i) rs

getConstraint :: String -> (String, [Range])
getConstraint s =
  let
    l = filter (isJust . find isDigit)
      $ splitBy (\x -> (x == '-') || (x == ' ')) s
  in  case read . dropWhile (not . isDigit) <$> l of
        [l1, l2, l3, l4] ->
          (takeWhile (not . isDigit) s, [Range l1 l2, Range l3 l4])

parseTicket :: String -> [Int]
parseTicket s = read <$> splitBy (== ',') s

part_1 :: String -> String
part_1 s =
  let l       = mergeLines (/= []) unlines $ lines s
      reqs    = fold $ snd . getConstraint <$> lines (head l)
      tickets = fold $ parseTicket <$> tail (lines (l !! 2))
  in  show $ sum $ filter (not . inRanges reqs) tickets
  -- in  show $ reqs

checkPerm :: [[Int]] -> [(String, [Range])] -> Bool
checkPerm tickets combos =
  let rngs = snd <$> combos
  in  foldl' (&&) True
        $   foldl' (&&) True
        <$> (zipWith inRanges rngs <$> tickets)

-- matrix of rows
tryEle
  :: Map [Range] (Map Int Bool)
  -> Set (String, [Range])
  -> [[Int]]
  -> Maybe [String]
tryEle mmap combos rows = if null combos
  then Just []
  else foldl'
    (\acc compl@(name, ele) -> case acc of
      Nothing -> case uncons rows of
        Just (row, tl) -> if (mmap ! ele) ! (length rows)
          then
            tryEle mmap (combos \\ singleton compl) tl
              >>= (\x -> Just $ name : x)
          else Nothing
        Nothing -> Nothing
      _ -> acc
    )
    Nothing
    combos

constructMap :: Set (String, [Range]) -> [[Int]] -> Map String (Set Int)
constructMap s rows = foldl'
  (\acc (str, rngs) ->
    let m = foldl'
          (\inner_acc (row, indx) ->
            if (isNothing $ find (not . inRanges rngs) row)
              then union inner_acc (singleton (length rows - indx))
              else inner_acc
          )
          Data.Set.empty
          (zip rows [0 ..])
    in  insert str m acc
  )
  Data.Map.empty
  s


part_2 :: String -> String
part_2 s =
  let l    = mergeLines (/= []) unlines $ lines s
      reqs = getConstraint <$> lines (head l)
      nearby_tickets =
          filter (isNothing . find (not . inRanges (fold (snd <$> reqs))))
            $   parseTicket
            <$> tail (lines (l !! 2))
      your_ticket = parseTicket (lines (l !! 1) !! 1)
      tickets     = transpose $ your_ticket : nearby_tickets
  -- in  show
  --        tryEle (constructMap (fromList reqs) tickets) (fromList reqs)
  --       $ tickets
  -- solve the rest by hand though you should be able to get 1 recursively
  in  show $ constructMap (fromList reqs) tickets
