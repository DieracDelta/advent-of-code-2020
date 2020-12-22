import           Control.Monad                  ( replicateM
                                                , foldM
                                                )
import           Numeric                        ( showIntAtBase )
import           Data.Ord                       ( comparing )
import           Data.Maybe                     ( fromJust
                                                , Maybe
                                                , catMaybes
                                                , isJust
                                                , mapMaybe
                                                , fromMaybe
                                                )
import           Data.Word
import           Data.Bits
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
                                                , dropWhile
                                                , takeWhile
                                                , transpose
                                                , (\\)
                                                , sortOn
                                                )
import           Data.Char                      ( isDigit
                                                , intToDigit
                                                )
import           Data.Map                       ( insert
                                                , singleton
                                                , empty
                                                , Map
                                                , toList
                                                , (!)
                                                )
import           Text.Printf                    ( printf )

data MatchType = LeftSide | BottomSide

-- top bottom left right
data TileRep = TileRep {indt ::Int, t :: Word16, b :: Word16, l:: Word16, r:: Word16} deriving (Eq, Show, Ord)

main :: IO ()
main = interact part_1

safeHead :: [a] -> Maybe a
safeHead list = case list of
  []      -> Nothing
  (x : _) -> Just x

performRotate :: TileRep -> TileRep
performRotate tr =
  tr { t = flipBits (l tr), b = flipBits (r tr), l = b tr, r = t tr }
  -- tr { t = l tr, b = r tr, l = flipBits $ b tr, r = flipBits $ t tr }

flipLR :: TileRep -> TileRep
flipLR tr = tr { l = r tr, r = l tr, t = flipBits $ t tr, b = flipBits $ b tr }

flipTB :: TileRep -> TileRep
flipTB tr = tr { t = b tr, b = t tr, l = flipBits $ l tr, r = flipBits $ r tr }

performFlip :: TileRep -> [TileRep]
performFlip tr = [tr, flipTB tr, flipLR tr, flipTB $ flipLR tr]

flipBits :: Word16 -> Word16
flipBits x =
  foldl' (\acc i -> if testBit x i then setBit acc (9 - i) else acc) 0 [0 .. 9]

rotateTile :: Int -> TileRep -> [TileRep]
rotateTile i tile = case i of
  0 -> [tile]
  _ -> tile : rotateTile (i - 1) (performRotate tile)

matchSide :: TileRep -> (MatchType, TileRep) -> Bool
matchSide considered_tile (mt, already_set_tile) = case mt of
  BottomSide -> b already_set_tile == t considered_tile
  LeftSide   -> r already_set_tile == l considered_tile

getNeighbors :: [Int] -> Map [Int] TileRep -> [(MatchType, TileRep)]
getNeighbors ele cur_map = case ele of
  [x, y] ->
    [ (LeftSide, cur_map ! [x - 1, y]) | x - 1 >= 0 ]
      ++ [ (BottomSide, cur_map ! [x, y - 1]) | y - 1 >= 0 ]
  _ -> error "invalid element"

getDist :: [Int] -> Int
getDist = sum . Prelude.map (\a -> a * a)

part_1 :: String -> String
part_1 s =
  let
    tiles = parseLines <$> mergeLines (/= []) unlines (lines s)
    width = round (sqrt (fromIntegral $ length tiles) :: Double) - 1
    indices =
      sortOn getDist $ [ [x, y] | x <- [0 .. width], y <- [0 .. width] ]
    results = fromJust $ generateMap Data.Map.empty tiles indices
  in
    show
    $   product
    $   indt
    .   (results !)
    <$> [[0, 0], [width, 0], [0, width], [width, width]]
  --       ]

generateMap
  :: Map [Int] TileRep -> [TileRep] -> [[Int]] -> Maybe (Map [Int] TileRep)
generateMap cur_map remaining_tiles remaining_indices =
  case uncons remaining_indices of
    Nothing                -> Just cur_map
    Just (cur_index, rest) -> safeHead $ mapMaybe
      (checkRotations cur_map remaining_tiles rest cur_index)
      remaining_tiles

checkRotations
  :: Map [Int] TileRep
  -> [TileRep]
  -> [[Int]]
  -> [Int]
  -> TileRep
  -> Maybe (Map [Int] TileRep)
checkRotations cur_map remaining_tiles remaining_indices cur_index tile =
  let rest_of_tiles = remaining_tiles \\ [tile]
      orientations_to_try =
          Data.Set.toList
            $   Data.Set.fromList
            $   foldl' (++) []
            $   rotateTile 3
            <$> performFlip tile
      neighbors = getNeighbors cur_index cur_map
  in  safeHead $ mapMaybe
        (\oriented_tile ->
          if foldl' (\acc x -> acc && matchSide oriented_tile x) True neighbors
            then generateMap (Data.Map.insert cur_index oriented_tile cur_map)
                             rest_of_tiles
                             remaining_indices
            else Nothing
        )
        orientations_to_try

mergeLines :: ([a] -> Bool) -> ([[a]] -> [a]) -> [[a]] -> [[a]]
mergeLines f combine lli = case span f lli of
  (f1, []) -> [combine f1]
  (f1, f2) -> combine f1 : mergeLines f combine (tail f2)

genWord16 :: [Char] -> Word16
genWord16 chars =
  foldl' setBit 0 $ fmap snd $ filter ((== '#') . fst) $ zip chars [9, 8 .. 0]

parseLines :: String -> TileRep
parseLines s = case uncons (lines s) of
  Just (idline, rest) ->
    let identifier =
            read $ takeWhile isDigit $ dropWhile (not . isDigit) idline
    in  TileRep identifier
                (genWord16 $ head rest)
                (genWord16 $ last rest)
                (genWord16 $ head $ transpose rest)
                (genWord16 $ last $ transpose rest)
  _ -> error "incorrect formatting"
