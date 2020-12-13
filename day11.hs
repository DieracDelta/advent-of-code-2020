import           Data.Vector
import           Data.List                      ( length
                                                , filter
                                                )
import           Data.Bifunctor                 ( bimap )

main :: IO ()
main = interact part_2

data Plot = EmptyPlot | Occupied | Floor deriving (Eq, Show)

parseChar :: Char -> Plot
parseChar c = case c of
  'L' -> EmptyPlot
  '#' -> Occupied
  _   -> Floor

parseRow :: String -> Vector Plot
parseRow s = parseChar <$> fromList s

numOccupied :: Vector (Vector Plot) -> Int
numOccupied rows =
  Data.Vector.sum
    $   Data.Vector.length
    .   Data.Vector.filter (== Occupied)
    <$> rows

isThing :: Plot -> Vector (Vector Plot) -> (Int, Int) -> Bool
isThing thing p (i, j) = Just thing == (p !? i >>= (!? j))

countThing :: Plot -> Vector (Vector Plot) -> Int -> Int -> Int
countThing thing p i j =
  Data.List.length
    $   Data.List.filter id
    $   isThing thing p
    <$> Data.Bifunctor.bimap (+ i) (+ j)
    <$> [(1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, -1), (-1, 0), (-1, 1)]

countThingP2 :: Plot -> Vector (Vector Plot) -> Int -> Int -> Int
countThingP2 thing p i j =
  Data.List.length
    $   Data.List.filter id
    $   checkDirection thing p (i, j)
    <$> [(1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, -1), (-1, 0), (-1, 1)]


inBounds :: Vector (Vector Plot) -> Int -> Int -> Bool
inBounds p i j =
  i < Data.Vector.length p && 0 <= i && 0 <= j && j < Data.Vector.length (p ! 0)

checkDirection
  :: Plot -> Vector (Vector Plot) -> (Int, Int) -> (Int, Int) -> Bool
checkDirection thing p (i, j) (di, dj) = if isThing thing p (i + di, j + dj)
  then True
  else if isThing Floor p (i + di, j + dj) && inBounds p i j
    then checkDirection thing p (i + di, j + dj) (di, dj)
    else False

incrementPlot :: Int -> Vector (Vector Plot) -> Int -> Int -> Plot
incrementPlot limit p i j =
  if isThing EmptyPlot p (i, j) && countThingP2 Occupied p i j == 0
    then Occupied
    else if isThing Occupied p (i, j) && countThingP2 Occupied p i j >= limit
      then EmptyPlot
      else (p ! i) ! j

mapPlots :: Int -> Vector (Vector Plot) -> Vector (Vector Plot)
mapPlots limit p =
  (\(i, row) ->
      (\(j, _) -> incrementPlot limit p i j)
        <$> Data.Vector.zip (fromList [0 .. Data.Vector.length row - 1]) row
    )
    <$> Data.Vector.zip (fromList [0 .. Data.Vector.length p - 1]) p

hammer :: Eq a => (a -> a) -> a -> a
hammer f x | x' == x   = x'
           | otherwise = hammer f x'
  where x' = f x

part_1 :: String -> String
part_1 s =
  let grid = parseRow <$> fromList (lines s)
  in  show $ numOccupied $ hammer (mapPlots 4) grid

part_2 :: String -> String
part_2 s =
  let grid = parseRow <$> fromList (lines s)
  in  show $ numOccupied $ hammer (mapPlots 5) grid
