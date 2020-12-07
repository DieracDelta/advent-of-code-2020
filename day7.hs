import           Data.Set
import           Data.List
import           Data.Map
import           Text.Read

main :: IO ()
main = interact part_2

part_1 :: String -> String
part_1 s =
  let maps =
          Data.List.foldr
              (\(key, value) acc -> Data.Map.insert key (snd <$> value) acc)
              Data.Map.empty
            $   parseLine
            <$> lines s
  in  show
        $   length
        .   Data.List.filter id
        $   lookupPhrase maps "shiny gold"
        <$> keys maps

part_2 :: String -> String
part_2 s =
  let maps =
          Data.List.foldr (\(key, value) acc -> Data.Map.insert key value acc)
                          Data.Map.empty
            $   parseLine
            <$> lines s
  in  show $ countPhrase maps "shiny gold"


countPhrase :: Map String [(Int, String)] -> String -> Int
countPhrase m s = case Data.Map.lookup s m of
  Nothing -> 1
  Just v ->
    (sum $ fst <$> v)
      + (sum $ zipWith (*) (fst <$> v) (countPhrase m . snd <$> v))

lookupPhrase :: Map String [String] -> String -> String -> Bool
lookupPhrase m searchTerm curKey = case Data.Map.lookup curKey m of
  Nothing -> False
  Just v ->
    Data.Set.member searchTerm (Data.Set.fromList v)
      || Data.List.foldr (\x acc -> acc || lookupPhrase m searchTerm x) False v

splitBy :: (Char -> Bool) -> [Char] -> [[Char]]
splitBy f s = case dropWhile f s of
  [] -> []
  s' -> w : splitBy f s'' where (w, s'') = break f s'

slice :: Int -> [a] -> Int -> [a]
slice start l end = (Data.List.take (end - start + 1) . Data.List.drop start) l

parseLine :: String -> (String, [(Int, String)])
parseLine s =
  let splitString = splitBy
        (\x -> Data.Set.member x (Data.Set.fromList [',', '.', ' ']))
        s
  in  let remainder = Data.List.drop 3 splitString
      in  let result =
                  (\x -> slice (x - 3) remainder (x - 1))
                    <$> Data.List.findIndices (\x -> x == "bags" || x == "bag")
                                              remainder
          in  ( unwords $ slice 0 splitString 1
              , (\x ->
                  ( case readMaybe (head x) of
                    Just y  -> y
                    Nothing -> 0
                  , unwords $ tail x
                  )
                )
                <$> result
              )
