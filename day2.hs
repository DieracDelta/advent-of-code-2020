import           Data.Set                       ( fromList
                                                , member
                                                )
import           Data.List                      ( groupBy )

data Password = Password {numTimesMin :: Int, numTimesMax :: Int, character :: Char, password :: String} deriving Show

isValidPart2 :: Password -> Bool
isValidPart2 p = foldr
  (/=)
  False
  (fmap (\x -> password p !! x == character p)
        [numTimesMin p - 1, numTimesMax p - 1]
  )

isValidPart1 :: Password -> Bool
isValidPart1 p =
  let len = length $ Prelude.filter (\x -> x == character p) (password p)
  in  len >= numTimesMin p && len <= numTimesMax p

parseString :: String -> Password
parseString s =
  let delimiters = fromList ['-', ' ', ':']
  in 
    -- not my best work...
      let parsed = filter (\x -> not $ member (head x) delimiters) $ groupBy
            (\x c -> not (member x delimiters) && not (member c delimiters))
            s
      in  Password (read $ head parsed)
                   (read $ parsed !! 1)
                   (head $ parsed !! 2)
                   (parsed !! 3)

part_1 :: String -> String
part_1 s =
  let l = lines s
  in  show $ length $ [ x | x <- l, isValidPart2 $ parseString x ]

main :: IO ()
main = interact part_1
