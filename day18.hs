import           Control.Monad                  ( replicateM
                                                , foldM
                                                )
import           Data.Maybe                     ( fromJust )

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
                                                -- , filter
                                                , map
                                                , toList
                                                )
import           Data.List                      ( uncons )

data Token = Times | Plus | LParen | RParen | Integer Int deriving Show
data Rule = MultiplyRule | AddRule | ParenRule | NoRule deriving Show

tokenize :: Char -> Token
tokenize c = case c of
  '(' -> LParen
  ')' -> RParen
  '+' -> Plus
  '*' -> Times
  _   -> Integer $ read [c]

main :: IO ()
main = interact part_1

part_1 :: String -> String
part_1 = parseInput

intAdd :: Int -> Token -> Int
intAdd b (Integer a) = a + b

parseInput :: String -> String
parseInput s =
  show $ foldl' intAdd 0 (head . fst . parseLine . filter (/= ' ') <$> lines s)

parseLine :: String -> ([Token], [Token])
parseLine s = reduceStack [] (tokenize <$> s)

reduce :: [Token] -> [Token]
reduce ts = case ts of
  (RParen : Integer i : LParen : x) -> Integer i : x
  (Integer i : Times : Integer j : x) -> Integer (i * j) : x
  (Integer i : Plus : Integer j : x) -> Integer (i + j) : x
  _ -> ts

getRule :: [Token] -> Rule
getRule ts = case ts of
  (RParen : Integer i : LParen : x) -> ParenRule
  (Integer i : Times : Integer j : x) -> MultiplyRule
  (Integer i : Plus : Integer j : x) -> AddRule
  _ -> NoRule


reduceStack :: [Token] -> [Token] -> ([Token], [Token])
reduceStack tokens remaining = case uncons remaining of
  Nothing -> case getRule tokens of
    NoRule -> (tokens, remaining)
    _      -> reduceStack (reduce tokens) remaining
  Just (lookahead, tl) ->
    let shift_step  = reduceStack (lookahead : tokens) tl
        reduce_step = reduceStack (reduce tokens) remaining
    in  case lookahead of
          Plus -> case getRule tokens of
            MultiplyRule -> shift_step
            NoRule       -> shift_step
            _            -> reduce_step
          Times -> case getRule tokens of
            AddRule      -> reduce_step
            MultiplyRule -> reduce_step
            NoRule       -> shift_step
            ParenRule    -> reduce_step
          _ -> case getRule tokens of
            NoRule -> shift_step
            _      -> reduce_step
