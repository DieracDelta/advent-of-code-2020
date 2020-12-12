import           Data.Maybe
-- import           Control.Lens
import           Data.List                      ( length
                                                , filter
                                                )
import           Data.Bifunctor                 ( bimap )

main :: IO ()
main = interact part_2

data Action = Move Direction Int | Turn RelDirection | Forward Int deriving Show

data RelDirection = LeftDir Int | RightDir Int deriving Show

data Direction = North | South | West | East deriving Show

data Loc = Loc {d :: Direction, x :: Int, y:: Int} deriving Show

part_1 :: String -> String
part_1 s =
  let result =
          foldl (\cur_loc action -> performAction cur_loc action) (Loc East 0 0)
            $   parseLine
            <$> lines s
  in  case result of
        Loc _ x y -> show $ abs x + abs y

part_2 :: String -> String
part_2 s =
  let result =
          foldl (\cur_loc action -> performActionWP cur_loc action)
                (Loc East 10 1, Loc East 0 0)
            $   parseLine
            <$> lines s
  in  case snd result of
        Loc _ x y -> show $ abs x + abs y



performAction :: Loc -> Action -> Loc
performAction l a = case a of
  Move d n -> case d of
    North -> l { y = (y l) + n }
    South -> l { y = (y l) - n }
    West  -> l { x = (x l) - n }
    East  -> l { x = (x l) + n }
  Turn    (LeftDir  n) -> l { d = apply turn_left n (d l) }
  Turn    (RightDir n) -> l { d = apply turn_right n (d l) }
  Forward n            -> performAction l $ Move (d l) n

performActionWP :: (Loc, Loc) -> Action -> (Loc, Loc)
performActionWP (wp, l) a = case a of
  Move d n -> case d of
    North -> (wp { y = (y wp) + n }, l)
    South -> (wp { y = (y wp) - n }, l)
    West  -> (wp { x = (x wp) - n }, l)
    East  -> (wp { x = (x wp) + n }, l)
  Turn    n -> (rotateLoc wp n, l)
  Forward n -> (wp, l { x = (x wp) * n + (x l), y = (y wp) * n + (y l) })

apply f n x = if n == 0 then x else apply f (n - 1) (f x)

turn_right :: Direction -> Direction
turn_right x = case x of
  North -> East
  East  -> South
  South -> West
  West  -> North

turn_left :: Direction -> Direction
turn_left x = case x of
  North -> West
  West  -> South
  South -> East
  East  -> North

parseLine :: String -> Action
parseLine s =
  let (act, param) = splitAt 1 s
      param_int    = read param
  in  case act of
        "N" -> Move North param_int
        "S" -> Move South param_int
        "W" -> Move West param_int
        "E" -> Move East param_int
        "F" -> Forward param_int
        "L" -> Turn $ LeftDir (param_int `div` 90)
        "R" -> Turn $ RightDir (param_int `div` 90)
        _   -> error "uhoh"

rotateLoc :: Loc -> RelDirection -> Loc
rotateLoc l d = case d of
  LeftDir i -> if i == 0
    then l
    else rotateLoc (l { y = (x l), x = -(y l) }) (LeftDir (i - 1))
  RightDir i -> rotateLoc l $ LeftDir (4 - i)
