import           Data.Char                      ( isDigit )
import           Data.List                      ( uncons )

import           Data.Bits
import           Data.Map                       ( Map
                                                , insert
                                                , empty
                                                , foldr
                                                )


data Mask = MaskP1 Int Int | MaskP2 [DBit] deriving Show

data Action = ChangeMask Mask | ChangeMemory Int Int deriving Show

data State = State { s_m :: Mask, s_mmap :: Map Int Int} deriving Show

parseLine :: (String -> Mask) -> String -> Action
parseLine s2m s =
  let ws           = words s
      parsed_value = ws !! 2
  in  case head ws of
        "mask" -> ChangeMask $ s2m parsed_value
        memaddr ->
          let parsed_mem_addr = read $ takeWhile isDigit $ drop 4 memaddr
          in  ChangeMemory parsed_mem_addr (read parsed_value)

applyAction :: State -> Action -> State
applyAction (State mask mmap) action = case action of
  ChangeMask mask_new   -> State mask_new mmap
  ChangeMemory addr val -> case mask of
    MaskP1 _ _ -> State mask $ insert addr (applyMask mask val) mmap
    MaskP2 bits ->
      let addrs = maskToAddrs bits addr
      in  State mask $ foldl (\acc x -> insert x val acc) mmap addrs

maskToAddrs :: [DBit] -> Int -> [Int]
maskToAddrs dbs addr = case uncons dbs of
  Nothing -> [0]
  Just (hd, tl) ->
    let i       = length dbs - 1
        results = maskToAddrs tl addr
    in  case hd of
          X    -> ((+ bit i) <$> results) ++ results
          One  -> (+ bit i) <$> results
          Zero -> (+ (addr .&. bit i)) <$> results

main :: IO ()
main = interact $ processInput (MaskP2 [])

processInput :: Mask -> String -> String
processInput (MaskP1 a b) s =
  let init_value = State (MaskP1 a b) empty
      state = foldl applyAction init_value (parseLine inputToMask <$> lines s)
  in  show $ Data.Map.foldr (+) 0 (s_mmap state)

processInput (MaskP2 dbs) s =
  let init_value = State (MaskP2 dbs) empty
      state =
          foldl applyAction init_value (parseLine inputToMaskP2 <$> lines s)
  in  show $ Data.Map.foldr mergeReps 0 (s_mmap state)

applyMask :: Mask -> Int -> Int
applyMask (MaskP1 orMask andMask) x = (x .|. orMask) .&. andMask

eleMask :: Char -> Int -> Mask -> Mask
eleMask c i (MaskP1 orMask andMask) = case c of
  'X' -> MaskP1 (clearBit orMask i) (setBit andMask i)
  '1' -> MaskP1 (setBit orMask i) (setBit andMask i)
  '0' -> MaskP1 (clearBit orMask i) (clearBit andMask i)

inputToMask :: String -> Mask
inputToMask s =
  foldl (flip (uncurry eleMask)) (MaskP1 0 0) $ zip s [35, 34 .. 0]

data DBit = X | One | Zero deriving Show

eleMaskP2 :: Char -> DBit
eleMaskP2 c = case c of
  'X' -> X
  '1' -> One
  '0' -> Zero

inputToMaskP2 :: String -> Mask
inputToMaskP2 s = MaskP2 $ eleMaskP2 <$> s
