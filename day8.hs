import           Data.Set
import           Data.List

main :: IO ()
main = interact part_2

data Program = Program { ip :: Int, is :: [Instr], acc :: Int, state :: State} deriving Show
data Instr = Jmp Int | Nop Int | Acc Int deriving Show
data State = Executing | Terminated | Nonterminating deriving (Show, Eq)

parseLine :: String -> Instr
parseLine s =
  let [typ, val] = words s
      sign       = case head val of
        '+' -> 1
        _   -> -1
      value = read $ tail val
  in  case typ of
        "jmp" -> Jmp $ sign * value
        "acc" -> Acc $ sign * value
        _     -> Nop $ sign * value

incrementIP :: Int -> Program -> Program
incrementIP i p =
  let new_instr = ip p + i
  in  if new_instr >= length (is p)
        then p { state = Terminated }
        else p { ip = ip p + i }

addToAcc :: Int -> Program -> Program
addToAcc a p = p { acc = acc p + a }

getInstr :: Program -> Int -> Instr
getInstr p indx = is p !! indx

executeInstruction :: Set Int -> Program -> Program
executeInstruction seen p =
  let i  = ip p
      ss = Data.Set.insert i seen
  in  if Data.Set.member i seen || state p /= Executing
        then p
        else case getInstr p i of
          Nop _    -> executeInstruction ss (incrementIP 1 p)
          Acc v    -> executeInstruction ss (incrementIP 1 $ addToAcc v p)
          Jmp addr -> executeInstruction ss (incrementIP addr p)

swapInstr :: Int -> Program -> Program
swapInstr indx p =
  let new = case is p !! indx of
        Jmp x -> Nop x
        Nop x -> Jmp x
        a     -> a
      (s, _ : e) = Data.List.splitAt indx (is p)
  in  p { is = s ++ [new] ++ e }

part_1 :: String -> String
part_1 s =
  let prog = Program { ip    = 0
                     , is    = parseLine <$> lines s
                     , acc   = 0
                     , state = Executing
                     }
  in  show $ acc $ executeInstruction Data.Set.empty prog

part_2 :: String -> String
part_2 s =
  let prog = Program { ip    = 0
                     , is    = parseLine <$> lines s
                     , acc   = 0
                     , state = Executing
                     }
      len = (length . is $ prog) - 1
  in  show
        $   acc
        .   head
        .   Data.List.filter ((== Terminated) . state)
        $   executeInstruction Data.Set.empty
        <$> (zipWith swapInstr [0 .. len] $ replicate len prog)
