import qualified Data.List                     as L
import qualified Data.Set                      as S
import qualified Data.Map                      as M
import qualified Data.Sort                     as DS

main :: IO ()
main = interact part_2

part_1 :: String -> String
part_1 s =
  let parsed_lines = parseLine <$> lines s
      overall_map  = L.foldl'
        (\acc_map (normals, weirds) -> addEles acc_map weirds normals)
        M.empty
        parsed_lines
      could_be_allergens = M.foldl' S.union S.empty overall_map
      all_allergens      = L.foldl' S.union S.empty (fst <$> parsed_lines)
      non_allergens      = all_allergens `S.difference` could_be_allergens
      -- ew...rewrite
      grand_total        = S.foldl'
        (\acc non_allergen -> acc + L.foldl'
          (\acc_inner ele -> if S.member non_allergen ele
            then (acc_inner + 1 :: Int)
            else acc_inner
          )
          0
          (fst <$> parsed_lines)
        )
        0
        non_allergens
  in  show grand_total

part_2 :: String -> String
part_2 s =
  let parsed_lines = parseLine <$> lines s
      overall_map  = L.foldl'
        (\acc_map (normals, weirds) -> addEles acc_map weirds normals)
        M.empty
        parsed_lines
      allergen_map = constructMap M.empty overall_map
  in  L.foldl' (\acc ele -> acc ++ "," ++ ele) "" $ snd <$> DS.sortOn
        fst
        (M.toList allergen_map)

constructMap
  :: M.Map String String -> M.Map String (S.Set String) -> M.Map String String
constructMap result_map allergen_map =
  let
    (discovered, rest) = M.partition (\v -> length v == 1) allergen_map
    new_result_map     = M.foldlWithKey'
      (\acc k val -> M.insert k (head $ S.toList val) acc)
      result_map
      discovered
    all_new_vals = L.foldl' S.union S.empty $ snd <$> M.toList discovered
    new_allergen_map =
      M.mapMaybe (\v -> Just $ v `S.difference` all_new_vals) rest
  in
    if M.size discovered == 0
      then result_map
      else constructMap new_result_map new_allergen_map

addEle
  :: M.Map String (S.Set String)
  -> String
  -> S.Set String
  -> M.Map String (S.Set String)
addEle mmap weird normals = M.insertWith S.intersection weird normals mmap

addEles
  :: M.Map String (S.Set String)
  -> S.Set String
  -> S.Set String
  -> M.Map String (S.Set String)
addEles mmap weirds normals =
  S.foldl' (\acc_map weird -> addEle acc_map weird normals) mmap weirds

parseLine :: String -> (S.Set String, S.Set String)
parseLine s =
  let (weird, normal) = span (/= '(') s
      normal_set =
          S.fromList $ words $ L.filter (/= ',') $ L.takeWhile (/= ')') $ L.drop
            9
            normal
      weird_set = S.fromList $ words weird
  in  (weird_set, normal_set)
