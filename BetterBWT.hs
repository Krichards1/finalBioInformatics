import Data.List
import Data.Function
import System.Environment
import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as C
import qualified Data.Set as Set
--A better BTW

index' lst x = head (elemIndices (x) lst)

sortVec :: V.Vector (Int, C.ByteString) -> V.Vector (Int, C.ByteString)
sortVec v = V.fromList (sortBy (compare `on` \(a1,a2) -> a2) $ V.toList v)

readF :: String -> IO C.ByteString
readF s = do
  file <- C.readFile s
  return file

splice lower upper lst =  take (upper-lower) (drop lower lst)

getTBMs :: Int -> Int -> String -> String -> (Matrix Int) -> Int -> [Int] -> (Int,Int,Int) -> [(Int,Int,Int)]
getTBMs first l letters lastCol countMat d firstOcc (top,bottom,miss) = filter (\(_,_,c) -> c <= d) (map (\(a,b,c) -> if a == rtop && b == rbottom then (a,b,c) else (a,b,c+1) ) tbms)
  where
    rtop = first + ((getCol (l+1) countMat) Data.Vector.! top)
    rbottom = first + ((getCol (l+1) countMat) Data.Vector.! bottom)
    tocheck = Set.toList(Set.fromList(map(index' letters) (splice top bottom lastCol)))
    tbms = map(\x -> ((firstOcc !! x) + ((getCol (x+1) countMat) Data.Vector.! top) , (firstOcc !! x) + ((getCol (x+1) countMat) Data.Vector.! bottom), miss)) tocheck

-- approxMatch :: Int -> Int -> [(Int,Int,Int)] -> String -> [Int] -> [Int] -> String -> (Matrix Int) -> String -> [Int]
-- approxMatch i d tbm letters suffixArray firstOccurence lastColumn countMat p
--   | i >= 0 && tbm /= [] = approxMatch (i-1) d tbm' letters suffixArray firstOccurence lastColumn countMat p
--   | otherwise = concat $ map(\(a,b,_) -> splice a b suffixArray) tbm
--   where
--     l = index' letters (p !! i)
--     first = firstOccurence !! l
--     tbm' = concat $ map (getTBMs first l letters lastColumn countMat d firstOccurence ) tbm

getTBMs :: C.ByteString -> C.ByteString -> Matrix Int -> Int -> V.Vector Int ->  V.Vector (Int,Int,Int)  ->  V.Vector (Int,Int,Int)
getTBMs letter lastColumn countMatrix d firstOccurence (top,bottom,miss) =
  where
    rtop = first + (getCol (l+1) countMatrix) V.! top
    rbottom = first + (getCol (l+1) countMatrix) V.! bottom




approxMatch :: Int -> V.Vector (Int,Int,Int) -> C.ByteString -> V.Vector Int -> V.Vector Int -> C.ByteString -> M.Matrix Int  -> C.ByteString -> V.Vector Int
approxMatch d tbm letters suffixArray firstOccurence lastColumn countMatrix pattern
  |

getCountMatrix :: C.ByteString -> C.ByteString -> M.Matrix Int
getCountMatrix letters lastColumn = M.transpose $ M.fromLists v
  where
    lenLet = C.length letters
    lenLast = C.length lastColumn
    v = [ V.toList $ V.scanl (+) 0 $ V.generate (lenLast) (\a -> if x == lastColumn `C.index` a then 1 else  0) | x <- C.unpack letters]

firstOccur :: C.ByteString -> (C.ByteString ,V.Vector Int)
firstOccur text = (letters, firstOcc)
  where
    sorted = C.sort text
    letters = C.pack $ map (C.head) $ groups
    groups  = C.group sorted
    firstOcc = V.postscanl (+) 0 $ V.map (\a -> C.length a) (V.fromList groups)


bwt :: C.ByteString -> (V.Vector Int, V.Vector C.ByteString)
bwt text = (fst last_suff, snd last_suff)
  where
    len = C.length text
    last_suff = V.unzip $ rots text
    rots s =  sortVec $ V.imap (\a b -> (a, b)) (V.iterateN (len) rot s)
    rot s = C.append (C.tail s) (C.pack $ [C.head s])

main = do
  file <- readF "test.txt"
  let genome = C.filter (/= '\r') . C.concat $ C.lines file

  let transform = bwt genome
  let suffixArray = fst transform
  let lastColumn = C.pack $ map (C.last) $ V.toList (snd transform)

  let letters_first = firstOccur lastColumn
  let letters = fst letters_first
  let first = snd letters_first

  let countMatrix = getCountMatrix letters lastColumn

  print (countMatrix)
