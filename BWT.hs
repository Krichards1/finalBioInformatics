import Data.List
import Data.Function
import System.Environment
import Data.Matrix
import Data.Vector ((!))
import qualified  Data.ByteString as Bs
import qualified Data.Set as Set

--Funtions that help get the values out of the Tuples bigger than 2
getFst (a,_,_) = a
getSnd (_,a,_) = a
getTrd (_,_,a) = a

index' lst x = head (elemIndices (x) lst)

readF :: String -> IO (String)
readF s = do
  file <- readFile s
  return file

splice lower upper lst =  take (upper-lower) (drop lower lst)

getTBMs :: Int -> Int -> String -> String -> (Matrix Int) -> Int -> [Int] -> (Int,Int,Int) -> [(Int,Int,Int)]
getTBMs first l letters lastCol countMat d firstOcc (top,bottom,miss) = filter (\(_,_,c) -> c <= d) (map (\(a,b,c) -> if a == rtop && b == rbottom then (a,b,c) else (a,b,c+1) ) tbms)
  where
    rtop = first + ((getCol (l+1) countMat) Data.Vector.! top)
    rbottom = first + ((getCol (l+1) countMat) Data.Vector.! bottom)
    tocheck = Set.toList(Set.fromList(map(index' letters) (splice top bottom lastCol)))
    tbms = map(\x -> ((firstOcc !! x) + ((getCol (x+1) countMat) Data.Vector.! top) , (firstOcc !! x) + ((getCol (x+1) countMat) Data.Vector.! bottom), miss)) tocheck

approxMatch :: Int -> Int -> [(Int,Int,Int)] -> String -> [Int] -> [Int] -> String -> (Matrix Int) -> String -> [Int]
approxMatch i d tbm letters suffixArray firstOccurence lastColumn countMat p
  | i >= 0 && tbm /= [] = approxMatch (i-1) d tbm' letters suffixArray firstOccurence lastColumn countMat p
  | otherwise = concat $ map(\(a,b,_) -> splice a b suffixArray) tbm
  where
    l = index' letters (p !! i)
    first = firstOccurence !! l
    tbm' = concat $ map (getTBMs first l letters lastColumn countMat d firstOccurence ) tbm

query :: Int -> Int -> Int ->  String -> [Int] -> [Int] -> String -> (Matrix Int) -> String -> [Int]
query i top bottom letters suffA fstOcc lstCol cntMat patt
  | i >= 0 && top /= bottom = query (i-1) top1 bottom1 letters suffA fstOcc lstCol cntMat patt
  | otherwise = splice top bottom suffA
  where
    l = index' letters (patt !! i)
    first = fstOcc !! l
    countA = getCol (l+1) cntMat
    top1 = first + (countA Data.Vector.! top)
    bottom1 = first + (countA Data.Vector.! (bottom))

getCountMatrix :: Int -> (Matrix Int) -> String -> String -> (Matrix Int)
getCountMatrix _ mat _ [] = mat
getCountMatrix i mat letters (x:xs) = getCountMatrix (i+1) (mapRow count i mat) letters xs
  where
    count a b =  if x == letters!!(a-1) then mat Data.Matrix.! (i-1,a)+1 else mat Data.Matrix.! (i-1,a)

firstOccurence :: Int -> String -> String -> [Int] -> (String,[Int])
firstOccurence i letters [] indices = (letters, indices++[i])
firstOccurence i letters (x:xs) indices
  | last letters /= x = firstOccurence (i+1) (letters++[x]) xs (indices++[i])
  | otherwise = firstOccurence (i+1) letters xs indices


bwt :: String -> Int -> ([(Int,Int)],String,String)
bwt s k = (partSuffArray,(map (\((_,b),_) -> b) fl),(map snd fl))
  where
    fl =  map (\((x1,x2),y) -> ((x1, last x2), y)) $ rots s
    partSuffArray = filter (\(_,a) -> a `mod` k == 0) $ zip [0..] (map (\((a,_),_) -> a) fl)
    rots s = sortBy (compare `on` \((a1,a2),b) -> a2) $ zip (zip [0..] (iterate rot s)) s
    rot (x:xs) = xs ++ [x]


main =  do
  file <- readF "test.txt"
  let genome  = concat $ lines file
  --SuffixArray = 0 , Last Column = 1 , First Column = 2
  let transform = bwt genome 150
  let suffixArray = getFst transform
  let lstCol = getSnd transform
  let fstCol = getTrd transform

  --letter = 0 , first Occurence = 1
  let lfstOcc = firstOccurence 1 "$" (tail fstCol) [0]
  let letters = fst lfstOcc
  let fstOcc = snd lfstOcc

  let countMat = getCountMatrix 2 (zero (length lstCol+1) (length letters)) letters lstCol
  print countMat
  -- let pattern = "ATT"
  -- let pattern' =  ((take $ length pattern -1) pattern)
  --
  -- let initLetter = index' letters (last pattern)
  -- let initLetter' = index' letters (last pattern')

  -- let res = approxMatch ((length pattern')-2) 0 [((fstOcc !! initLetter'),(fstOcc !! (initLetter'+1)),0)] letters suffixArray fstOcc lstCol countMat pattern'
  -- let res' = approxMatch ((length pattern)-2) 1 [((fstOcc !! initLetter),(fstOcc !! (initLetter+1)),0)] letters suffixArray fstOcc lstCol countMat  pattern

  -- print res'
  -- print res
  -- print genome
  -- print suffixArray
  -- print fstOcc
  -- print fstCol
  -- print lstCol
  -- print countMat
  --return (transform)
