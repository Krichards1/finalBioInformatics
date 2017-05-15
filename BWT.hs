import Data.List
import Data.Function
import System.Environment
import Data.Matrix
import Data.Vector ((!))

--Funtions that help get the values out of the Tuples bigger than 2
getFst (a,_,_) = a
getSnd (_,a,_) = a
getTrd (_,_,a) = a

index' x lst = head (elemIndices (x) lst)

readF :: IO (String)
readF = do
  [s] <- getArgs
  file <- readFile s
  return file

query :: Int -> Int -> Int ->  String -> [Int] -> [Int] -> String -> (Matrix Int) -> String -> [Int]
query i top bottom letters suffA fstOcc lstCol cntMat patt
  | i >= 0 && top /= bottom = query (i-1) top1 bottom1 letters suffA fstOcc lstCol cntMat patt
  | otherwise = take (bottom+1) (drop top suffA)
  where
    l = index' (patt !! i) letters
    first = fstOcc !! l
    countA = getCol (l+1) cntMat
    top1 = first + (countA Data.Vector.! top)
    bottom1 = first + (countA Data.Vector.! (bottom) -1)

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


--Returns Suffix Array, First and Last Column of BWT
bwt :: String -> ([Int],String,String)
bwt s = ((map (\((a,_),_) -> a) fl),(map (\((_,b),_) -> b) fl),(map snd fl))
  where
    fl =  map (\((x1,x2),y) -> ((x1, last x2), y)) $ rots s
    rots s = sortBy (compare `on` \((a1,a2),b) -> a2) $ zip (zip [0..] (iterate rot s)) s
    rot (x:xs) = xs ++ [x]


main =  do
  file <- readF
  let genome  = concat $ lines file

  --SuffixArray = 0 , Last Column = 1 , First Column = 2
  let transform = bwt genome
  let suffixArray = getFst transform
  let lstCol = getSnd transform
  let fstCol = getTrd transform

  --letter = 0 , first Occurence = 1
  let lfstOcc = firstOccurence 1 "$" (tail fstCol) [0]
  let letters = fst lfstOcc
  let fstOcc = snd lfstOcc

  let countMat = getCountMatrix 2 (zero (length fstCol+1) (length letters)) letters lstCol

  let pattern = "GCTA"
  let initLetter = index' (last pattern) letters
  let res = query  ((length pattern)-2) (fstOcc !! initLetter) (fstOcc !! (initLetter+1)) letters suffixArray fstOcc lstCol countMat pattern

  print res
  print genome
  print suffixArray
  print fstOcc
  print fstCol
  print lstCol
  print countMat
