module Main where
import Lib()
import System.Environment
import Text.Read
import Data.String
import Data.List
import Data.Tuple
import System.IO
import System.Random(randomRIO)
import Data.Maybe()

data Conf = Info {
    number :: Maybe Int,
    limit :: Maybe Int,
    file :: Maybe String
} deriving (Show)

defaultConf :: Conf
defaultConf = Info {
    number = Just 0,
    limit = Just 0,
    file = Nothing
}

data Pix = Pix {
    position :: (Int, Int),
    color :: (Int, Int, Int)
} deriving (Show)

data Centroid = Centroid {
    centrColor :: (Int, Int, Int),
    pixList :: [Pix]
} deriving (Show)

getMin :: String -> Int -> Maybe Int
getMin str mini = readMaybe str >>= \nb -> if nb >= mini then Just nb else Nothing

readNb :: String -> Int -> Int -> Maybe Int
readNb str min max = readMaybe str >>= \nb -> case min <= nb && nb <= max of
     True -> Just nb
     False -> Nothing

getPos :: String -> Maybe (Int, Int)
getPos str = readMaybe str :: Maybe (Int, Int)

getColor :: String -> Maybe (Int, Int, Int)
getColor str = readMaybe str :: Maybe (Int, Int, Int)

getOption :: Conf -> [String] -> Maybe Conf
getOption info [x] = Nothing
getOption info [] = Just info
getOption info (x:y:xs) = case x of
    "-n" -> getOption info {number = getMin y 0} xs
    "-l" -> getOption info {limit = getMin y 0} xs
    "-f" -> getOption info {file = Just y} xs
    _ -> Nothing

getNb :: Maybe Int -> Int
getNb Nothing = -84
getNb (Just x) = x

getStr :: Maybe String -> String
getStr Nothing = ""
getStr (Just x) = x

strToWord :: Maybe String -> IO [String]
strToWord Nothing = return []
strToWord (Just str) = do 
    string <- readFile str
    return (words string)

createPix :: Maybe (Int, Int) -> Maybe (Int, Int, Int) -> Pix
createPix (Just pos) (Just color) = Pix pos color

fillPixList :: [String] -> [Pix]
fillPixList [] = []
fillPixList (x:y:xs) = createPix (getPos x) (getColor y) : fillPixList xs
fillPixList _ = error "Wrong File"

getFrtCentroid :: Maybe Int -> [Pix] -> IO [(Int, Int, Int)]
getFrtCentroid (Just 0) _ = return []
getFrtCentroid (Just nb) list = do
    randNb <- randomRIO (0, length list - 1)
    let new = list !! randNb
    centroid <- getFrtCentroid (Just (nb - 1)) list
    return (color new : centroid)

distance :: (Int, Int, Int) -> (Int, Int, Int) -> Float
distance (x1, y1, z1) (x2, y2, z2) =
    sqrt (fromIntegral ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1) + (z2 - z1) * (z2 - z1)) :: Float)

minDistBy :: [Pix] -> Pix -> Pix -> Float -> Pix
minDistBy [] _ minP _ = minP
minDistBy (x:xs) centroid minP minD = let newD = distance (color x) (color centroid) in
    case newD < minD of
    True -> minDistBy xs centroid x (distance (color x) (color centroid))
    False -> minDistBy xs centroid minP minD

closest :: [(Int, Int, Int)] -> Pix -> (Int, Int, Int) -> Float -> (Int, Int, Int)
closest [] _ minC _ = minC
closest (x:xs) pix minC minD = let newD = distance x (color pix) in
    case newD < minD of
    True -> closest xs pix x (distance x (color pix))
    False -> closest xs pix minC minD

printClosest :: [(Int, Int, Int)] -> Pix -> IO ()
printClosest [] _ = return ()
printClosest (x:xs) pix =
    print $ closest xs pix x (distance x (color pix))

myAddTriple :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
myAddTriple (a, b, c) (a', b', c') = (a + a', b + b', c + c')

getSumK :: [Pix] -> (Int, Int, Int)
getSumK [] = (0, 0, 0)
getSumK (x:xs) = myAddTriple (color x) (getSumK xs)

getAverageK :: (Int, Int, Int) -> Int -> (Int, Int, Int)
getAverageK _ 0 = (0,0,0)
getAverageK (a, b, c) l = (a `div`l , b `div` l, c `div` l)

getNewK :: [[Pix]] -> [(Int, Int, Int)]
getNewK [] = []
getNewK (x:xs) = (getAverageK (getSumK x) (length x)) : getNewK xs

createTab :: Int -> [[Pix]]
createTab 0 = []
createTab nb = [] : createTab (nb - 1)

addTotab :: [[Pix]] ->  Int -> Maybe Int -> Pix -> [[Pix]]
addTotab [] _ _ _ = []
addTotab (x:xs) i (Just j) elem | i == j = (elem : x) : xs
                         | otherwise = x : addTotab xs (i + 1) (Just j) elem

getTabK :: [Pix] -> [(Int, Int, Int)] -> [[Pix]]
getTabK [] tab = createTab (length tab)
getTabK (x:xs) kList = let idx = elemIndex (closest kList x (head kList) (distance (head kList) (color x))) kList in
    (addTotab (getTabK xs kList) 0 idx x)

printPos :: (Int, Int) -> IO ()
printPos (a, b) = putStr ("(" ++ show a ++ "," ++ show b ++ ")")

printColor :: (Int, Int, Int) -> IO ()
printColor (a, b, c) = putStr ("(" ++ show a ++ "," ++ show b ++ "," ++ show c ++ ")")

printTabAlgo :: [Pix] -> IO ()
printTabAlgo [] = putStr ""
printTabAlgo (x:xs) = printPos (position x) >> putStr " " >> printColor (color x) >> putStrLn "" >> printTabAlgo xs

printAlgo :: [[Pix]] -> [(Int, Int, Int)] -> IO ()
printAlgo [] _ = putStr ""
printAlgo (x:xs) (y:ys) = putStrLn "--" >> putStrLn (show y) >> putStrLn "-" >> printTabAlgo x >> printAlgo xs ys

algo :: [Pix] -> [(Int, Int, Int)] -> Int -> IO ()
algo tabPix tabCentroid 0 = let tabK = getTabK tabPix tabCentroid in
    printAlgo tabK (getNewK tabK)
algo tabPix tabCentroid nb = algo tabPix (getNewK (getTabK tabPix tabCentroid)) (nb - 1)

main :: IO ()
main = getArgs >>= \args -> case getOption defaultConf args of
    Just info -> do
        tab <- strToWord (file info)
        tabCentroid <- getFrtCentroid (number info) (fillPixList tab)
        if null tab
            then putStrLn "Usage: ./imageCompressor -n x -l y -f pathToFile" >> return ()
        else algo (fillPixList tab) tabCentroid 10
    Nothing -> putStrLn "Usage: ./imageCompressor -n x -l y -f pathToFile"