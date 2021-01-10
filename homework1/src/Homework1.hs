{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Homework1 where

import Data.List (find, sort,  nub, delete )
import GHC.Unicode ( isSpace, isDigit )
import Data.Maybe (isNothing,  fromJust, fromMaybe, listToMaybe )
import Control.Monad (join)
import System.Random
import Data.List.Split 

getNumber :: IO()
getNumber = do
        s <- getLine
        print $ f1_1 (read s :: Int)


f1_1 :: Int -> Int 
f1_1 i = i

f2_1 :: Int -> Bool
f2_1 n = abs n > 1 && null [ x | x <- [2 .. abs n - 1], abs n `mod` x == 0]

f3_1 :: Bool -> Bool -> Int
f3_1 False False = 0
f3_1 True True = 2
f3_1 _ _ = 1

f4_1 :: Int -> Int
f4_1 0 = 0
f4_1 1 = 0
f4_1 n = sum [x | x <- [1 .. abs n - 1], abs n `mod` x == 0]

f5_1 :: Int -> Int
f5_1 n = if f4_1 (n + 1) == n + 1 then n + 1
                        else f5_1 (n + 1)

f4_12 :: Integer -> Integer
f4_12 0 = 0
f4_12 1 = 0
f4_12 n = sum [x | x <- [1..(abs n - 1)], mod n x == 0]
                        
f6_1 :: Integer -> Integer
f6_1 n = if f4_12 (n + 1) == (n + 1) then n + 1
                                 else f6_1 (n + 1)                      

f7_1 :: Int -> Int -> Int 
f7_1 m n = if
        | m == 0          -> n + 1
        | m > 0 && n == 0 -> f7_1 (m - 1) 1
        | m > 0 && n > 0  -> f7_1 (m - 1) (f7_1 m (n - 1))
        | otherwise       -> error "ne nado tak"

f8_1 :: Int -> Int -> Integer
f8_1 n m = toInteger $ f7_1 m n

equalization :: Double -> Double -> Double -> Double -> Double -> Bool
equalization a b c d xi = a**(1/3) * xi + d**(1/3) * (a**(2/3) * xi * xi - (a * d)**(1/3) * xi + d**(2/3)) == -xi * (c + b*xi) 

f9_1 :: Double -> Double -> Double -> Double -> (Double, Double, Double)
f9_1 a b c d = do
        let f = equalization a b c d
        head [ (x1,x2,x3) | x1 <- [1..], f x1, x2 <- [x1..], f x2, x3 <- [x2..], f x3 ]

f10_0 :: (Double, Double) -> (Double, Double)
f10_0 (n, m) = (n + 1, m)
        
f10_1 :: Double -> Double -> (Double, Double)
f10_1 a b = if a < b then (0.0, a) 
                else f10_0 (f10_1 (a - b) b)

f11_0 :: Int -> Double -> Double
f11_0 k t = if k == 0 then 1 / (1.0 + t)
                else f11_0 (k - 1) (2 + (2.0 * fromIntegral k - 1)**2 / (2 + t))

f11_1 :: Int -> Double
f11_1 k = if k <= 0 then 0
                else 1 / f11_0 (k -1) (2 * fromIntegral k - 1)**2 / 2

f12_1 :: Int
f12_1 = 2
 
antisort :: Ord a => [a] -> [a]
antisort [x, y, z] = [maximum [x, y, z], minimum [x, y, z]] ++ delete (minimum [x, y, z]) (delete (maximum [x, y, z]) [x, y, z])
antisort xs = head xs : antisort (tail xs)

antiprimes :: Int -> [Integer]
antiprimes k = take k [toInteger x | x <- [1 .. ], not $ f2_1 x]

--antiunion :: Eq a => [a] -> [a] -> [a]
--antiunion l1 l2 = [x| x <- l1, (x `notElem` l2) ] ++ [x| x <- l2, (x `notElem` l1)]

antimerge :: Eq a => [a] -> [(Int, a)]
antimerge l = nub [ (length $ filter (== x) l, x) | x <- l ]

antiintercalate_0 :: Eq a => [a] -> [(Int, a)] -> [(Int, a)]
antiintercalate_0 list pairs = if
        | null list                     -> reverse pairs
        | head list == snd (head pairs) -> antiintercalate_0 (tail list) ((fst (head pairs) + 1, head list) : tail pairs)
        | otherwise                     -> antiintercalate_0 (tail list) ((1, head list) : pairs)

antiintercalate :: Eq a => [a] -> [(Int, a)]
antiintercalate list = antiintercalate_0 list [] 

antiantiintercalate :: [(Int, a)] -> [a]
antiantiintercalate [] = []
antiantiintercalate [(k, e)] = replicate k e
antiantiintercalate list = antiantiintercalate [head list] ++ antiantiintercalate (tail list)

readMaybe :: Int -> String -> Maybe Integer
readMaybe i str = if 
        |i == length str    -> Just (read str :: Integer) 
        |isDigit $ str !! i -> readMaybe (i + 1) str
        |otherwise          -> Nothing

getNumberOrNot :: String -> Maybe Integer
getNumberOrNot str = readMaybe 0 $ filter (not . isSpace) str :: Maybe Integer

maybeMaybeMaybeMaybeMaybeMaybeMaybeOrNot :: Maybe (Maybe (Maybe (Maybe (Maybe (Maybe a))))) -> a -> a
maybeMaybeMaybeMaybeMaybeMaybeMaybeOrNot m def = fromMaybe def (join (join (join (join (join m)))))

listToTuples :: [a] -> [(a, a, a, a)]
listToTuples [a, b, c, d] = [(a, b, c, d)]
listToTuples _ = []

stupidTraverse0 :: [a] -> Maybe [(a, a, a, a)]
stupidTraverse0 [] = Just []
stupidTraverse0 xs = if 
        | length xs < 4  -> Nothing
        | length xs == 4 -> Just $ listToTuples xs
        | otherwise      -> case stupidTraverse0 (drop 4 xs) of
                                        Nothing -> Nothing
                                        Just v  -> Just (fromJust (stupidTraverse0 (take 4 xs)) ++ v)

stupidTraverse :: [Maybe a] -> Maybe [(a, a, a, a)]
stupidTraverse xs = stupidTraverse0 (map fromJust (filter (not . null) xs))

dfs0 :: [(Int, Int)] -> Int -> Int -> [Int] -> Bool
dfs0 edges from to visited = from == to 
        ||
            any
                (\dest -> dfs0 edges (snd dest) to (from : visited))
                    (filter 
                    (not . (`elem` visited) . snd) (filter ((== from) . fst) edges))

dfs :: [(Int, Int)] -> Int -> Int -> Bool
dfs edges from to = dfs0 edges from to []

scnd :: (a, b, c) -> b
scnd (_, b, _) = b

frst :: (a, b, c) -> a
frst (a, _, _) = a

trd :: (a, b, c) -> c
trd (_, _, c) = c

fatWay0 :: (Num a, Ord a) => [(Int, Int, a)] -> Int -> Int -> [Int] -> [a]
fatWay0 edges from to visited = if from == to then 
                                [0]
                        else
                            sort 
                                (concatMap
                                    (\dest -> map (\l -> l + trd dest) (fatWay0 edges (scnd dest) to (from : visited)))
                                    (filter (not . (`elem` visited) . scnd) (filter ((== from) . frst) edges)))

fatWay :: (Num a, Ord a) => [(Int, Int, a)] -> Int -> Int -> a
fatWay edges from to = case listToMaybe (take 1 (drop 1 (fatWay0 edges from to []))) of
    Nothing -> fromInteger (negate 1)
    Just val -> val

data Tree a = EmptyTree
                | TreeValue a (Tree a) (Tree a) (Tree a)
                deriving Show
             
seek :: Ord a => a -> Tree a -> Maybe a
seek _ EmptyTree = Nothing
seek val (TreeValue a left center right) = case compare val a of
    EQ -> Just a
    GT -> seek val right
    LT -> let valueL = seek val left 
              valueC = seek val center
          in case (valueL, valueC) of
                    (Just al, _) -> Just al
                    (_, Just ac) -> Just ac
                    (_, _)       -> Nothing

deleteTree :: Ord a => a -> Tree a -> Tree a
deleteTree _ EmptyTree = EmptyTree
deleteTree val (TreeValue a left center right) = case compare val a of
    EQ -> TreeValue a (merge left center right) EmptyTree EmptyTree
    GT -> TreeValue a left center (deleteTree val right)
    LT -> TreeValue a (deleteTree val (mergeTwoTrees left center)) EmptyTree right

addTree :: Ord a => a -> Tree a -> Tree a
addTree a = mergeTwoTrees (TreeValue a EmptyTree EmptyTree EmptyTree)

split :: Ord a => a -> Tree a -> (Tree a, Tree a)
split _ EmptyTree = (EmptyTree, EmptyTree)
split val (TreeValue a left center right) = case compare val a of
    GT  -> let (lt,rt) = Homework1.split val right in (TreeValue a left center lt, rt)
    EQ -> (TreeValue a left center EmptyTree, right)
    LT  -> let (lt, rt) = Homework1.split val $ mergeTwoTrees left center in (lt, TreeValue a rt EmptyTree right)

mergeTwoTrees:: Ord a => Tree a -> Tree a -> Tree a
mergeTwoTrees tree EmptyTree = tree 
mergeTwoTrees EmptyTree tree = tree
mergeTwoTrees (TreeValue a1 left1 center1 right1) tree2 = 
    let (lt, rt) = Homework1.split a1 tree2 in 
    TreeValue a1 (mergeTwoTrees (mergeTwoTrees left1 center1) lt) EmptyTree (mergeTwoTrees right1 rt)

merge :: Ord a => Tree a -> Tree a -> Tree a -> Tree a
merge tr1 tr2 = mergeTwoTrees (mergeTwoTrees tr1 tr2) 

instance Functor Tree where
    fmap _ EmptyTree = EmptyTree
    fmap f (TreeValue a left center right) =
        TreeValue (f a) (fmap f left) (fmap f center) (fmap f right)

data N = Z | S N 
            deriving Show

instance Eq N where
    (==) Z Z = True
    (==) (S _) Z = False
    (==) Z (S _) = False
    (==) (S x) (S y) = x == y
            
instance Ord N where
    compare Z Z = EQ
    compare (S _) Z = GT
    compare Z (S _) = LT
    compare (S x) (S y) = compare x y            

natSum :: N -> N -> N
natSum Z x = x
natSum x Z = x
natSum x (S y) = S (natSum x y)
            
natMult :: N -> N -> N
natMult Z _ = Z
natMult _ Z = Z
natMult x (S y) = natSum x (natMult x y)

natPow :: N -> N -> N
natPow Z _ = Z
natPow _ Z = S Z
natPow x (S y) = natMult x (natPow x y)

superNat :: N -> N -> N -> N
superNat x y Z = natSum x y
superNat x y (S Z) = natMult x y
superNat x y (S (S Z)) = natPow x y
superNat x (S b) (S n)= natMult x $ natPow x (natSum b n)

--superNat Z x y = natSum x y
--superNat (S Z) x y = natMult x y
--superNat (S (S Z)) x y = natPow x y
--superNat _ _ _= error "нет такого"

--superNat op x y = if
--    | op == Z       -> natSum x y
--    | op == S Z     -> natMult x y
--    | op == S (S Z) -> natPow x y
--    | otherwise -> error "ne nado tak"

readInteger :: IO Integer
readInteger = readLn

readInt :: IO Int
readInt = readLn

apb :: IO()
apb = do
    a <- readInteger
    b <- readInteger
    print (a + b)

whileNotZero :: IO()
whileNotZero = do 
    a <- whileNotZero0 0
    print a

whileNotZero0 :: Integer -> IO Integer
whileNotZero0 summ = do
    a <- readInteger 
    if a == 0 then return summ else whileNotZero0 (summ + a)

fakeRandom :: IO()
fakeRandom = do
    seed <- readInt
    k <- readInt
    print (take k $ randoms (mkStdGen seed) :: [Float])

data FS = File String String
        | Directory String [FS]
        deriving Show

getName :: FS -> String
getName (File name _) = name
getName (Directory name _) = name
        
getChildrenNames :: FS -> String
getChildrenNames (File _ _) = "" 
getChildrenNames (Directory _ children) = concatMap (\x -> getName x ++ "\n") children
        
fsTree :: FS -> String -> String
fsTree (File name _) prefix = prefix ++ " " ++ name ++ "\n"
fsTree (Directory name children) prefix = prefix ++ "> " ++ name ++ "\n" ++ concatMap (\child -> fsTree child ('|' : prefix)) children
        
fsFind :: FS -> String -> Maybe FS
fsFind (File _ _) _ = Nothing
fsFind (Directory _ children) req = find (\child -> getName child == req) children

findDir :: FS -> String -> Maybe FS
findDir (File _ _) _ = Nothing
findDir (Directory name children) findName = if name == findName 
                            then Just $ Directory name children 
                            else find (\case
                                        File _ _ -> False
                                        Directory cildName _ -> (cildName == findName)) children

fsMakeDir :: FS -> String -> FS
fsMakeDir (File _ _) _ = error "net"
fsMakeDir (Directory name children) newName = 
    Directory name (Directory newName [] : children)
                                        
fsMakeFile :: FS -> String -> String -> FS
fsMakeFile (File _ _) _ _ = error "net"
fsMakeFile (Directory name children) newName newContents =
    Directory name (File newName newContents : children)
 
fsSplitNameAndContent :: String -> (String, String)
fsSplitNameAndContent str = let result = splitOn " " str in (head result, result !! 1)
 
fsReplace :: FS -> FS -> FS
fsReplace (Directory name children) updatedSubDir =
    Directory name (map
        (\it -> case it of
            (File _ _) -> it
            (Directory name _) -> if name == getName updatedSubDir then updatedSubDir else it
        ) children)

runFS :: FS -> IO ()
runFS fs = do
    runFS0 [] fs
    return ()    

runFS0 :: [FS] -> FS -> IO ()
runFS0 parents fs = do 
    comand <- getLine
    case comand of
        "exit" -> return ()
        
        "ls" -> do
            putStrLn (getChildrenNames fs)
            runFS0 parents fs
        
        "tree" -> do
            putStrLn (fsTree fs "")
            runFS0 parents fs
        
        ('c':'d':' ':dirName) ->
            if dirName == ".." then
                runFS0 (tail parents) (fsReplace (head parents) fs)
            else
                case findDir fs dirName of
                    Nothing -> do
                        putStrLn ("Directory " ++ dirName ++ " doesn't exist")
                        runFS0 parents fs
                    Just dir -> runFS0 (fs : parents) dir
        
        ('m':'k':'d':'i':'r':' ': dirName) ->
            if isNothing (fsFind fs dirName) then
                runFS0 parents (fsMakeDir fs dirName)
            else do
                putStrLn ("Directory " ++ dirName ++ " already exists")
                runFS0 parents fs
        
        ('t':'o':'u':'c':'h':' ': fileNameAndContent) ->
            if isNothing (fsFind fs (fst (fsSplitNameAndContent fileNameAndContent))) then
                runFS0 parents $ uncurry (fsMakeFile fs) (fsSplitNameAndContent fileNameAndContent) 
            else do
                putStrLn ("File " ++ fst (fsSplitNameAndContent fileNameAndContent) ++ " already exists")
                runFS0 parents fs
        
        unknown -> do
            putStrLn ("command " ++ unknown ++ " doesn't exist")
            runFS0 parents fs

