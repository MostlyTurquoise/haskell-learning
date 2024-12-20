module Main where
import Data.Char

binSearch :: [Int] -> Int -> Int
binSearch list target = 
    let midpoint = length list `div` 2 in
    if list !! midpoint == target then midpoint else
    if list !! midpoint > target
    then binSearch (take midpoint list) target 
    else binSearch (drop midpoint list) target

-- Merge Sort

merge :: [Int] -> [Int] -> [Int] -> [Int]
merge l1 [] lo = reverse lo ++ l1
merge [] l2 lo = reverse lo ++ l2
merge l1 l2 lo = 
    if head l1 < head l2 
    then
        merge (tail l1) l2 (head l1 : lo)
    else
        merge l1 (tail l2) (head l2 : lo)

mergeSort :: [Int] -> [Int]
mergeSort li 
    | length li > 1 = 
        let midpoint = length li `div` 2 
        in merge (mergeSort (take midpoint li)) (mergeSort (drop midpoint li)) []
    | otherwise = li

-- ...

sortAndFind :: [Int] -> Int -> ([Int], Int)
sortAndFind li t = let sli = mergeSort li in (sli, binSearch sli t)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

mylast :: [a] -> a
mylast li = reverse li !! 0


withoutMultiplesOf :: [Int] -> Int -> [Int]
withoutMultiplesOf [] _ = []
withoutMultiplesOf (v:li) m | (v `mod` m) == 0 = li `withoutMultiplesOf` m
                            | otherwise = v : li `withoutMultiplesOf` m

sieveOfErastothenes :: [Int] -> [Int]
sieveOfErastothenes [] = []
sieveOfErastothenes (v:li) | v == 1 = v : sieveOfErastothenes li
                           | otherwise = v : sieveOfErastothenes (li `withoutMultiplesOf` v)

primesUpTo :: Int -> [Int]
primesUpTo n = sieveOfErastothenes [2..n]

caesarCipher :: [Char] -> Int -> [Char]
caesarCipher [] _ = []
caesarCipher (c:st) v
    | ord (toUpper c) - ord 'A' >= 0 && ord (toUpper c) - ord 'A' < 26 = chr (((ord (toUpper c) - ord 'A' + v) `mod` 26) + ord 'A') : caesarCipher st v
    | otherwise = c : caesarCipher st v

caesarCipherL :: [Char] -> Int -> [Char]
caesarCipherL st v = [chr (((ord (toUpper x) - ord 'A' + v) `mod` 26) + ord 'A') | x <- st]

lsum :: Num a => [a] -> a
lsum [] = 0;
lsum (f:l) = f + lsum l

isPalindrome :: [Char] -> [Char] -> Bool
isPalindrome [] _ = False
isPalindrome (i:str) ot | length str + 1 == length ot = (i:str) == ot
                        | length str == length ot = str == ot
                        | otherwise = isPalindrome str (i:ot)




main :: IO ()
main = print (primesUpTo 20)
-- main = print (last (reverse [10, 8..]))

