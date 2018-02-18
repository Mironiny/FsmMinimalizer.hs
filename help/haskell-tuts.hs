import Data.List
import System.IO

main = do
    putStrLn "What is your name"
    name <- getLine
    putStrLn ("Hello " ++ name)

addMe :: Int -> Int -> Int

addMe x y = x + y

factorial :: Int -> Int

factorial 0 = 1
factorial n = n * factorial (n - 1)

is0dd :: Int -> Bool

is0dd n
    | n `mod` 2 == 0 = False
    | otherwise = True

batAvgPlayers :: Double -> Double -> String

batAvgPlayers hits atBats
    | avg <= 0.200 = "Terrible Bating Average"
    | avg <= 0.250 =  "Average Player"
    | otherwise = "You are superstar"
    where
        avg = hits / atBats


getListItems :: [Int] -> String
getListItems [] = "Your list is empty"
getListItems (x:[]) = "Your list starts with " ++ show x
getListItems (x:y:[]) = "Your list starts with " ++ show x ++ " and " ++ show y
getListItems (x:xs) = "The 1st item is " ++ show x ++ "dd" ++ show xs


-- maxInt = maxBound :: Int

-- sumOfNums = sum [1..1000]
--
-- modEx = mod 5 4
--
-- neg = 5 + (-4)
--
-- num9 = 9 :: Int
-- sqrtOf9 = sqrt (fromIntegral num9)
--
-- primeNumbers = [3,5,7,11]
--
-- morePrime = primeNumbers ++ [13,17,19,23,29]
--
-- favNums = 2 : 7 : 21 : 66 :[]
--
-- multList = [[3,5,7], [11, 13, 17]]
--
-- morePrimes2 = 2 : morePrime
-- lenPrime = length(morePrimes2)
--
-- revPrime = reverse morePrimes2
--
-- isListEmpty = null []
--
-- secondPrime = morePrimes2 !! 1 --vyber konkrétní prvek z listu
--
-- firstPrime = head morePrimes2
--
-- lastPrime = last morePrimes2
--
-- first3Primes = take 3 morePrimes2
--
-- is7InList = elem 7 morePrimes2
--
-- zeroToTen = [0..10]
--
-- infinPow10 = [10,20..]
--
-- infiElem = infinPow10 !! 5
--
-- listTimes2 = [x * 3 | x <- [1..10], x * 3 <=50 ]
--
-- divisBy9N13 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]
--
-- sumOfList = zipWith (*) [1, 3, 5] [2, 3, 8]
--
-- filterF = filter (>5) [1,9,8,2,6,9,1,4]
--
-- pow3 = [3^n | n <- [1..10]]
--
-- randTuple = (1, "Random tuple ")
--
-- names = ["mirek", "marek", "tomas"]
-- phones = [608, 220, 666]
--
-- namesPhones = zip names phones
--
-- let num7 = 7
--
-- let getTriple x = 3 * x
