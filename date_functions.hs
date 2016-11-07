-- author: Nathan Egan

-- date is assumed to be a (Int, Int, Int) where (Year, Month, Day)
-- functions only garaunteed for valid dates
-- functions do not check for valid dates.  Assumes valid date input
-- a valid date is assumed to have:
--                      - positive year
--                      - month between 1 and 12
--                      - day no greater than 31 (depending on month)

import Data.List
import Data.Maybe
import Data.Char
import Data.Generics.Text

--returns true if second arg is older than first
isOlder :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
isOlder (y1, m1, d1) (y2, m2, d2) = (y1 <= y2 && m1 <= m2 && d1 < d2)

--returns the number of dates from the given list in the given month
numberInMonth :: [(Int, Int, Int)] -> Int -> Int
numberInMonth dateList month = foldl (\acc (y, m, d) -> if m == month then acc+1 else acc) 0 dateList

--returns the number of dates from the given list in the given months
numberInMonths :: [(Int, Int, Int)] -> [Int] -> Int
numberInMonths dateList monthList = foldl (\acc (y, m, d) -> if elem m monthList then acc+1 else acc) 0 dateList

--returns the dates from the given list of dates in the given month
datesInMonth :: [(Int, Int, Int)] -> Int -> [(Int, Int, Int)]
datesInMonth dateList month = filter (\(y, m, d) -> m==month) dateList

--returns the dates from the given list of dates in the given months
datesInMonths :: [(Int, Int, Int)] -> [Int] -> [(Int, Int, Int)]
datesInMonths dateList monthList = filter (\(y, m, d) -> elem m monthList) dateList

--returns the nth string in a list of strings
--nonzero based indexing.  1 should return 1st string, 2 shoud return second, ...
getNth :: [String] -> Int -> String
getNth stringList n = stringList !! (n-1)

--returns a string form of given date (ex: October 5, 2015)
dateToString :: (Int, Int, Int) -> String
dateToString (y1, m1, d1) = let
                                months = ["January", "Febraury", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
                                in
                                (getNth months m1) ++ " " ++ show d1 ++ ", " ++ show y1

--returns the count of items 1 before the sum of those exceed the given sum limit
numberBeforeReachingSum :: Int -> [Int] -> Int
numberBeforeReachingSum sum1 intList = foldl (\n i -> if (sum (take n intList)) >= sum1 then n else n+1) 0 intList

--returns int of month that day input (1-365) is in
whatMonth :: Int -> Int
whatMonth day = let
                    months = [31,28,31,30,31,30,31,31,30,31,30,31]
                    in
                    numberBeforeReachingSum day months

--returns list of months that are in the day range provided
monthRange :: Int -> Int -> [Int]
monthRange dayS dayE = let
                        days = [dayS..dayE]
                        in
                        map (\s -> whatMonth s) days

--returns oldest date from a list of dates
oldest :: [(Int, Int, Int)] -> Maybe (Int, Int, Int)
oldest [] = Nothing
oldest dateList = Just (minimum dateList)

--NOT FINISHED
--returns true if date is valid, false otherwise
validDate :: (Int, Int, Int) -> Bool
validDate (y, m, d) = if (y > 0 && m > 0 && m <= 12 || ((y `mod` 400) == 0 || (y `mod` 4) == 0) || (y `mod` 100) /= 0) then True else False
