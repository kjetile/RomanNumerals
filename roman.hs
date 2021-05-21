-- Decimal to roman numeral converter
-- By Kjetil Eide 21.05.21.
--
-- Usage: roman n
--
-- Works by dividing n by base numbers in the list "numerals".
-- For example when converting number 123:
--
-- 123/100 = 1 C
-- 23/90 = 0
-- 23/50 = 0
-- 23/40 = 0
-- 23/10 = 2 XX
-- 3/9 = 0
-- 3/5 = 0
-- 3/4 = 0
-- 3/1 = 3 III
-- RESULT: CXXII

import Data.List
import Data.Maybe

numerals = [
    (1000,"M"),
    (900,"CM"),
    (500,"D"),
    (400,"CD"),
    (100,"C"),
    (90,"XC"),
    (50,"L"),
    (40,"XL"),
    (10,"X"),
    (9,"IX"),
    (5,"V"),
    (4,"IV"),
    (1,"I")]

roman n = snd (accumulateNumerals (n, "")) 

-- recursively accumulate numerals
-- rest: what is left of n to convert.
-- accumulatedNumerals: the numerals we have so far

accumulateNumerals (rest, accumulatedNumerals) 
  | (rest > 0) = accumulateNumeral (getNumeral rest) rest accumulatedNumerals 
  | otherwise = (0, accumulatedNumerals)

accumulateNumeral (base, numeral) rest accumulatedNumerals = 
   accumulateNumerals (rest - base, accumulatedNumerals ++ numeral)

getNumeral n = (fromMaybe (0, "") (findNumeralTuple n))

findNumeralTuple n = find(\(base, _) -> (div n base) > 0) numerals


