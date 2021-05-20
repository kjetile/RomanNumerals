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

roman n = accumulateNumerals(n, "") 

accumulateNumerals (rest, romanNumeral) 
                    | (rest > 0) = getNumeral rest 
                    | otherwise = (0, "")

getNumeral n = (fromMaybe (0, "") (findNumeralTuple rest))


--foo (rest, numeral) = 


findNumeralTuple n = find(\(base, _) -> base > n) numerals


