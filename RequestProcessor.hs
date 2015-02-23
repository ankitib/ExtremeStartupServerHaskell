{-# LANGUAGE OverloadedStrings #-}

module RequestProcessor (
 processQuery
   )
where

import Data.Text.Lazy as D
import Data.Monoid (mconcat)
import Data.Char as C
import Prelude as P
import Text.Regex.Posix
import Math.NumberTheory.Powers
import Data.Map as Map
import Data.List as L
default (String)

processQuery :: String -> Text
processQuery x
  | x =~ D.unpack "which of the following numbers is the largest:*" = intToText $ P.maximum (convtToIntArray x)
  | x =~ D.unpack "what is [0-9]* multiplied by [0-9]* plus [0-9]*"  ::Bool = intToText $ multiplyPlus $ convtToIntArray x
  | x =~ D.unpack "what is [0-9]* plus [0-9]* multiplied by [0-9]*"  ::Bool = intToText $ plusMultiply $ convtToIntArray x
  | x =~ D.unpack "what is [0-9]* plus [0-9]*"  ::Bool = intToText $ sum (convtToIntArray x)
  | x =~ D.unpack "what is [0-9]* multiplied by [0-9]*"  ::Bool = intToText $ product (convtToIntArray x)
  | x =~ D.unpack "which of the following numbers is both a square and a cube: [0-9]*"  ::Bool = intArrToText $ findSqRootCubeRoot (convtToIntArray x)
  | x =~ D.unpack "which of the following numbers are primes: [0-9]*"  ::Bool = intArrToText $ findPrime (convtToIntArray x)
  | x =~ D.unpack "what is [0-9]* minus [0-9]*"  ::Bool = intToText $ minus (convtToIntArray x)
  | x =~ D.unpack "what is the [0-9]*th number in the Fibonacci sequence"  ::Bool = intToText $ fibonacci $ getNumberEndingWithTH x
  | x =~ D.unpack "what is [0-9]* to the power of [0-9]*"  ::Bool = integerToText $ calPower $ convtToIntArray x
  | x =~ D.unpack "what is the english scrabble score of"  ::Bool = intToText $ getScrabbleSum $ P.last $ P.words x  
  | x =~ D.unpack "which of the following is an anagram of \"(.*)\": (.*)" ::Bool = D.pack $ findAnagram $ getAnagramMatch x  
  | x =~ D.unpack "which city is the Eiffel tower in"  ::Bool =  D.pack "Paris"
  | x =~ D.unpack "who played James Bond in the film Dr No"  ::Bool =  D.pack "Sean Connery"
  | x =~ D.unpack "who is the Prime Minister of Great Britain"  ::Bool =  D.pack "David Cameron"
  | x =~ D.unpack "what colour is a banana"  ::Bool =  D.pack "Yellow"
  | x =~ D.unpack "what currency did Spain use before the Euro"  ::Bool =  D.pack "peseta"
  |otherwise = D.pack "-1"
  
intToText::Int->Text
intToText x= D.pack $ show $x 

intArrToText::[Int]->Text
intArrToText x= replace (pack ",") (pack ", ") $ D.pack $P.init $P.tail $show x -- putting a space after comma

integerToText::Integer->Text
integerToText x= D.pack $ show $x 

convtToIntArray:: String->[Int]
convtToIntArray x= convtStringArrToIntArr . P.filter (P.all isDigit) . 
        P.map (P.filter (/= ',')) 
        $ P.words x

convtStringArrToIntArr::[String]->[Int]
convtStringArrToIntArr = P.map read 

findSqRootCubeRoot::[Int]->[Int]
findSqRootCubeRoot x=P.filter (\y -> isCube y && isSquare y) $ x

isPrime::Int->Bool
isPrime x 
  |x == 1 = False
  |otherwise = P.null (P.filter (\y ->  x `mod`y == 0) (P.takeWhile (\y ->  y*y <= x) [2..]))


findPrime::[Int]->[Int]
findPrime x= P.filter isPrime x

minus::[Int]->Int
minus x= P.head x - (sum $ P.tail x)

calPower::[Int]->Integer
calPower x= (fromIntegral $P.head x) ^ ((fromIntegral $ P.last x)::Int)

fibonacci ::Int -> Int
fibonacci x
	|x<=0=0
	|x==1=1
	|otherwise = (fibonacci $x-2) + (fibonacci $x-1)

getNumberEndingWithTH::String->Int
getNumberEndingWithTH x= read (((  x =~ D.unpack " ([0-9]+)th* " ::[[String]]) !! 0) !! 1) :: Int

multiplyPlus::[Int]->Int
multiplyPlus x= ((x!!0) * (x!!1)) + (x!!2)

plusMultiply::[Int]->Int
plusMultiply x=(x!!0) + (x!!1) * (x!!2)

scrabbleMap=Map.fromList [('a',1),('b',3),('c',3),('d',2),('e',1),('f',4),('g',2),('h',4),('i',1),('j',8),('k',5),('l',1),('m',3),('n',1),('o',1),('p',3),('q',10),('r',1),('s',1),('t',1),('u',1),('v',4),('w',4),('x',8),('y',4),('z',10)]

getScrabbleSum::String->Int
getScrabbleSum x=sum $ fmap (\y->scrabbleMap ! C.toLower y) x 

getAnagramMatch::String->[String]
getAnagramMatch x=(x =~ D.unpack " which of the following is an anagram of \"(.*)\": (.*)" ::[[String]]) !! 0

findAnagram::[String]->String
findAnagram x=P.head $ P.filter (\y ->  (L.sort y) == (L.sort word)) optionList
          where word=x!!1 
                optionList = P.map (P.filter (/= ',')) $ P.words (x!!2)