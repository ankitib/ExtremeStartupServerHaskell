{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Data.Text.Lazy as D
import Data.Monoid (mconcat)
import Data.Char
import Prelude as P
import Text.Regex.Posix
import Math.NumberTheory.Powers

main = scotty 4000 $ do
    middleware logStdoutDev

    get "/" $ do
        beam<- param "q" :: ActionM D.Text
        text $ processQuery (beam)


processQuery :: D.Text -> Text
processQuery x
  | D.unpack x =~ D.unpack "which of the following numbers is the largest:*" = intToText $ P.maximum (convtToIntArray x)
  | D.unpack x=~ D.unpack "what is [0-9]* plus [0-9]*"  ::Bool = intToText $ sum (convtToIntArray x)
  | D.unpack x=~ D.unpack "what is [0-9]* multiplied by [0-9]*"  ::Bool = intToText $ product (convtToIntArray x)
  | D.unpack x=~ D.unpack "which of the following numbers is both a square and a cube: [0-9]*"  ::Bool = intArrToText $ findSqRootCubeRoot (convtToIntArray x)
  | D.unpack x=~ D.unpack "which of the following numbers are primes: [0-9]*"  ::Bool = intArrToText $ findPrime (convtToIntArray x)
  | D.unpack x=~ D.unpack "which city is the Eiffel tower in"  ::Bool =  D.pack "Paris"
  | D.unpack x=~ D.unpack "who played James Bond in the film Dr No"  ::Bool =  D.pack "Sean Connery"
  | D.unpack x=~ D.unpack "who is the Prime Minister of Great Britain"  ::Bool =  D.pack "David Cameron"
  | D.unpack x=~ D.unpack "what colour is a banana"  ::Bool =  D.pack "Yellow"
  |otherwise = D.pack "-1"
  
intToText::Int->Text
intToText x= D.pack $ show $x 

intArrToText::[Int]->Text
intArrToText x= replace (pack ",") (pack ", ") $ D.pack $P.init $P.tail $show x -- putting a space after comma

convtToIntArray:: Text->[Int]
convtToIntArray x= convtStringArrToIntArr . P.filter (P.all isDigit) . 
	    P.map (D.unpack) . 
        P.map (D.filter (/= ',')) 
        $ D.words x

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