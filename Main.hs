{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Data.Text.Lazy as D
import Data.Monoid (mconcat)
import Data.Char
import Prelude as P
import Text.Regex.Posix


main = scotty 4000 $ do
    middleware logStdoutDev

    get "/" $ do
        beam<- param "q" :: ActionM D.Text
        text $ D.pack( show $ processQuery (beam))


processQuery :: D.Text -> Int
processQuery x
  | D.unpack x =~ D.unpack "which of the following numbers is the largest:*" = P.maximum (convtToIntArray x)
  | D.unpack x=~ D.unpack "what is [0-9]* plus [0-9]*"  ::Bool = sum (convtToIntArray x)
  |otherwise = -1
  


convtToIntArray:: Text->[Int]
convtToIntArray x= convtStringArrToIntArr . P.filter (P.all isDigit) . 
	    P.map (D.unpack) . 
        P.map (D.filter (/= ',')) 
        $ D.words x

convtStringArrToIntArr::[String]->[Int]
convtStringArrToIntArr = P.map read 



