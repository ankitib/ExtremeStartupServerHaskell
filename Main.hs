{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import RequestProcessor
import Control.Monad
import System.Environment
import Data.Text.Lazy as D

main = do
       port <- liftM read $ getEnv "PORT"
       scotty port $ do
          middleware logStdoutDev

          get "/" $ do
             beam<- param "q" :: ActionM D.Text
             text $ processQuery (beam)



                
