{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import GHC.Generics
import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import Control.Monad.IO.Class (liftIO)


data LoginRequest = LoginRequest {
  username :: String
  , password :: String
  } deriving (Show, Generic)

instance FromJSON LoginRequest
instance ToJSON LoginRequest

main = do
  putStrLn "Firing up server"

  scotty 3000 $ do
    post "/login" $ do
      loginRequest <- jsonData :: ActionM LoginRequest
      liftIO $ putStrLn $ show loginRequest
      json loginRequest
