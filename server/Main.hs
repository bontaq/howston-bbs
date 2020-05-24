{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import GHC.Generics
import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)

data LoginRequest = LoginRequest {
  username :: String
  , password :: String
  } deriving (Show, Generic)

instance FromJSON LoginRequest

main = putStrLn "hello world"
