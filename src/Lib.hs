{-# LANGUAGE DeriveGeneric #-}
module Lib (LoginRequest(..) ) where

import GHC.Generics
import qualified Data.Text as T
import Data.Aeson (FromJSON, ToJSON)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data LoginRequest = LoginRequest {
  username :: T.Text
  , password :: T.Text
  } deriving (Show, Generic)

instance ToJSON LoginRequest
instance FromJSON LoginRequest
