{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import GHC.Generics
import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import Control.Monad.IO.Class (liftIO)
import qualified Lib


main = do
  putStrLn "Firing up server"

  scotty 3000 $ do
    post "/login" $ do
      loginRequest <- jsonData :: ActionM Lib.LoginRequest
      liftIO $ putStrLn $ show loginRequest
      json loginRequest
