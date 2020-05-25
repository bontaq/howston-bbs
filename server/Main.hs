{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import GHC.Generics
import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import Control.Monad.IO.Class (liftIO)
import qualified Lib

import Polysemy

data Persist m a where
  GetUser :: String -> Persist m ()
  SaveUser :: Lib.LoginRequest -> Persist m ()

makeSem ''Persist

checkUser :: Member Persist r => Sem r ()
checkUser = undefined

registerUser :: Member Persist r => Sem r ()
registerUser = undefined


main = do
  putStrLn "Firing up server"

  scotty 3000 $ do
    post "/login" $ do
      loginRequest <- jsonData :: ActionM Lib.LoginRequest
      liftIO $ putStrLn $ "Login: " <> show loginRequest
      json loginRequest

    post "/register" $ do
      registerRequest <- jsonData :: ActionM Lib.LoginRequest
      liftIO $ putStrLn $ "Register: " <> show registerRequest
      json registerRequest
