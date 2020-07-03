{-# LANGUAGE DeriveAnyClass #-}
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

import Database.PostgreSQL.Simple
import Polysemy
import Polysemy.Input

data User = User {
  name :: String
  , password :: String
  } deriving (Show, Generic, FromRow)

data Persist m a where
  GetUser :: String -> Persist m [User]
  SaveUser :: User -> Persist m ()

makeSem ''Persist

checkUser :: Member Persist r => String -> Sem r [User]
checkUser username = getUser username

runPersistAsPostgres :: Member (Embed IO) r
                       => Sem (Persist : r) a
                       -> Sem (Input Connection : r) a
runPersistAsPostgres = reinterpret $ \case
  GetUser username -> do
    conn <- input
    result <- embed
      (query conn
       "SELECT * FROM users WHERE username = ?" (Only username) :: IO [User])
    return result

runLoginUser :: String -> String -> IO [User]
runLoginUser username password = do
  conn <- connectPostgreSQL "dbname=howston"
  runM
    . runInputConst conn
    . runPersistAsPostgres
    $ checkUser username

runRegisterUser :: String -> String -> IO [User]
runRegisterUser username password = do
  conn <- connectPostgreSQL "dbname=howston"
  runM
    . runInputConst conn
    . runPersistAsPostgres
    $ checkUser username

main = do
  putStrLn "Firing up server"

  scotty 3000 $ do
    post "/login" $ do
      loginRequest <- jsonData :: ActionM Lib.LoginRequest
      liftIO $ putStrLn $ "Login: " <> show loginRequest
      liftIO $ putStrLn . show =<< runLoginUser "" ""
      json loginRequest

    post "/register" $ do
      registerRequest <- jsonData :: ActionM Lib.LoginRequest
      liftIO $ putStrLn $ "Register: " <> show registerRequest
      liftIO $ putStrLn . show =<< runRegisterUser "" ""
      json registerRequest
