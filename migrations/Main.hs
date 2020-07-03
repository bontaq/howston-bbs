{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
module Main where

import GHC.Generics
import GHC.Records
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits (Symbol)

import Text.Trifecta.Parser
import Text.Trifecta.Result
import Text.Parser.Char
import Text.Parser.Combinators
import System.Directory
import Data.List
import Data.String (fromString)
import Control.Monad
import Database.PostgreSQL.Simple
import Text.RawString.QQ (r)

instance forall x r a. HasField x r a => IsLabel x (r -> a) where
  fromLabel r = getField @x r

type Up = String
type Down = String

filterDirs :: [FilePath] -> [FilePath]
filterDirs = filter (\s -> not $ ".hs" `isSuffixOf` s)

data Migration = Migration
  { number :: String
  , name :: String
  } deriving (Show, Eq)

instance Ord Migration where
  a `compare` b = (number a) `compare` (number b)

toMigration :: FilePath -> Migration
toMigration string = Migration
  { number = takeWhile (not . (== '_')) string
  , name = string
  }

toMigrations :: [FilePath] -> [Migration]
toMigrations = sort . fmap toMigration

toLocations :: [FilePath] -> [FilePath]
toLocations = fmap (\s -> "./migrations/" <> s)

getSQLStatements :: Parser (Up, Down)
getSQLStatements = do
  _    <- manyTill anyChar (try $ string "> up")
  up   <- manyTill anyChar (try $ string "< down")
  down <- manyTill anyChar eof
  pure (up, down)

parseMigrations :: [String]-> Result [(Up, Down)]
parseMigrations migrations =
      sequence $ fmap (parseString getSQLStatements mempty) migrations

readMigration :: Migration -> IO String
readMigration migration = readFile (#name migration)

readMigrations :: [Migration] -> IO [String]
readMigrations = mapM readMigration

data MigrationRow = MigrationRow {
  id :: Integer
  , name :: String
  , ran :: Bool
  } deriving (Show, Generic, FromRow)

setupMigrationTable :: Connection -> IO ()
setupMigrationTable conn = do
  execute_ conn [r|
                  CREATE TABLE IF NOT EXISTS migrations (
                    id SERIAL PRIMARY KEY,
                    name VARCHAR (200) UNIQUE NOT NULL,
                    ran BOOLEAN
                  );
                  |]
  pure ()

testInsert :: Connection -> IO ()
testInsert conn = do
  execute
    conn
    "INSERT INTO migrations (name, ran) VALUES (?, ?)"
    ("babbies first run" :: String, False :: Bool)
  pure ()

getAllMigrations :: Connection -> IO [MigrationRow]
getAllMigrations conn =
  query_ conn "SELECT * FROM migrations"

findNewMigrations :: [MigrationRow] -> [(Migration, (Up, Down))] -> [(Migration, (Up, Down))]
findNewMigrations existingMigrations allMigrations =
  let
    existingNames = fmap #name existingMigrations
  in
    filter (\(m, _) -> not $ (#name m) `elem` existingNames) allMigrations

insertNewMigrations :: Connection -> [(Migration, (Up, Down))] -> IO ()
insertNewMigrations conn migrations =
  let
    names = fmap (\(m, _) -> #name m) migrations
    toInsert = zip names (repeat False)
  in do
    executeMany
      conn
     "INSERT INTO migrations (name, ran) VALUES (?, ?)"
     (toInsert :: [(String, Bool)])
    pure ()

runMigration :: Connection -> [(Migration, (Up, Down))] -> MigrationRow -> IO ()
runMigration conn migrations toRun =
  let
    match = find (\(m, _) -> (#name toRun) == (#name m)) migrations
  in case match of
      Just match ->
        let
          (_, (up, _)) = match
          toUpdate = (\(m, _) -> #name m) match
        in do
          -- run migration!
          execute_ conn (fromString up)
          -- update it as ran
          execute
            conn
            "UPDATE migrations SET ran = True WHERE name = (?)"
            ([toUpdate])

          pure ()
      Nothing -> error $ "could not find migration: " <> show toRun

runMigrations :: Connection -> [(Migration, (Up, Down))] -> IO ()
runMigrations conn migrations = do
  toRun <- query_ conn "SELECT * FROM migrations WHERE ran = False" :: IO [MigrationRow]
  mapM_ (runMigration conn migrations) toRun

main = do
  -- collect migrations
  fileNames <- toLocations <$> listDirectory "./migrations"
  let migrations = toMigrations $ filterDirs fileNames

  -- read migrations
  readMigrations <- readMigrations migrations
  -- parse migrations
  let parsedMigrations = case parseMigrations readMigrations of
        Success a -> a
        Failure x -> error $ show x
      migrationsWithName = zip migrations parsedMigrations

  conn <- connectPostgreSQL "dbname=howston"

  setupMigrationTable conn

  existingMigrations <- getAllMigrations conn

  -- filter out existing migrations
  let newMigrations = findNewMigrations existingMigrations migrationsWithName

  -- insert new migrations
  insertNewMigrations conn newMigrations

  -- run migrations
  runMigrations conn migrationsWithName

  print "Success"
