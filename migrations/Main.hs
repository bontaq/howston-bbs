{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import GHC.Generics
import Text.Trifecta.Parser
import Text.Trifecta.Result
import Text.Parser.Char
import Text.Parser.Combinators
import System.Directory
import Data.List
import Control.Monad
import Database.PostgreSQL.Simple
import Text.RawString.QQ (r)

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

toAbsolutes :: [FilePath] -> IO [FilePath]
toAbsolutes = mapM (makeAbsolute . (\s -> "./migrations/" <> s))

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
readMigration migration = readFile (name (migration :: Migration))

readMigrations :: [Migration] -> IO [String]
readMigrations = mapM readMigration

data MigrationRow = MigrationRow {
  id :: String
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
    ("babbies second run" :: String, False :: Bool)
  pure ()

main = do
  -- collect migrations
  fileNames <- toAbsolutes =<< listDirectory "./migrations"
  let migrations = toMigrations $ filterDirs fileNames

  -- read migrations
  readMigrations <- readMigrations migrations

  -- parse migrations
  let parsedMigrations = parseMigrations readMigrations
  print parsedMigrations

  -- lookup already run migrations
  conn <- connectPostgreSQL "dbname=howston"
  setupMigrationTable conn

  testInsert conn

  r <- query_ conn "SELECT * FROM migrations" :: IO [MigrationRow]
  print r

  -- compare the two and run them

  pure $ show $ parsedMigrations
