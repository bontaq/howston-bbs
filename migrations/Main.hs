module Main where

import Text.Trifecta.Parser
import Text.Trifecta.Result
import Text.Parser.Char
import Text.Parser.Combinators
import System.Directory
import Data.List
import Control.Monad

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
  _ <- string "> up"
  up <- manyTill anyChar (try $ string "< down")
  down <- manyTill anyChar eof
  pure (up, down)

parseMigrations :: [String]-> Result [(Up, Down)]
parseMigrations migrations =
      sequence $ fmap (parseString getSQLStatements mempty) migrations

readMigration :: Migration -> IO String
readMigration migration =
  let
    fileName = name migration
  in
    do
      readFile fileName

readMigrations :: [Migration] -> IO [String]
readMigrations = mapM readMigration

main = do
  -- collect migrations
  fileNames <- toAbsolutes =<< listDirectory "./migrations"
  let migrations = toMigrations $ filterDirs fileNames

  -- read migrations
  readMigrations <- readMigrations migrations

  -- parse migrations
  let parsedMigrations = parseMigrations readMigrations

  -- lookup already run migrations

  -- compare the two and run them

  pure $ show $ parsedMigrations
