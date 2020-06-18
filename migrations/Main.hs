module Main where

import Text.Trifecta.Parser
import System.Directory
import Data.List

filterDirs :: [FilePath] -> [FilePath]
filterDirs = filter (\s -> not $ ".hs" `isSuffixOf` s)

data Migration = Migration
  { number :: String
  , name :: String
  } deriving (Show, Eq)

instance Ord Migration where
  a `compare` b = (number a) `compare` (number b)

toMigration string = Migration
  { number = takeWhile (not . (== '_')) string
  , name = string
  }

toMigrations :: [FilePath] -> [Migration]
toMigrations = sort . fmap toMigration

main = do
  -- collect migrations
  migrations <- toMigrations <$> filterDirs <$> listDirectory "./migrations"

  -- parse migrations

  -- lookup already run migrations

  -- compare the two and run them

  pure migrations
