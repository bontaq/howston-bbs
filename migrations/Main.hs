module Main where

import Text.Trifecta.Parser
import System.Directory
import Data.List

filterDirs :: [FilePath] -> [FilePath]
filterDirs = filter (\s -> not $ ".hs" `isSuffixOf` s)

main = do
  -- collect files
  files <- filterDirs <$> listDirectory "./migrations"

  -- order files
  pure files
