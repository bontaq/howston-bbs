{-# LANGUAGE OverloadedStrings #-}
module M1_add_users where

import Control.Monad
import Database.Rivet.V0

migrate :: Migration IO ()
migrate = sql up down

up = "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";"
  <> "CREATE TABLE users("
  <> "id uuid DEFAULT uuid_generate_v4 (),"
  <> "username VARCHAR NOT NULL,"
  <> "password VARCHAR NOT NULL,"
  <> "PRIMARY KEY (id)"
  <> ");"

down = "DROP TABLE users;"
