{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module Entities where

import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Maybe
import Data.Foldable
import Test.Hspec
import Database.Persist.Sql
import Database.Persist.TH

import Database.Persist.Documentation

entityDefs :: [EntityDef]
entityDefs = [persistUpperCase|
  User
    firstName Text.Text
    active Bool
    deriving Show Eq Read Ord
|]

