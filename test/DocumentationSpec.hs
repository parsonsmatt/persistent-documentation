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

module DocumentationSpec where

import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Maybe
import Data.Foldable
import Test.Hspec
import Database.Persist.Sql
import Database.Persist.TH

import Database.Persist.Documentation
import Database.Persist.Documentation.Internal (alignFields, single, asHaskellNames)
import Data.StrMap
import Entities

share [mkPersist sqlSettings, deriveShowFields] entityDefs

docs :: [EntityDef]
docs = document entityDefs $ do
  User --^ do
    "you can use string literals to write documentation for the entity itself. "
    "The strings will be mappended together, so you'll need to handle "
    "whitespace yourself."
    UserFirstName # "The user's first name."
    UserActive # "Whether or not the user is able to log in."
    UserId # "You can document the user's ID field."

spec :: Spec
spec = do
  runIO $ Text.writeFile "test/example.md" $ render markdownTableRenderer docs
  describe "Example Documentation" $ do
    it "has documentation for ID field" $ do
      fieldComments (entityId (head docs))
        `shouldBe`
          Just "You can document the user's ID field."
    it "has documentation for all fields" $ do
      for_ docs $ \ed ->
        for_ (entityFields ed) $ \f ->
          fieldComments f `shouldSatisfy` isJust

  describe "FieldDef" $ do
    let
      edef = entityDef (Nothing :: Maybe User)
      fields = entityFields edef
    describe "fieldType" $ do
      it "does not have the entity prefix" $ do
        for_ fields $ \efield -> do
          unHaskellName (fieldHaskell efield)
            `shouldSatisfy`
              (not . ("User" `Text.isPrefixOf`))

      it "has a lowercase first letter" $ do
        for_ fields $ \efield -> do
          Text.unpack (Text.take 1 (unHaskellName (fieldHaskell efield)))
            `shouldSatisfy`
              (all Char.isLower)

  describe "asHaskellNames" $ do
    let
      strMap = mconcat
        [ single UserFirstName "Hello, world"
        , single UserActive "If the user is active"
        , single UserId "UserID"
        ]
    it "formats the EntityField so it corresponds with the HaskellName" $ do
      Set.fromList (Map.keys (asHaskellNames strMap))
        `shouldBe`
          Set.fromList ["firstName", "active", "id"]

  describe "alignFields" $ do
    let
      userDef = entityDef (Nothing :: Maybe User)
      fields = entityId userDef : entityFields userDef
      strMap@(StrMap theMap) = mconcat
        [ single UserFirstName "Hello, world"
        , single UserActive "If the user is active"
        , single UserId "user identity"
        ]
    it "strMap contains an easy-to-find field name" $ do
      Set.fromList (fmap asStrText (Map.keys theMap))
        `shouldBe`
          Set.fromList ["UserActive", "UserFirstName", "UserId"]

    it "has Text for fields with a documented entry in the StrMap" $ do
      Set.fromList (mapMaybe fieldComments (alignFields fields strMap))
        `shouldBe`
          Set.fromList ["Hello, world", "If the user is active", "user identity"]

