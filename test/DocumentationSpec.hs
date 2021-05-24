{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
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

import Data.Proxy
import Control.Monad
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

#if MIN_VERSION_persistent(2,13,0)
import Database.Persist.FieldDef.Internal
import Database.Persist.EntityDef.Internal
#endif

share [mkPersist sqlSettings, mkEntityDefList "entityDefs", deriveShowFields] [persistUpperCase|
  User
    firstName Text.Text
    active Bool
    deriving Show Eq Read Ord

  Dog
    Id Text.Text
    toy Text.Text

  UserDog
    dog DogId
    user UserId

|]

docs :: [EntityDef]
docs = document entityDefs $ do
  User --^ do
    "you can use string literals to write documentation for the entity itself. "
    "The strings will be mappended together, so you'll need to handle "
    "whitespace yourself."
    UserFirstName # "The user's first name."
    UserActive # "Whether or not the user is able to log in."
    UserId # "You can document the user's ID field."

  UserDog --^ do
    "Users can have many dogs, and dogs can have many users."
    UserDogDog # "This should have type text."

spec :: Spec
spec = do
  runIO $ Text.writeFile "test/example.md" $ render markdownTableRenderer docs
  let (userDoc : dogDog : userDogDoc : _) = docs
  describe "Example Documentation" $ do
    it "has documentation for ID field" $ do
#if MIN_VERSION_persistent(2,13,0)
      let Just idField = getEntityIdField userDoc
      fieldComments idField
#else
      fieldComments (entityId userDoc)
#endif
        `shouldBe`
          Just "You can document the user's ID field."
    it "has documentation for all User fields" $ do
      for_ (entityFields userDoc) $ \f ->
        fieldComments f `shouldSatisfy` isJust

    describe "UserDogDog" $ do
      let (userDogDog : userDogUser : _) = entityFields userDogDoc
      it "has documentation" $ do
        fieldComments userDogDog
          `shouldBe`
            Just "This should have type text."

  describe "FieldDef" $ do
    let
      edef = entityDef (Nothing :: Maybe User)
      fields = entityFields edef
    describe "fieldType" $ do
      it "does not have the entity prefix" $ do
        for_ fields $ \efield -> do
          unFieldNameHS (fieldHaskell efield)
            `shouldSatisfy`
              (not . ("User" `Text.isPrefixOf`))

      it "has a lowercase first letter" $ do
        for_ fields $ \efield -> do
          Text.unpack (Text.take 1 (unFieldNameHS (fieldHaskell efield)))
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
      fields = toList $ keyAndEntityFields userDef
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
