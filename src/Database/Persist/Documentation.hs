{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | This module contains code for documenting a set of @persistent@ entity
-- definitions. All the library provides is a means to render a Markdown
-- document with table and column documentation and comments. A further
-- expansion could use the information here to generate PostgreSQL @COMMENT@s on
-- the fields and tables too.
--
-- = Getting Started
--
-- You probably already have a @persistent@ entity definitions somewhere, and
-- they probably look like this:
--
-- @
-- 'share' ['mkPersist' 'sqlSettings'] ['persistUpperCase'|
--   User
--     firstName Text.Text
--     active    Bool
--     deriving Show Eq Read Ord
-- |]
-- @
--
-- The 'persistUpperCase' QuasiQuoter parses the block of text and returns
-- a value of type @['EntityDef']@. We need to get our hands on that
-- definition so we can document it. We'll use the 'mkEntityDefList'
-- function to expose it:
--
-- @
-- 'share'
--   [ 'mkPersist' 'sqlSettings'
--   , 'mkEntityDefList' "entityDefs"
--   ] ['persistUpperCase'|
--   User
--     firstName Text.Text
--     active    Bool
--     deriving Show Eq Read Ord
-- |]
-- @
--
-- You may want to factor out the quasiquoter into a term and import from
-- another module. This has an important downside: the ID fields from the
-- QuasiQuoter are given as 'Int64' regardless of what they actually are.
-- It's not possible for the persistent quasiquoter to properly know the
-- types of the IDs.
--
-- = Documentating The Schema
--
-- Now, we're going to use the 'document' function to link up the
-- @entityDefs@ with a documentation expression (type 'EntityDoc').
--
-- @
-- docs :: ['EntityDef']
-- docs = 'document' entityDefs $ do
--   pure ()
-- @
--
-- The 'EntityDoc' type is a monad, and we'll use @do@ notation to sequence
-- multiple entity definitions.
--
-- @
-- docs :: ['EntityDef']
-- docs = 'document' entityDefs $ do
--   User '--^' do
--     pure ()
-- @
--
-- The '--^' operator mimics the Haddock comment syntax. We use the
-- constructor of the entity (in this case, @User@). On the right, we
-- provide documentation for the entity. The right hand expression will
-- have the type 'FieldDoc', and we can use @do@ notation to construct it.
--
-- We can use string literals to document the entity itself, with the
-- @OverloadedStrings@ extension enabled. The string literals are
-- concatenated, and used to provide entity-level comments. You'll need to
-- manage whitespace yourself, though.
--
-- @
-- docs :: ['EntityDef']
-- docs = 'document' entityDefs $ do
--   User '--^' do
--     "This is user documentation. "
--     "You can have multiple lines, but you need to watch out for spaces. "
--     "The lines will be combined."
-- @
--
-- We can also document the entity fields. We do this using the '#'
-- operator.
--
-- @
-- docs :: ['EntityDef']
-- docs = 'document' entityDefs $ do
--   User '--^' do
--     "This is user documentation. "
--     "You can have multiple lines, but you need to watch out for spaces. "
--     "The lines will be combined."
--
--     UserFirstName '#' "The user's first name."
--     UserActive    '#' "Whether or not the user is able to log in."
-- @
--
-- This attaches the comment to the entity field.
--
-- = Rendering the Documentation
--
-- Finally, we'll use 'render' and provide a 'Renderer' to generate
-- documentation. For an example of what this looks like, check out the
-- file @test/example.md@ in the repository (linked from the README).
--
-- @
-- renderedDocs :: Text
-- renderedDocs = 'render' 'markdownTableRenderer' docs
-- @
module Database.Persist.Documentation
  ( -- * The Documentation DSL
    document
  , (--^)
  , (#)
  , EntityDoc
  , FieldDoc
  , deriveShowFields
    -- * Rendering Documentation
  , Renderer(..)
  , render
  , markdownTableRenderer
  ) where

import           Control.Monad.Writer
import qualified Data.Char                               as Char
import           Data.Foldable                           (fold)
import           Data.Map                                (Map)
import qualified Data.Map                                as Map
import           Data.String
import           Data.Text                               (Text)
import qualified Data.Text                               as Text
import           Data.Typeable
import           Database.Persist.Sql                    hiding (insert)
import           Language.Haskell.TH

import           Data.SemiMap
import           Data.StrMap
import           Database.Persist.Documentation.Internal

-- | This function accepts a list of 'EntityDef' and an 'EntityDoc' block, and
-- substitutes the 'entityComments' and 'fieldComments' from the
-- 'EntityDoc'.
--
-- @since 0.1.0.0
document :: [EntityDef] -> EntityDoc -> [EntityDef]
document entities (ED docs) = fmap associate entities
  where
    schemaDocs = execWriter docs
    typeReps = Map.mapKeys show (unSemiMap schemaDocs)
    associate edef =
      let
        tyStr = Text.unpack . unHaskellName . entityHaskell $ edef
       in
        case Map.lookup tyStr typeReps of
          Just (SomeDocs (EntityDocs e cs)) ->
            edef
              { entityComments = Just e
              , entityFields = alignFields (entityFields edef) cs
              , entityId = head (alignFields [entityId edef] cs)
              }
          Nothing -> edef

-- | A renderer for documented entities, abstract in the intermediate
-- representations of entities and fields.
--
-- @since 0.1.0.0
data Renderer rendered where
  Renderer
    :: { renderField :: FieldDef -> Maybe Text -> renderedField
       -- ^ Render a field definition as some intermediate structure
       , renderFields :: [renderedField] -> renderedFields
       -- ^ Fold a collection of rendered fields
       , renderEntity :: EntityDef -> Maybe Text -> renderedFields -> renderedEntity
       -- ^ Attach some entity-level metadata to a rendered collection of fields
       , renderEntities :: [renderedEntity] -> rendered
       -- ^ Finally, fold a collection of rendered entities
       }
    -> Renderer rendered

-- | Given a 'Renderer' for a list of entity defintiions, render it.
--
-- @since 0.1.0.0
render :: Renderer rendered -> [EntityDef] -> rendered
render Renderer{..} =
  renderEntities . map f
  where
    f ent = renderEntity ent entityDocs renderedFields
      where
        fields = entityId ent : entityFields ent
        entityDocs = entityComments ent
        renderedFields =
          renderFields (map (\f -> renderField f (fieldComments f)) fields)

-- | A 'Renderer' that generates Markdown tables for an entity.
--
-- === __ Example __
--
-- Given 'entityDefs' like:
--
-- @
-- entityDefs :: ['EntityDef']
-- entityDefs = ['persistUpperCase'|
--   User
--     firstName Text.Text
--     active    Bool
--     deriving Show Eq Read Ord
-- |]
-- @
--
-- and a doc block like:
--
-- @
-- docs :: [EntityDef]
-- docs = document entityDefs $ do
--   User --^ do
--     "you can use string literals to write documentation for the entity itself. "
--     "The strings will be mappended together, so you'll need to handle "
--     "whitespace yourself."
--     UserFirstName # "The user's first name."
--     UserActive # "Whether or not the user is able to log in."
--     UserId # "You can document the user's ID field."
-- @
--
-- This will rende the given Markdown output:
--
-- @
-- # `User`
--
-- you can use string literals to write documentation for the entity itself. The strings will be
-- mappended together, so you'll need to handle whitespace yourself.
--
-- * Primary ID: `id`
--
-- | Column name | Type | Description |
-- |-|-|-|
-- | `id` | integer (64) | You can document the user's ID field. |
-- | `firstName` | string | The user's first name. |
-- | `active` | boolean | Whether or not the user is able to log in. |
-- @
--
-- @since 0.1.0.0
markdownTableRenderer :: Renderer Text
markdownTableRenderer = Renderer{..}
  where
   renderField FieldDef{..} mextra =
      fold
        [ "| `"
        , unDBName fieldDB
        , "` | "
        , showType fieldSqlType
        , " | "
        , fold mextra
        , " |"
        ]

   renderFields xs =
     Text.unlines $
         "| Column name | Type | Description |"
       : "|-|-|-|"
       : xs

   renderEntity EntityDef{..} mdocs fields =
     Text.unlines
       [ "# `" <> unDBName entityDB <> "`"
       , case mdocs of
           Just entityDocs -> "\n" <> entityDocs <> "\n"
           Nothing         -> ""
       , "* Primary ID: `" <> unDBName (fieldDB entityId) <> "`"
       , ""
       ]
     <> fields

   renderEntities =
     Text.unlines

   showType SqlString    = "string"
   showType SqlInt32     = "integer (32)"
   showType SqlInt64     = "integer (64)"
   showType SqlReal      = "double"
   showType SqlNumeric{} = "numeric"
   showType SqlDay       = "date"
   showType SqlTime      = "time"
   showType SqlDayTime   = "datetime"
   showType SqlBlob      = "blob"
   showType SqlBool      = "boolean"
   showType (SqlOther t) = t

-- | Render the '[EntityDef]' into a Markdown table representation. See
-- 'markdownTable
--
-- @since 0.1.0.0
toMarkdownTables :: [EntityDef] -> Text
toMarkdownTables = render markdownTableRenderer

-- | Formats the @'SomeField' rec@ in the keys of the 'Map' to be formatted in
-- the same way as the 'HaskellName' present in a 'FieldDef'.
--
-- @since 0.1.0.0
asHaskellNames
  :: forall rec. RC rec
  => StrMap (SomeField rec) Text -> Map Text Text
asHaskellNames (StrMap extraDocMap) =
  Map.mapKeys (lowercaseFirstChar . Text.drop (length recName) . asStrText) extraDocMap
  where
    recName =
      show (typeRep (Proxy @rec))

-- | Given a list of entity definitions, derives `Show` for all their fields.
-- This is necessary for using this library for internal reasons, unfortunately.
--
-- @since 0.1.0.0
deriveShowFields :: [EntityDef] -> Q [Dec]
deriveShowFields defs = fmap join . forM defs $ \def -> do
  let name = conT . mkName . Text.unpack . unHaskellName . entityHaskell $ def
  [d|deriving instance Show (EntityField $(name) x)|]
