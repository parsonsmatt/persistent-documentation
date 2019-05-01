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

-- | This module contains code for documenting a set of `persistent` entity
-- definitions.
module Database.Persist.Documentation where

import           Control.Monad.Writer
import qualified Data.Char as Char
import           Data.Foldable        (fold)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.String
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Typeable
import           Database.Persist.Sql hiding (insert)
import           Language.Haskell.TH

import Data.StrMap
import Data.SemiMap

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
abstractRender :: Renderer rendered -> [EntityDef] -> rendered
abstractRender Renderer{..} =
  renderEntities . map f
  where
    f ent = renderEntity ent entityDocs renderedFields
      where
        fields = entityId ent : entityFields ent
        entityDocs = entityComments ent
        renderedFields =
          renderFields (map (\f -> renderField f (fieldComments f)) fields)

-- | Render the '[EntityDef]' into a Markdown table representation.
--
-- @since 0.1.0.0
toMarkdownTables :: [EntityDef] -> Text
toMarkdownTables = abstractRender Renderer{..}
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
           Nothing -> ""
       , "* Primary ID: `" <> unDBName (fieldDB entityId) <> "`"
       , ""
       ]
     <> fields

   renderEntities =
     Text.unlines

   showType SqlString = "string"
   showType SqlInt32 = "integer (32)"
   showType SqlInt64 = "integer (64)"
   showType SqlReal = "double"
   showType SqlNumeric{} = "numeric"
   showType SqlDay = "date"
   showType SqlTime = "time"
   showType SqlDayTime = "datetime"
   showType SqlBlob = "blob"
   showType SqlBool = "boolean"
   showType (SqlOther t) = t

-- | Given a list of 'FieldDef's, this associates each 'FieldDef' with the
-- additional documentation comment provided in the @'StrMap' ('SomeField' rec)
-- 'Text'@ for that entity, if any is present.
--
-- Precondition: The @['FieldDef']@ comes from the @'PersistEntity' rec@ that
-- this is called for. Doing eg:
--
-- @
-- alignFields
--   (entityFields (entityDef (Proxy :: Proxy User)))
--   (strMap :: StrMap (SomeField Order) Text)
-- @
--
-- will be extremely weird.
--
-- @since 0.1.0.0
alignFields
  :: forall rec. RC rec
  => [FieldDef] -> StrMap (SomeField rec) Text -> [FieldDef]
alignFields fields strMap =
  map findFieldDoc fields
  where
    findFieldDoc fld@FieldDef{..} =
      case Map.lookup (nameAsText fieldHaskell) haskellNames of
        Nothing -> fld
        Just c -> fld { fieldComments = Just c }
    haskellNames = asHaskellNames strMap
    nameAsText = lowercaseFirstChar . unHaskellName

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

-- | A type for defining documentation for a schema.
--
-- @since 0.1.0.0
newtype EntityDoc' a = ED (Writer SchemaDocs a)
  deriving (Functor, Applicative, Monad, MonadWriter SchemaDocs)

-- | The 'SchemaDocs' maps a 'TypeRep' of the @'Entity' rec@ that is documented
-- to the 'SomeDocs' for that entity.
--
-- @since 0.1.0.0
type SchemaDocs = SemiMap TypeRep SomeDocs

-- | A wrapper around 'EntityDocs' that allows them to be stored in a list
-- together. Contains the 'RC' constraint alias, which will ensure that all
-- necessary constraints for document rendering are packaged in.
data SomeDocs where
  SomeDocs :: RC rec => EntityDocs rec -> SomeDocs

instance Semigroup SomeDocs where
  SomeDocs (r0 :: EntityDocs r0) <> SomeDocs (r1 :: EntityDocs r1) =
    case eqT @r0 @r1 of
      Just Refl -> SomeDocs (r0 <> r1)
      Nothing -> SomeDocs r0

-- | Expand this constraint synonym to pack necessary constraints in with the
-- 'EntityDocs' type. Used in a few places to ensure that constraints are easy to
-- modify in one place.
--
-- @since 0.1.0.0
type RC rec = (Typeable rec)

-- | 'EntityDocs' contain the documentation comment for the @'Entity' rec@ that
-- is being documented, as well as a map of documentation for the fields of that
-- entity.
--
-- @since 0.1.0.0
data EntityDocs rec = EntityDocs
  { entityDocumentation :: Text
  , fieldDocumentation :: StrMap (SomeField rec) Text
  }

instance Semigroup (EntityDocs rec) where
  EntityDocs d0 f0 <> EntityDocs d1 f1 = EntityDocs (d0 <> d1) (f0 <> f1)

instance Monoid (EntityDocs rec) where
  mempty = EntityDocs mempty mempty

type EntityDoc = EntityDoc' ()
type FieldDoc s = FieldDoc' s ()

-- | Wrap the result type of a 'EntityField' value so it can be stored in
-- homogenous containers.
data SomeField rec where
  SomeField :: FC rec typ => EntityField rec typ -> SomeField rec

-- | We need this instance so we can store 'SomeField' values in the 'StrMap'.
-- The quantified constraint ensures that we can show the underlying field. The
-- 'deriveShowFields' function defined later ensures that this is defined for
-- records in the schema.
instance (forall typ. Show (EntityField rec typ)) => Show (SomeField rec) where
  show (SomeField fld) = show fld

-- | Expand this constraint synonym to pack necessary constraints for packing
-- 'EntityField' values into 'SomeField's.
type FC rec typ = forall x. Show (EntityField rec x)

-- | A monad for writing documentation on an entity's fields.
newtype FieldDoc' rec a = FD (Writer (EntityDocs rec) a)
  deriving (Functor, Applicative, Monad, MonadWriter (EntityDocs rec))

-- | Define documentation for an entity. Looks like this:
--
-- @
-- x :: EntityDoc
-- x = do
--   User ^-- do
--     "This comment is for the entity User."
--     UserName # "This comment is for a field.""
-- @
(^--)
  :: forall a r. (KnowResult a ~ r, Typeable r, RC r)
  => a
  -- ^ A constructor for the @'Entity' r@ you want to document.
  -> FieldDoc r
  -- ^ A block that contains documentation for the @'Entity' r@.
  -> EntityDoc
_ ^-- FD fieldDocs =
  tell
  . SemiMap
  $ Map.singleton
    (typeRep (Proxy @r))
    (SomeDocs (execWriter fieldDocs))

-- | Write documentation for the given 'EntityField'.
(#) :: FC rec typ => EntityField rec typ -> Text -> FieldDoc rec
field # txt = tell mempty { fieldDocumentation = single field txt }

single
  :: FC rec typ
  => EntityField rec typ -> Text -> StrMap (SomeField rec) Text
single k t = insert (SomeField k) t mempty

type family KnowResult a where
  KnowResult (i -> o) = KnowResult o
  KnowResult a = a

instance (a ~ ()) => IsString (FieldDoc' s a) where
  fromString str = tell mempty { entityDocumentation = Text.pack str }

-- | Given a list of entity definitions, derives `Show` for all their fields.
-- This is necessary for using this library for internal reasons, unfortunately.
deriveShowFields :: [EntityDef] -> Q [Dec]
deriveShowFields defs = fmap join . forM defs $ \def -> do
  let name = conT . mkName . Text.unpack . unHaskellName . entityHaskell $ def
  [d|deriving instance Show (EntityField $(name) x)|]

lowercaseFirstChar :: Text -> Text
lowercaseFirstChar txt = case Text.uncons txt of
  Just (c, r) -> Char.toLower c `Text.cons` r
  Nothing -> ""
