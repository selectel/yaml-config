{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Yaml.Config.Internal
    (
    -- * Types
      Config(..)
    , KeyError(..)
    , Key

    -- * Loading
    , load

    -- * Access functions
    , keys
    , subconfig
    , lookup
    , lookupDefault
    , lookupMaybe
    , fullpath
    ) where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (Exception, throw)
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as ST
import Data.Typeable (Typeable)
import Prelude hiding (lookup)

import qualified Data.HashMap.Strict as HashMap
import Data.Yaml (FromJSON (parseJSON), Object, parseMaybe)
import qualified Data.Yaml as Yaml
import qualified Data.Yaml.Include as YamlInclude

-- | Config or field name
type Key = ST.Text

-- | This error can be raised if config has not target path.
newtype KeyError = KeyError Key
    deriving (Show, Typeable)

instance Exception KeyError

-- | Type contains config section and path from root.
data Config = Config [Key] Object
    deriving (Eq, Show)

instance NFData Config where
    rnf (Config p o) = rnf p `seq` rnf o `seq` ()

ke :: Monad m => Key -> m a
ke = throw . KeyError

-- | Returns full path from the root to the given key.
-- Levels are separated by dots.
--
-- >>> fullpath sub "field1"
-- "section1.field1"
--
fullpath :: Config -> Key -> Key
fullpath (Config parents _) path =
    ST.intercalate "." $ reverse (path : parents)

-- | Attempts to load a config from a given YAML file.
-- Fails with @InvalidYaml@ if the file does not exist.
--
-- >>> config <- load "example.yaml"
--
load :: FilePath -> IO Config
load f = maybe err (return . Config []) =<< YamlInclude.decodeFile f
  where
    err = error $ "Invalid config file " <> f <> "."

-- | Returns all toplevel keys in a config.
--
-- >>> keys config
-- ["section1","section2"]
--
keys :: Config -> [Key]
keys (Config _ o) = HashMap.keys o

-- | Returns a value for a given key.
-- Fails with a @KeyError@ if the key doesn't exist.
--
-- >>> keys sub
-- ["field1","field2"]
-- >>> putStrLn =<< lookup "field1" sub
-- value1
--
lookup :: (Monad m, FromJSON a)
       => Key                   -- ^ Field name
       -> Config                -- ^ Config to query
       -> m a                   -- ^ Looked up value
lookup path c = maybe err return $ lookupMaybe path c
  where
    err = ke $ "Field " <> fullpath c path <> " not found or has wrong type."

-- | An exception-free alternative to @lookup@.
--
-- >>> keys sub
-- ["field1","field2"]
-- >>> lookupMaybe "field1" sub
-- Just "value1"
--
-- @since 0.4.1
lookupMaybe :: FromJSON a => Key -> Config -> Maybe a
lookupMaybe path conf = foldM (flip subconfig) conf (init pathes) >>=
    look (last pathes)
  where
    look k (Config _ o) = HashMap.lookup k o >>= parseMaybe parseJSON
    pathes = ST.splitOn "." path

-- | Returns a value for a given key or a default value if a key doesn't exist.
--
-- >>> lookupDefault "field3" "def" sub
-- "def"
--
lookupDefault :: FromJSON a
              => Key            -- ^ Field name
              -> a              -- ^ Default value
              -> Config         -- ^ Config to query
              -> a              -- ^ Looked up or default value
lookupDefault p d = fromMaybe d . lookupMaybe p

-- | Narrows into a config section corresponding to a given key.
-- Fails with a @KeyError@ if a key doesn't exist at the current level.
--
-- >>> :set -XOverloadedStrings
-- >>> sub <- subconfig "section1" config
--
subconfig :: Monad m
          => Key                 -- ^ Subconfig name
          -> Config              -- ^ (Sub)Config to narrow into
          -> m Config            -- ^ Subconfig
subconfig path c@(Config parents o) = case HashMap.lookup path o of
    Just (Yaml.Object so) -> return $ Config (path : parents) so
    _                     -> err
  where
    err = ke $ "Subconfig " <> fullpath c path <> " not found."
