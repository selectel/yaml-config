{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Yaml.Config.Internal
    ( Config(..)
    , KeyError(..)
    , Key

    -- * Work with files
    , load

    -- * Explore config
    , keys
    , subconfig
    , lookup
    , lookupDefault
    , fullpath
    ) where

import Prelude hiding (lookup)
import Control.DeepSeq (NFData(rnf))
import Control.Exception (Exception)
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import qualified Data.Text as ST

import Data.Yaml (Object, FromJSON(parseJSON), parseMaybe)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Yaml as Yaml
import qualified Data.Yaml.Include as YamlInclude
import Control.Failure

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

ke :: Failure KeyError m => Key -> m a
ke = failure . KeyError

-- | Show full path from the root to target key. Levels are separated by dots.
--
-- >>> fullpath sub "field1"
-- "section1.field1"
--
fullpath :: Config -> Key -> Key
fullpath (Config parents _) path = ST.intercalate "." $
    reverse $ path : parents

-- | Find file in filesystem and try to load it as YAML config.
-- May fail with @InvalidYaml@ if file not found.
--
-- >>> config <- load "example.yaml"
--
load :: FilePath -> IO Config
load f = maybe err (return . Config []) =<< YamlInclude.decodeFile f
  where
    err = error $ "Invalid config file " <> f <> "."

-- | Show all first level config field's.
--
-- >>> keys config
-- ["section1","section2"]
--
keys :: Config -> [Key]
keys (Config _ o) = HashMap.keys o

-- | Get value for given key.
-- May fail with @KeyError@ if key doesn't exist.
--
-- >>> keys sub
-- ["field1","field2"]
-- >>> putStrLn =<< lookup "field1" sub
-- value1
--
lookup :: (Failure KeyError m, FromJSON a)
       => Key                               -- ^ Field name
       -> Config                            -- ^ Config for find
       -> m a                               -- ^ Field value
lookup path c = maybe err return $ lookupMaybe path c
  where
    err = ke $ "Field " <> fullpath c path <> " not found or has wrong type."

lookupMaybe :: FromJSON a => Key -> Config -> Maybe a
lookupMaybe path conf = foldM (flip subconfig) conf (init pathes) >>=
    look (last pathes)
  where
    look k (Config _ o) = HashMap.lookup k o >>= parseMaybe parseJSON
    pathes = ST.splitOn "." path

-- | Find value in config and return it or return default value.
--
-- >>> lookupDefault "field3" "def" sub
-- "def"
--
lookupDefault :: FromJSON a
              => Key         -- ^ Field name
              -> a           -- ^ Default value
              -> Config      -- ^ Config for find
              -> a           -- ^ Founded or default value
lookupDefault p d = fromMaybe d . lookup p

-- | Get subconfig by name.
-- May fail with @KeyError@ if target key doesn't exist at current level.
--
-- >>> :set -XOverloadedStrings
-- >>> sub <- subconfig "section1" config
--
subconfig :: Failure KeyError m
          => Key                 -- ^ Subconfig name
          -> Config              -- ^ (Sub)Config for find
          -> m Config            -- ^ Founded Subconfig
subconfig path c@(Config parents o) = case HashMap.lookup path o of
    Just (Yaml.Object so) -> return $ Config (path : parents) so
    _                     -> err
  where
    err = ke $ "Subconfig " <> fullpath c path <> " not found."
