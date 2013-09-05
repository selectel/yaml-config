{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Yaml.Config.Internal
    ( Config(..)
    , Key
    , load
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
import Control.Failure

-- | Subconfig or field name
type Key = ST.Text

-- | Throwable exception
newtype KeyError = KeyError Key
    deriving (Show, Typeable)

instance Exception KeyError

-- | (Sub)Config type
data Config = Config [Key] Object
    deriving (Eq, Show)

instance NFData Config where
    rnf (Config p o) = rnf p `seq` rnf o `seq` ()

ke :: Failure KeyError m => Key -> m a
ke = failure . KeyError

-- | Show full path from the root to target key
fullpath :: Config -> Key -> Key
fullpath (Config parents _) path = ST.intercalate "." $
    reverse $ path : parents

-- | Find file in filesystem and try to load it as YAML config.
-- May fail with @KeyError@.
load :: FilePath -> IO Config
load f = maybe err (return . Config []) =<< Yaml.decodeFile f
  where
    err = error $ "Invalid config file " <> f <> "."

-- | Show all (sub)config first level filed's name
keys :: Config -> [Key]
keys (Config _ o) = HashMap.keys o

-- | Get value for given key.
-- May fail with @KeyError@.
lookup :: (Failure KeyError m, FromJSON a)
       => Key     -- ^ Field name
       -> Config  -- ^ (Sub)Config for find
       -> m a     -- ^ Field value
lookup path c = maybe err return $ lookupMaybe path c
  where
    err = ke $ "Field " <> fullpath c path <> " not found or has wrong type."

lookupMaybe :: FromJSON a => Key -> Config -> Maybe a
lookupMaybe path conf = foldM (flip subconfig) conf (init pathes) >>=
    look (last pathes)
  where
    look k (Config _ o) = HashMap.lookup k o >>= parseMaybe parseJSON
    pathes = ST.splitOn "." path

-- | Find value in (sub)config and return it or default value
lookupDefault :: FromJSON a
              => Key    -- ^ Field name
              -> a      -- ^ Default value
              -> Config -- ^ (Sub)Config for find
              -> a      -- ^ Return value
lookupDefault p d = fromMaybe d . lookup p

-- | Find subconfig.
-- May fail with @KeyError@.
subconfig :: Failure KeyError m
          => Key       -- ^ Subconfig name
          -> Config    -- ^ (Sub)Config for find
          -> m Config -- ^ Subconfig
subconfig path c@(Config parents o) = case HashMap.lookup path o of
    Just (Yaml.Object so) -> return $ Config (path : parents) so
    _                     -> err
  where
    err = ke $ "Subconfig " <> fullpath c path <> " not found."
