{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Yaml.Config.Internal
    ( Config(..)
    , Key
    , load
    , keys
    , lookupSubconfig
    , subconfig
    , lookup
    , lookupDefault
    , require
    , fullpath
    ) where

import Prelude hiding (lookup)
import Control.DeepSeq (NFData(rnf))
import Control.Exception (Exception, throwIO)
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import qualified Data.Text as ST

import Data.Yaml (Object, FromJSON(parseJSON), parseMaybe)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Yaml as Yaml

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

ke :: Key -> IO a
ke = throwIO . KeyError

-- | Show full path from the root to target key
fullpath :: Config -> Key -> Key
fullpath (Config parents _) path = ST.intercalate "." $
    reverse $ path : parents

-- | Find file in filesystem and try to load it as YAML config
-- May fail with @KeyError@
load :: FilePath -> IO Config
load f = maybe err (return . Config []) =<< Yaml.decodeFile f
  where
    err = error $ "Invalid config file " <> f <> "."

-- | Show all (sub)config first level filed's name
keys :: Config -> [Key]
keys (Config _ o) = HashMap.keys o

-- | Field value wrapped into @Maybe@ (sub)config
lookup :: FromJSON a
       => Config  -- ^ (Sub)Config for find
       -> Key     -- ^ Field name
       -> Maybe a -- ^ Field value
lookup conf path = foldM lookupSubconfig conf (init pathes) >>=
    look (last pathes)
  where
    look k (Config _ o) = HashMap.lookup k o >>= parseMaybe parseJSON
    pathes = ST.splitOn "." path

-- | Subconfig wrapped into @Maybe@
lookupSubconfig :: Config       -- ^ (Sub)Config for find
                -> Key          -- ^ Field name
                -> Maybe Config -- ^ Maybe Subconfig
lookupSubconfig (Config parents o) k = HashMap.lookup k o >>= \s -> case s of
    (Yaml.Object so) -> Just $ Config (k : parents) so
    _                -> Nothing

-- | Find value in (sub)config and return it or default value
lookupDefault :: FromJSON a
              => Config -- ^ (Sub)Config for find
              -> Key    -- ^ Field name
              -> a      -- ^ Default value
              -> a      -- ^ Return value
lookupDefault c p d = fromMaybe d $ lookup c p

-- | Find subconfig
-- May fail with @KeyError@
subconfig :: Config    -- ^ (Sub)Config for find
          -> Key       -- ^ Subconfig name
          -> IO Config -- ^ Subconfig
subconfig c path = maybe err return $ lookupSubconfig c path
  where
    err = ke $ "Subconfig " <> fullpath c path <> " not found."

-- | Same as @lookup@ buf fail with @KeyError@
-- if there is no field with target name
require :: FromJSON a => Config -> Key -> IO a
require c path = maybe err return $ lookup c path
  where
    err = ke $ "Field " <> fullpath c path <> " not found or has wrong type."
