{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
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
lookup :: (Failure KeyError m, FromJSON a)
       => Config  -- ^ (Sub)Config for find
       -> Key     -- ^ Field name
       -> m a     -- ^ Field value
lookup c path = maybe err return $ lookupMaybe c path
  where
    err = ke $ "Field " <> fullpath c path <> " not found or has wrong type."

lookupMaybe :: FromJSON a => Config -> Key -> Maybe a
lookupMaybe conf path = foldM lookupSubconfig conf (init pathes) >>=
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
{-# DEPRECATED lookupSubconfig "use `subconfig` instead" #-}

-- | Find value in (sub)config and return it or default value
lookupDefault :: FromJSON a
              => Config -- ^ (Sub)Config for find
              -> Key    -- ^ Field name
              -> a      -- ^ Default value
              -> a      -- ^ Return value
lookupDefault c p d = fromMaybe d $ lookup c p

-- | Find subconfig
-- May fail with @KeyError@
subconfig :: Failure KeyError m
          => Config    -- ^ (Sub)Config for find
          -> Key       -- ^ Subconfig name
          -> m Config -- ^ Subconfig
subconfig c path = maybe err return $ lookupSubconfig c path
  where
    err = ke $ "Subconfig " <> fullpath c path <> " not found."

-- | Same as @lookup@ buf fail with @KeyError@
-- if there is no field with target name
require :: FromJSON a => Config -> Key -> IO a
require = lookup
{-# DEPRECATED require "use `lookup` instead" #-}
