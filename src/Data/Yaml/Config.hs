{-# LANGUAGE NoImplicitPrelude #-}

module Data.Yaml.Config
    ( Config
    , Key
    , load
    , keys
    , lookupSubconfig
    , subconfig
    , lookup
    , lookupDefault
    , require
    ) where

import Data.Yaml.Config.Internal
