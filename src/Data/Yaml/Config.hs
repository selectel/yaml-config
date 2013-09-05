{-# LANGUAGE NoImplicitPrelude #-}

module Data.Yaml.Config
    ( Config
    , Key
    , load
    , keys
    , subconfig
    , lookup
    , lookupDefault
    ) where

import Data.Yaml.Config.Internal
