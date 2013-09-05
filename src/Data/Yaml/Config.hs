{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Yaml.Config
    ( Config
    , Key
    , load
    , keys
    , subconfig
    , lookup
    , lookupDefault

    -- deprecated stuff
    , lookupSubconfig
    , require
    ) where

import Data.Yaml.Config.Internal
