{-# LANGUAGE NoImplicitPrelude #-}

-- | Library for read config files in YAML format.
--
-- example.yaml:
--
-- @
-- server:
--     port: 8080
--     logs:
--         access: \/var\/log\/server\/access.log
--         error:  \/var\/log\/server\/error.log
-- @
--
-- Usage example:
--
-- @
--module Main where
--import Prelude hiding (lookup)
--import Data.Word (Word16)
--import Data.Yaml.Config (load, subconfig, lookupDefault, lookup)
--
--main :: IO ()
--main = do
--    config <- load "./example.yaml"
--
--    serverConfig <- subconfig config "server"
--    let interface = lookupDefault serverConfig "interface" "127.0.0.1"
--        port :: Word16 = lookupDefault serverConfig "port" 80
--
--    logConfig <- subconfig serverConfig "logs"
--    accessLog <- lookup logConfig "access"
--    errorLog <- lookup logConfig "error"
--
--    mapM_ putStrLn [interface, (show port), errorLog, accessLog]
-- @
--
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
