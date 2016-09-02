{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where
import Prelude hiding (lookup)
import Data.Word (Word16)
import Data.Yaml.Config (load, subconfig, lookupDefault, lookup)

main :: IO ()
main = do
    config <- load "./example.yaml"

    serverConfig <- subconfig "server" config
    let interface = lookupDefault "interface" "127.0.0.1" serverConfig
        port :: Word16 = lookupDefault "port" 80 serverConfig

    logConfig <- subconfig "logs" serverConfig
    accessLog <- lookup "access" logConfig
    errorLog <- lookup "error" logConfig

    mapM_ putStrLn [interface, (show port), errorLog, accessLog]
