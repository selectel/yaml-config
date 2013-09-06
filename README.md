# yaml-config [![Build Status][travis-img]][travis]

Load, parse and find fields in YAML config files.

[travis]: http://travis-ci.org/selectel/yaml-config
[travis-img]: https://secure.travis-ci.org/selectel/yaml-config.png

## Example

### YAML config

~~~ {.yaml}
server:
    port: 8080
    logs:
        access: /var/log/server/access.log
        error:  /var/log/server/error.log
~~~
### Haskell source

~~~ {.haskell}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where
import Prelude hiding (lookup)
import Data.Word (Word16)
import Data.Yaml.Config (load, subconfig, lookupDefault, lookup)

main :: IO ()
main = do
    config <- load "./example.yaml"

    serverConfig <- subconfig config "server"
    let interface = lookupDefault serverConfig "interface" "127.0.0.1"
        port :: Word16 = lookupDefault serverConfig "port" 80

    logConfig <- subconfig serverConfig "logs"
    accessLog <- lookup logConfig "access"
    errorLog <- lookup logConfig "error"

    mapM_ putStrLn [interface, (show port), errorLog, accessLog]
~~~

### Result

~~~
$ ./server
127.0.0.1
8080
/var/log/server/error.log
/var/log/server/access.log
~~~

## Links

[Hackage](http://hackage.haskell.org/package/yaml-config)
