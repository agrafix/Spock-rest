Spock-rest
=====

[![Build Status](https://travis-ci.org/agrafix/Spock-rest.svg)](https://travis-ci.org/agrafix/Spock-rest)


## Intro


DSL for defining RESTful services in Spock (WIP)


## Library Usage Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Data.Aeson
import Data.HVect
import Web.Spock.Rest

newtype Req = Req Bool
    deriving (FromJSON)
newtype Resp = Resp Bool
    deriving (ToJSON)

handleReq :: Req -> Int -> ActionCtxT ctx IO Resp
handleReq (Req i) val =
    return $ Resp $ if i then val > 0 else val < 0

main :: IO ()
main =
    runSpock 3000 $ spockT id $
    get (singleton JSON) ("foo" <//> var) handleReq

```

## Install

* From Source (cabal): `git clone https://github.com/agrafix/Spock-rest.git && cd Spock-rest && cabal install`
* From Source (stack): `git clone https://github.com/agrafix/Spock-rest.git && cd Spock-rest && stack build`


## Misc

### Supported GHC Versions

* 7.10.2

### License

Released under the MIT license.
2015 Alexander Thiemann <mail@athiemann.net>
