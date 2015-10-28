{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Data.Aeson
import Web.Spock.Rest

newtype Req = Req Bool
    deriving (FromJSON)
newtype Resp = Resp Bool
    deriving (ToJSON)

handleReq :: Req -> Int -> ActionCtxT ctx IO Resp
handleReq (Req i) val =
    return $ Resp $ if i then (val > 0) else (val < 0)

main :: IO ()
main =
    runSpock 3000 $ spockT id $
    get ("foo" <//> var) handleReq
