{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Main where

import           Data.Aeson
import           Data.HVect
import           Web.Spock.Rest

newtype Req = Req Bool
    deriving (FromJSON)
newtype Resp = Resp Bool
    deriving (ToJSON)

data SampleContentType = SampleContentType

instance ContentType SampleContentType where
    ctMimeType _ = "foo/bar"

instance ContentReader Req SampleContentType where
    crDecode _ bs =
        if bs == "42"
        then Right (Req True)
        else Left "Request has to be 42"

handleReq :: Req -> Int -> ActionCtxT ctx IO Resp
handleReq (Req i) val =
    return $ Resp $ if i then val > 0 else val < 0

main :: IO ()
main =
    runSpock 3000 $ spockT id $
    do post (SampleContentType :~> JSON :|: JSON :~> JSON :|: CtNull) ("foo" <//> var) handleReq
       get (Only JSON :|: CtNull) "load" $ \() -> return $ Resp True
