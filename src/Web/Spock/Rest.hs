{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Web.Spock.Rest
    ( -- * Spock's route definition monad
      S.spock, S.SpockM, S.SpockCtxM
    , S.spockT, S.spockLimT, S.SpockT, S.SpockCtxT
     -- * Defining routes
    , S.Path, S.root, S.Var, S.var, S.static, (S.<//>)
     -- * Rendering routes
    , S.renderRoute
     -- * Content Encoders
    , ContentType(..), ContentWriter(..), ContentReader(..)
    , JSON(..)
     -- * Hooking routes
    , S.subcomponent, S.prehook
    , get, post, getpost, head, put, delete, patch, wire
    , S.StdMethod (..)
      -- * Adding Wai.Middleware
    , S.middleware
      -- * Safe actions
    , S.SafeAction (..)
    , S.safeActionPath
    , module Web.Spock.Shared
    )
where

import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import           Data.HList.FakePrelude
import           Data.HList.HCurry
import qualified Data.HList.HList          as H
import           Data.HVect                hiding (head)
import qualified Data.Text                 as T
import           Network.HTTP.Types.Status
import           Prelude                   hiding (curry, head, uncurry)
import           Web.Routing.SafeRouting   hiding (renderRoute, singleton)
import qualified Web.Spock                 as S
import           Web.Spock.Shared

-- | Specify an action that will be run when the HTTP verb 'GET' and the given route match
get :: RestCallable ct req resp xs n f ctx m => ct -> Path xs -> f -> S.SpockCtxT ctx m ()
get = wire S.GET

-- | Specify an action that will be run when the HTTP verb 'POST' and the given route match
post :: RestCallable ct req resp xs n f ctx m => ct -> Path xs -> f -> S.SpockCtxT ctx m ()
post = wire S.POST

-- | Specify an action that will be run when the HTTP verb 'GET'/'POST' and the given route match
getpost :: RestCallable ct req resp xs n f ctx m => ct -> Path xs -> f -> S.SpockCtxT ctx m ()
getpost r a = wire S.POST r a >> wire S.GET r a

-- | Specify an action that will be run when the HTTP verb 'HEAD' and the given route match
head :: RestCallable ct req resp xs n f ctx m => ct -> Path xs -> f -> S.SpockCtxT ctx m ()
head = wire S.HEAD

-- | Specify an action that will be run when the HTTP verb 'PUT' and the given route match
put :: RestCallable ct req resp xs n f ctx m => ct -> Path xs -> f -> S.SpockCtxT ctx m ()
put = wire S.PUT

-- | Specify an action that will be run when the HTTP verb 'DELETE' and the given route match
delete :: RestCallable ct req resp xs n f ctx m => ct -> Path xs -> f -> S.SpockCtxT ctx m ()
delete = wire S.DELETE

-- | Specify an action that will be run when the HTTP verb 'PATCH' and the given route match
patch :: RestCallable ct req resp xs n f ctx m => ct -> Path xs -> f -> S.SpockCtxT ctx m ()
patch = wire S.PATCH

-- | Convert 'HVect' to 'H.HList'
vectToHlist :: HVect xs -> H.HList xs
vectToHlist HNil = H.HNil
vectToHlist (a :&: as) = H.HCons a (vectToHlist as)

-- | Length of 'HVect' as 'HNat'
type family HVectLenH (ts :: [*]) :: HNat where
    HVectLenH '[] = 'HZero
    HVectLenH (t ': ts) = 'HSucc (HVectLenH ts)

-- | Type constraints for a rest callable function
type RestCallable ct req resp xs n f ctx m =
    ( ContentType ct
    , ContentReader ct req, ContentWriter ct resp
    , HasRep xs, n ~ HVectLenH (req ': xs)
    , HCurry' n f (req ': xs) (S.ActionCtxT ctx m resp)
    , ArityFwd f n, ArityRev f n
    , MonadIO m
    )

-- | Data that can be parsed and/or serialized to json
data JSON = JSON

-- | Define a content type
class ContentType ct where
    ctMimeType :: Proxy ct -> T.Text

instance ContentType JSON where
    ctMimeType _ = "application/json"

class ContentReader ct a where
    crDecode :: Proxy ct -> BS.ByteString -> Either String a

class ContentWriter ct a where
    cwEncode :: Proxy ct -> a -> BS.ByteString

instance ToJSON a => ContentWriter JSON a where
    cwEncode _ = BSL.toStrict . encode

instance FromJSON a => ContentReader JSON a where
    crDecode _ = eitherDecodeStrict'

-- | Specify an action that will be run when a HTTP verb and the given route match
wire ::
    forall ct req resp xs n f ctx m.
    ( RestCallable ct req resp xs n f ctx m )
    => S.StdMethod
    -> ct
    -> S.Path xs
    -> f
    -> S.SpockCtxT ctx m ()
wire m _ path a =
    let prxy :: Proxy ct
        prxy = Proxy

        fun :: H.HList (req ': xs) -> S.ActionCtxT ctx m resp
        fun = hUncurry a

        handler :: HVect xs -> S.ActionCtxT ctx m ()
        handler captures =
            do bsBody <- S.body
               case crDecode prxy bsBody of
                   Left errMsg ->
                    do S.setStatus status500
                       text $ T.pack $ "Invalid JSON: " ++ errMsg
                   Right (req :: req) ->
                    do (result :: resp) <- fun (H.HCons req (vectToHlist captures))
                       S.setHeader "Content-Type" (ctMimeType prxy)
                       S.bytes (cwEncode prxy result)

        hook :: HVectElim xs (S.ActionCtxT ctx m ())
        hook = curry handler
    in S.hookRoute m path hook
