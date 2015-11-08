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
import           GHC.Exts
import           Network.HTTP.Types.Status
import           Prelude                   hiding (curry, head, uncurry)
import           Web.Routing.SafeRouting   hiding (renderRoute, singleton)
import qualified Web.Spock                 as S
import           Web.Spock.Shared

-- | Specify an action that will be run when the HTTP verb 'GET' and the given route match
get :: RestCallable cts req resp xs n f ctx m => HVect cts -> Path xs -> f -> S.SpockCtxT ctx m ()
get = wire S.GET

-- | Specify an action that will be run when the HTTP verb 'POST' and the given route match
post :: RestCallable cts req resp xs n f ctx m => HVect cts -> Path xs -> f -> S.SpockCtxT ctx m ()
post = wire S.POST

-- | Specify an action that will be run when the HTTP verb 'GET'/'POST' and the given route match
getpost :: RestCallable cts req resp xs n f ctx m => HVect cts -> Path xs -> f -> S.SpockCtxT ctx m ()
getpost r a = wire S.POST r a >> wire S.GET r a

-- | Specify an action that will be run when the HTTP verb 'HEAD' and the given route match
head :: RestCallable cts req resp xs n f ctx m => HVect cts -> Path xs -> f -> S.SpockCtxT ctx m ()
head = wire S.HEAD

-- | Specify an action that will be run when the HTTP verb 'PUT' and the given route match
put :: RestCallable cts req resp xs n f ctx m => HVect cts -> Path xs -> f -> S.SpockCtxT ctx m ()
put = wire S.PUT

-- | Specify an action that will be run when the HTTP verb 'DELETE' and the given route match
delete :: RestCallable cts req resp xs n f ctx m => HVect cts -> Path xs -> f -> S.SpockCtxT ctx m ()
delete = wire S.DELETE

-- | Specify an action that will be run when the HTTP verb 'PATCH' and the given route match
patch :: RestCallable cts req resp xs n f ctx m => HVect cts -> Path xs -> f -> S.SpockCtxT ctx m ()
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
type RestCallable cts req resp xs n f ctx m =
    ( AllHave ContentType cts
    , AllHave (ContentReader req) cts, AllHave (ContentWriter resp) cts
    , HasRep xs, n ~ HVectLenH (req ': xs)
    , HCurry' n f (req ': xs) (S.ActionCtxT ctx m resp)
    , ArityFwd f n, ArityRev f n
    , MonadIO m
    )

-- | Proof that all types in a list conform to a constraint
type family AllHave (c :: * -> Constraint) (xs :: [*]) :: Constraint where
    AllHave x '[] = 'True ~ 'True
    AllHave x (y ': ys) = (x y, AllHave x ys)

-- | Data that can be parsed and/or serialized to json
data JSON = JSON

-- | Define a content type
class ContentType ct where
    ctMimeType :: Proxy ct -> T.Text

instance ContentType JSON where
    ctMimeType _ = "application/json"

class ContentReader a ct where
    crDecode :: Proxy ct -> BS.ByteString -> Either String a

class ContentWriter a ct where
    cwEncode :: Proxy ct -> a -> BS.ByteString

instance ToJSON a => ContentWriter a JSON where
    cwEncode _ = BSL.toStrict . encode

instance FromJSON a => ContentReader a JSON where
    crDecode _ = eitherDecodeStrict'

matchesMimeType :: T.Text -> T.Text -> Bool
matchesMimeType t needle =
    let (mimeTypeStr, _) = T.breakOn ";" t
        mimeTypes = map (T.toLower . T.strip) $ T.splitOn "," mimeTypeStr
        firstMatch [] = False
        firstMatch (x:xs) =
            x == needle || firstMatch xs
    in firstMatch mimeTypes

-- | Specify an action that will be run when a HTTP verb and the given route match
wire ::
    forall cts req resp xs n f ctx m.
    ( RestCallable cts req resp xs n f ctx m )
    => S.StdMethod
    -> HVect cts
    -> S.Path xs
    -> f
    -> S.SpockCtxT ctx m ()
wire m ctypes path a =
    let fun :: H.HList (req ': xs) -> S.ActionCtxT ctx m resp
        fun = hUncurry a

        matcherLoop ::
            forall yts.
            ( AllHave ContentType yts
            , AllHave (ContentReader req) yts
            , AllHave (ContentWriter resp) yts
            ) => T.Text -> HVect xs -> HVect yts -> ActionCtxT ctx m ()
        matcherLoop accept captures cts =
            case cts of
                HNil ->
                    do S.setStatus status500
                       text "Invalid request: Can not handle required Content-type."
                ((_ :: x) :&: xs) ->
                    let p :: Proxy x
                        p = Proxy
                    in if accept `matchesMimeType` ctMimeType p
                       then do bsBody <- S.body
                               case crDecode p bsBody of
                                   Left errMsg ->
                                    do S.setStatus status500
                                       text $ T.pack $ "Invalid JSON: " ++ errMsg
                                   Right (req :: req) ->
                                    do (result :: resp) <- fun (H.HCons req (vectToHlist captures))
                                       S.setHeader "Content-Type" (ctMimeType p)
                                       S.bytes (cwEncode p result)
                       else matcherLoop accept captures xs

        handler :: HVect xs -> S.ActionCtxT ctx m ()
        handler captures =
            do mAccept <- header "accept"
               case mAccept of
                   Nothing ->
                        do S.setStatus status500
                           text "Missing Accept header!"
                   Just t ->
                        matcherLoop t captures ctypes

        hook :: HVectElim xs (S.ActionCtxT ctx m ())
        hook = curry handler
    in S.hookRoute m path hook
