{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
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
    , CTypes(..), (:~>)(..), NoReqBody
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
      -- * Helper classes and families
    , AllAre, AllHave
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
get :: (AllAre NoReqBody reqs, RestCallable reqs resps req resp xs n f ctx m) => CTypes reqs resps -> Path xs -> f -> S.SpockCtxT ctx m ()
get = wire S.GET

-- | Specify an action that will be run when the HTTP verb 'POST' and the given route match
post :: RestCallable reqs resps req resp xs n f ctx m => CTypes reqs resps -> Path xs -> f -> S.SpockCtxT ctx m ()
post = wire S.POST

-- | Specify an action that will be run when the HTTP verb 'GET'/'POST' and the given route match
getpost :: RestCallable reqs resps req resp xs n f ctx m => CTypes reqs resps -> Path xs -> f -> S.SpockCtxT ctx m ()
getpost r a = wire S.POST r a >> wire S.GET r a

-- | Specify an action that will be run when the HTTP verb 'HEAD' and the given route match
head :: RestCallable reqs resps req resp xs n f ctx m => CTypes reqs resps -> Path xs -> f -> S.SpockCtxT ctx m ()
head = wire S.HEAD

-- | Specify an action that will be run when the HTTP verb 'PUT' and the given route match
put :: RestCallable reqs resps req resp xs n f ctx m => CTypes reqs resps -> Path xs -> f -> S.SpockCtxT ctx m ()
put = wire S.PUT

-- | Specify an action that will be run when the HTTP verb 'DELETE' and the given route match
delete :: RestCallable reqs resps req resp xs n f ctx m => CTypes reqs resps -> Path xs -> f -> S.SpockCtxT ctx m ()
delete = wire S.DELETE

-- | Specify an action that will be run when the HTTP verb 'PATCH' and the given route match
patch :: RestCallable reqs resps req resp xs n f ctx m => CTypes reqs resps -> Path xs -> f -> S.SpockCtxT ctx m ()
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
type RestCallable reqs resps req resp xs n f ctx m =
    ( AllHave ContentType reqs, AllHave ContentType resps
    , AllHave (ContentReader req) reqs, AllHave (ContentWriter resp) resps
    , HasRep xs, n ~ HVectLenH (req ': xs)
    , HCurry' n f (req ': xs) (S.ActionCtxT ctx m resp)
    , ArityFwd f n, ArityRev f n
    , MonadIO m
    )

-- | Proof that all elements in a type level list are equal to the first param
type family AllAre (x :: *) (xs :: [*]) :: Constraint where
    AllAre x '[] = 'True ~ 'True
    AllAre x (y ': ys) = (x ~ y, AllAre x ys)

-- | Proof that all types in a list conform to a constraint
type family AllHave (c :: * -> Constraint) (xs :: [*]) :: Constraint where
    AllHave x '[] = 'True ~ 'True
    AllHave x (y ': ys) = (x y, AllHave x ys)

-- | List that maps request content types to response content types
data CTypes (reqs :: [*]) (resps :: [*]) where
  CtNull :: CTypes '[] '[]
  (:|:) :: (req :~> resp) -> CTypes as bs -> CTypes (req ': as) (resp ': bs)

infixr 5 :|:

-- | Type level information indicating that clients will not send any data
data NoReqBody

-- | Map a request content type to a response content type
data (:~>) req resp where
    -- | Content-Type header must match the requests mimeType and Accept header the responses mimeType
    (:~>) :: req -> resp -> req :~> resp
    -- | Accept header must match the responses mimeType
    Only :: resp -> NoReqBody :~> resp

-- | Data that can be parsed and/or serialized to json
data JSON = JSON

-- | Define a content type
class ContentType ct where
    ctMimeType :: Proxy ct -> T.Text

instance ContentType JSON where
    ctMimeType _ = "application/json"

instance ContentType NoReqBody where
    ctMimeType _ = error "Library error, this should never be called"

-- | Parse a value from a request bytestring
class ContentReader a ct where
    crDecode :: Proxy ct -> BS.ByteString -> Either String a

instance ContentReader () NoReqBody where
    crDecode _ _ = Right ()

-- | Serialize a value to a request bytestring
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
    forall reqs resps req resp xs n f ctx m.
    ( RestCallable reqs resps req resp xs n f ctx m )
    => S.StdMethod
    -> CTypes reqs resps
    -> S.Path xs
    -> f
    -> S.SpockCtxT ctx m ()
wire m ctypes path a =
    let fun :: H.HList (req ': xs) -> S.ActionCtxT ctx m resp
        fun = hUncurry a

        matcherLoop ::
            forall as bs.
            ( AllHave ContentType as, AllHave ContentType bs
            , AllHave (ContentReader req) as
            , AllHave (ContentWriter resp) bs
            ) => T.Text -> HVect xs -> CTypes as bs -> ActionCtxT ctx m ()
        matcherLoop accept captures cts =
            case cts of
                CtNull ->
                    do S.setStatus status500
                       text "Invalid request: Can not handle required Content-type."
                ((rule :: x :~> y) :|: xs) ->
                    let px :: Proxy x
                        px = Proxy
                        py :: Proxy y
                        py = Proxy
                        bodyHandler runCond =
                            do res <- runCond
                               if res
                               then do bsBody <- S.body
                                       case crDecode px bsBody of
                                           Left errMsg ->
                                            do S.setStatus status500
                                               text $ T.pack $ "Invalid JSON: " ++ errMsg
                                           Right (req :: req) ->
                                            do (result :: resp) <- fun (H.HCons req (vectToHlist captures))
                                               S.setHeader "Content-Type" (ctMimeType py)
                                               S.bytes (cwEncode py result)
                               else matcherLoop accept captures xs
                    in case rule of
                        Only _ ->
                            bodyHandler $ return $ accept `matchesMimeType` ctMimeType py
                        (_ :~> _) ->
                            bodyHandler $
                            do mContentType <- header "content-type"
                               case mContentType of
                                   Nothing ->
                                        return False
                                   Just t ->
                                        return $ accept `matchesMimeType` ctMimeType py && t `matchesMimeType` ctMimeType px

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
