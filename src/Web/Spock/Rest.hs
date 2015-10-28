{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Web.Spock.Rest
    ( -- * Spock's route definition monad
      S.spock, S.SpockM, S.SpockCtxM
    , S.spockT, S.spockLimT, S.SpockT, S.SpockCtxT
     -- * Defining routes
    , S.Path, S.root, S.Var, S.var, S.static, (S.<//>)
     -- * Rendering routes
    , S.renderRoute
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

import Control.Monad.Trans
import Data.Aeson
import Data.HList.FakePrelude
import Data.HList.HCurry
import Data.HVect hiding (head)
import Prelude hiding (curry, uncurry, head)
import Web.Routing.SafeRouting hiding (renderRoute, singleton)
import qualified Data.HList.HList as H
import qualified Web.Spock as S
import Web.Spock.Shared

-- | Specify an action that will be run when the HTTP verb 'GET' and the given route match
get :: RestCallable req resp xs n f ctx m => Path xs -> f -> S.SpockCtxT ctx m ()
get = wire S.GET

-- | Specify an action that will be run when the HTTP verb 'POST' and the given route match
post :: RestCallable req resp xs n f ctx m => Path xs -> f -> S.SpockCtxT ctx m ()
post = wire S.POST

-- | Specify an action that will be run when the HTTP verb 'GET'/'POST' and the given route match
getpost :: RestCallable req resp xs n f ctx m => Path xs -> f -> S.SpockCtxT ctx m ()
getpost r a = wire S.POST r a >> wire S.GET r a

-- | Specify an action that will be run when the HTTP verb 'HEAD' and the given route match
head :: RestCallable req resp xs n f ctx m => Path xs -> f -> S.SpockCtxT ctx m ()
head = wire S.HEAD

-- | Specify an action that will be run when the HTTP verb 'PUT' and the given route match
put :: RestCallable req resp xs n f ctx m => Path xs -> f -> S.SpockCtxT ctx m ()
put = wire S.PUT

-- | Specify an action that will be run when the HTTP verb 'DELETE' and the given route match
delete :: RestCallable req resp xs n f ctx m => Path xs -> f -> S.SpockCtxT ctx m ()
delete = wire S.DELETE

-- | Specify an action that will be run when the HTTP verb 'PATCH' and the given route match
patch :: RestCallable req resp xs n f ctx m => Path xs -> f -> S.SpockCtxT ctx m ()
patch = wire S.PATCH

-- | Convert 'HVect' to 'H.HList'
vectToHlist :: HVect xs -> H.HList xs
vectToHlist HNil = H.HNil
vectToHlist (a :&: as) = H.HCons a (vectToHlist as)

-- | Length of 'HVect' as 'HNat'
type family HVectLenH (ts :: [*]) :: HNat where
    HVectLenH '[] = HZero
    HVectLenH (t ': ts) = HSucc (HVectLenH ts)

-- | Type constraints for a rest callable function
type RestCallable req resp xs n f ctx m =
    ( FromJSON req, ToJSON resp
    , HasRep xs, n ~ HVectLenH (req ': xs)
    , HCurry' n f (req ': xs) (S.ActionCtxT ctx m resp)
    , ArityFwd f n, ArityRev f n
    , MonadIO m
    )

-- | Specify an action that will be run when a HTTP verb and the given route match
wire ::
    forall req resp xs n f ctx m.
    ( RestCallable req resp xs n f ctx m )
    => S.StdMethod
    -> S.Path xs
    -> f
    -> S.SpockCtxT ctx m ()
wire m path a =
    let fun :: H.HList (req ': xs) -> S.ActionCtxT ctx m resp
        fun = hUncurry a

        handler :: HVect xs -> S.ActionCtxT ctx m ()
        handler captures =
            do (req :: req) <- S.jsonBody'
               (result :: resp) <- fun (H.HCons req (vectToHlist captures))
               S.json result
        hook :: HVectElim xs (S.ActionCtxT ctx m ())
        hook = curry handler
    in S.hookRoute m path hook
