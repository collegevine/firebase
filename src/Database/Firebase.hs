{-# LANGUAGE TupleSections #-}

module Database.Firebase(
    module Database.Firebase.Types,
    query,
    key,
    get,
    put,
    post,
    patch,
    delete
) where

import Database.Firebase.Types

import Control.Applicative ((<$>))
import Control.Lens (view)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO)
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Network.HTTP.Nano
import Network.HTTP.Types.URI (urlEncode)

query :: Query
query = Query Nothing Nothing Nothing Nothing

key :: ToJSON a => a -> Key
key = Key

get :: (
  MonadIO m, 
  MonadError e m, 
  MonadReader r m,
  AsHttpError e,
  HasFirebase r,
  HasHttpCfg r,
  FromJSON a) => 
    Location -> 
    Maybe Query -> m a
get loc mq = httpJSON =<< fbReq GET loc NoRequestData mq

put :: (
  MonadIO m, MonadError e m,
  MonadReader r m,
  AsHttpError e,
  HasFirebase r,
  HasHttpCfg r,
  ToLocation a,
  ToJSON a) => 
    a -> m ()
put dta = do
  fb <- view firebase
  let path = toLoc dta fb
  http' =<< fbReq PUT path (mkJSONData dta) Nothing

post :: (
  MonadIO m, MonadError e m,
  MonadReader r m,
  AsHttpError e,
  HasFirebase r,
  HasHttpCfg r,
  ToLocation a,
  ToJSON a) => 
    a -> m FBID
post dta = do
  fb <- view firebase
  let path = toLoc dta fb
  unName <$> (httpJSON =<< fbReq POST path (mkJSONData dta) Nothing)

patch :: (
  MonadIO m, MonadError e m,
  MonadReader r m,
  AsHttpError e,
  HasFirebase r,
  HasHttpCfg r,
  ToLocation a,
  ToJSON a) => 
    a -> m ()
patch dta = do
  fb <- view firebase
  let path = toLoc dta fb
  http' =<< fbReq (CustomMethod "PATCH") path (mkJSONData dta) Nothing

delete :: (
  MonadIO m, MonadError e m,
  MonadReader r m,
  AsHttpError e,
  HasFirebase r,
  HasHttpCfg r,
  ToLocation a) => 
    a -> m ()
delete obj = do 
  fb <- view firebase
  let path = toLoc obj fb
  http' =<< fbReq DELETE path NoRequestData Nothing

fbReq :: (MonadIO m, MonadError e m, MonadReader r m, AsHttpError e, HasFirebase r, HasHttpCfg r) => 
  HttpMethod -> Location -> RequestData -> Maybe Query -> m Request
fbReq mthd loc dta mq = do
    baseURL <- view firebaseURL
    token <- view firebaseToken
    let qstr = maybe "" buildQuery mq
    let url = baseURL ++ loc ++ ".json?auth=" ++ token ++ "&" ++ qstr
    buildReq mthd url dta

buildQuery :: Query -> String
buildQuery q = concat . intersperse "&" . fmap (uncurry toParam) $ catMaybes [orderByQ, startAtQ, endAtQ, limitQ $ limit q]
    where
    orderByQ = ("orderBy",) . show <$> orderBy q
    startAtQ = ("startAt",) . encodeK <$> startAt q
    endAtQ = ("endAt",) . encodeK <$> endAt q
    limitQ Nothing = Nothing
    limitQ (Just (LimitToFirst l)) = Just ("limitToFirst", show l)
    limitQ (Just (LimitToLast l)) = Just ("limitToLast", show l)

encodeK :: Key -> String
encodeK (Key a) = BL.unpack $ encode a

toParam :: String -> String -> String
toParam k v = k ++ "=" ++ (B.unpack . urlEncode False $ B.pack v)
