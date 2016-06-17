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
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Network.HTTP.Nano
import Network.HTTP.Types.URI (urlEncode)

query :: Query
query = Query Nothing Nothing Nothing Nothing

key :: ToJSON a => a -> Key
key = Key

get :: (FbHttpM m e r, HasFirebase r, FromJSON a) => Location -> Maybe Query -> m a
get loc mq = httpJSON =<< fbReq GET loc NoRequestData mq

put :: (FbHttpM m e r, HasFirebase r, ToJSON a) => Location -> a -> m ()
put loc dta = http' =<< fbReq PUT loc (mkJSONData dta) Nothing

post :: (FbHttpM m e r, HasFirebase r, ToJSON a) => Location -> a -> m FBID
post loc dta = unName <$> (httpJSON =<< fbReq POST loc (mkJSONData dta) Nothing)

patch :: (FbHttpM m e r, HasFirebase r, ToJSON a) => Location -> a -> m ()
patch loc dta = http' =<< fbReq (CustomMethod "PATCH") loc (mkJSONData dta) Nothing

delete :: (FbHttpM m e r, HasFirebase r) => Location -> m ()
delete loc = http' =<< fbReq DELETE loc NoRequestData Nothing

fbReq :: (FbHttpM m e r, HasFirebase r) => HttpMethod -> Location -> RequestData -> Maybe Query -> m Request
fbReq mthd loc dta mq = do
    baseURL <- view firebaseURL
    token <- view firebaseToken
    let qstr = maybe "" buildQuery mq
    let url = baseURL ++ loc ++ ".json?auth=" ++ token ++ "&" ++ qstr
    buildReq mthd url dta

buildQuery :: Query -> String
buildQuery q = intercalate "&" . fmap (uncurry toParam) $ catMaybes [orderByQ, startAtQ, endAtQ, limitQ $ limit q]
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
