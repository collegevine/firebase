{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Firebase.Types where

import Control.Applicative ((<$>))
import Control.Lens.TH
import Control.Monad (mzero)
import Control.Monad.Reader  (MonadReader)
import Control.Monad.Except (MonadError)
import Network.HTTP.Nano
import Control.Monad.Trans (MonadIO)
import Data.Aeson
import qualified Data.Text.Internal as T

type Location = String
type FBID = String
type FbHttpM m e r = (MonadIO m, MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r)

data Firebase = Firebase {
    _firebaseToken :: String,
    _firebaseURL :: String
}

-- |Representation of a Firebase name response to a POST
newtype Name = Name { unName :: String } deriving Show

instance FromJSON Name where
    parseJSON (Object v) = Name <$> v .: "name"
    parseJSON _ = mzero

data Query = Query {
    orderBy :: Maybe Location,
    startAt :: Maybe Key,
    endAt :: Maybe Key,
    limit :: Maybe Limit
}

data Message a = Message {
    to                  :: Maybe String,
    registrationIDs     :: [String],
    collapseKey         :: Maybe String,
    priority            :: Priority,
    contentAvailable    :: Maybe Bool,
    delayWhileIdle      :: Maybe Bool,
    ttl                 :: Maybe Int,
    payload             :: MessageBody a
}

instance ToJSON a => ToJSON (Message a) where
    toJSON Message {..} = 
        omitNulls [ "to" .= toJSON to,
                    "registration_ids" .= toJSON registrationIDs,
                    "collapse_key" .= toJSON collapseKey,
                    "priority" .= toJSON priority,
                    "content_available" .= toJSON contentAvailable,
                    "delay_while_idle" .= toJSON delayWhileIdle,
                    "time_to_live" .= toJSON ttl,
                    case payload of
                         Notification x -> "notification" .= toJSON x
                         Data x -> "data" .= toJSON x
        ]

data MessageBody a = Notification a | Data a

data Priority = Low | High

instance ToJSON Priority where
    toJSON Low = String "low"
    toJSON High = String "high"

data Limit = LimitToFirst Int | LimitToLast Int

data Key = forall a. ToJSON a => Key a

omitNulls :: [(T.Text, Value)] -> Value
omitNulls = object . filter notNull where
    notNull (_, Null) = False
    notNull _         = True  

makeClassy ''Firebase
