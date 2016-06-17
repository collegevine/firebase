{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Database.Firebase.Types where

import Control.Applicative ((<$>))
import Control.Lens.TH
import Control.Monad (mzero)
import Control.Monad.Reader  (MonadReader)
import Control.Monad.Except (MonadError)
import Network.HTTP.Nano
import Control.Monad.Trans (MonadIO)
import Data.Aeson

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

data Limit = LimitToFirst Int | LimitToLast Int

data Key = forall a. ToJSON a => Key a

makeClassy ''Firebase
