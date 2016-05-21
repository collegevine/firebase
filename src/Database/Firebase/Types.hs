{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Firebase.Types where

import Control.Applicative ((<$>))
import Control.Lens.TH
import Control.Monad (mzero)
import Data.Aeson

type Location = String
type FBID = String

data Firebase = Firebase {
    _firebaseToken :: String,
    _firebaseURL :: String
}

newtype Name = Name { unName :: String } deriving Show

instance FromJSON Name where
    parseJSON (Object v) = Name <$> v .: "name"
    parseJSON _ = mzero

makeClassy ''Firebase
