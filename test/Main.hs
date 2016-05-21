{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Database.Firebase
import Network.HTTP.Nano
import System.Environment

type TestM = ReaderT TestEnv (ExceptT HttpError IO)

data TestEnv = TestEnv {
    testFB :: Firebase,
    testHttp :: HttpCfg
}

instance HasHttpCfg TestEnv where
    httpCfg = lens testHttp (\te h -> te { testHttp = h })

instance HasFirebase TestEnv where
    firebase = lens testFB (\te f -> te { testFB = f })

main :: IO ()
main = do
    [tok,url] <- getArgs
    let fb = Firebase tok url
    mgr <- tlsManager
    let httpc = HttpCfg mgr
    let te = TestEnv fb httpc
    res <- runExceptT $ flip runReaderT te test
    liftIO . putStrLn $ show res

test :: TestM ()
test = do
    put "sandbox" $ object ["val" .= ("test value" :: String)]
    r <- (get "sandbox" :: TestM Value)
    liftIO $ putStrLn (show r)
