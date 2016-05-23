{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens.Lens
import Control.Monad
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
    case res of
        Left e -> error $ show e
        Right r -> return ()

test :: TestM ()
test = do
    put "a" $ object ["val" .= ("test value" :: String)]
    ra <- (get "a" Nothing :: TestM Value)
    liftIO $ putStrLn (show ra)
    mapM_ (post "b") ([1..10] :: [Int])
    rb <- (get "b" Nothing :: TestM Value)
    liftIO $ putStrLn (show rb)
    put "c" $ object ["a" .= ("va" :: String), "b" .= ("vb" :: String)]
    patch "c" $ object ["a" .= ("va_patched" :: String)]
    rx <- (get "" (Just $ query { orderBy = Just "$key", startAt = Just $ key ("c" :: String) }) :: TestM Value)
    liftIO $ putStrLn (show rx)
