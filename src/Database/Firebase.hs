
module Database.Firebase(
    module Database.Firebase.Types,
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
import Data.Aeson (FromJSON, ToJSON)
import Network.HTTP.Nano

get :: (MonadIO m, MonadError e m, MonadReader r m, AsHttpError e, HasFirebase r, HasHttpCfg r, FromJSON a) => Location -> m a
get loc = httpJSON =<< fbReq GET loc NoRequestData

put :: (MonadIO m, MonadError e m, MonadReader r m, AsHttpError e, HasFirebase r, HasHttpCfg r, ToJSON a) => Location -> a -> m ()
put loc dta = http' =<< fbReq PUT loc (mkJSONData dta)

post :: (MonadIO m, MonadError e m, MonadReader r m, AsHttpError e, HasFirebase r, HasHttpCfg r, ToJSON a) => Location -> a -> m FBID
post loc dta = unName <$> (httpJSON =<< fbReq POST loc (mkJSONData dta))

patch :: (MonadIO m, MonadError e m, MonadReader r m, AsHttpError e, HasFirebase r, HasHttpCfg r, ToJSON a) => Location -> a -> m ()
patch loc dta = http' =<< fbReq (CustomMethod "PATCH") loc (mkJSONData dta)

delete :: (MonadIO m, MonadError e m, MonadReader r m, AsHttpError e, HasFirebase r, HasHttpCfg r) => Location -> m ()
delete loc = http' =<< fbReq DELETE loc NoRequestData

fbReq :: (MonadIO m, MonadError e m, MonadReader r m, AsHttpError e, HasFirebase r, HasHttpCfg r) => HttpMethod -> Location -> RequestData -> m Request
fbReq mthd loc dta = do
    baseURL <- view firebaseURL
    token <- view firebaseToken
    let url = baseURL ++ loc ++ ".json?auth=" ++ token
    buildReq mthd url dta
