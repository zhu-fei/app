{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Main where

import Model
import Site

import Web.Spock.Safe
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import qualified Data.Text as T

-- ?:
import Database.Persist.Sqlite hiding (get)

import Control.Monad
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as TL
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Bootstrap as H

import Web.Spock.Digestive
import qualified Text.Digestive as FF
import qualified Text.Digestive.Bootstrap as F

------------------------------------------
-- | Main:
------------------------------------------

main :: IO ()
main = do
    pool <- runNoLoggingT $ createSqlitePool "database.db" 5
    runNoLoggingT $ runSqlPool (runMigration migrateModel) pool
    app pool

------------------------------------------
-- | App:
------------------------------------------

app :: ConnectionPool -> IO ()
app pool = runSpock 4444 $ spock (defaultSpockCfg Nothing (PCPool pool) ()) $ do

    -- | Middleware:
    middleware logStdoutDev
    middleware (staticPolicy (addBase "static"))

    -- | Routes:
    get root homePage
    get recordRoute recordPage
    get secretRoute secretPage

    -- | Auth:
    getpost "/login" loginAction
    get "logout" $ writeSession Nothing >> redirect "/"

------------------------------------------
-- | Routes:
------------------------------------------

recordRoute :: Path '[Int]
recordRoute = "record" <//> var

secretRoute :: Path '[]
secretRoute = "secret"

------------------------------------------
-- | Pages:
------------------------------------------

homePage :: MonadIO m => ActionT m ()
homePage = blaze $ H.h1 "the home page"

recordPage :: MonadIO m => Int -> ActionT m ()
recordPage id = text $ pack $ show id

secretPage :: MonadIO m => ActionT m ()
secretPage = html "<h1>the secret page</h1>"

-- secretPage :: SpockAction conn Session st ()
-- secretPage = do
--     sess <- readSession
--     when (isNothing sess) $ redirect "/login"
--     site $ H.h1 "the secret"

-- secretPage :: SpockAction conn Session st ()
-- secretPage = do
--     records <- runSQL $ selectList [] []
--     site $
--         do H.h1 "records"
--             H.ul $
--                 forM_ records $ \record ->
--                 H.li $ H.toHtml (recordTitle $ entityVal record)



------------------------------------------
-- | Actions:
------------------------------------------

loginAction :: SpockAction conn Session st ()
loginAction = do
    let formView = F.renderForm loginFormSpec
    f <- runForm "loginForm" loginForm
    case f of
        (view, Nothing) ->
            site $ formView view
        (view, Just loginReq) ->
            if lrUser loginReq == "admin" && lrPassword loginReq == "assword"
                then do sessionRegenerateId
                        writeSession (Just $ lrUser loginReq)
                        redirect "/secret"
                else site $
                     do H.alertBox H.BootAlertDanger "login failed"
                        formView view

type Username = T.Text
type Session = Maybe Username

site :: H.Html -> SpockAction conn Session st ()
site ct =
    do sess <- readSession
       let sv = SiteView sess
       blaze $ siteView sv ct

------------------------------------------
-- | Forms:
------------------------------------------

data LoginRequest = LoginRequest
    { lrUser :: T.Text
    , lrPassword :: T.Text
    } deriving (Show)

loginFormSpec :: F.FormMeta
loginFormSpec = F.FormMeta
    { F.fm_method = POST
    , F.fm_target = "/login"
    , F.fm_elements =
        [ F.FormElement "name" (Just "Username") F.InputText
        , F.FormElement "password" (Just "Password") F.InputPassword
        ]
    , F.fm_submitText = "Login"
    }

------------------------------------------
-- | Validation:
------------------------------------------

minMaxLen :: (Int, Int) -> T.Text -> FF.Result H.Html T.Text
minMaxLen (minLen, maxLen) t =
    if len >= minLen && len <= maxLen
        then FF.Success stripped
        else FF.Error $ H.toHtml $
            "Must be longer than " ++ show minLen
            ++ " and shorter than "
            ++ show maxLen ++ " characters"
    where
        stripped = T.strip t
        len = T.length stripped

loginForm :: Monad m => FF.Form H.Html m LoginRequest
loginForm = LoginRequest
    <$> "name" FF..: FF.validate (minMaxLen(3, 12)) (FF.text Nothing)
    <*> "password" FF..: FF.validate (minMaxLen(6, 20)) (FF.text Nothing)

------------------------------------------
-- | Units:
------------------------------------------

blaze :: MonadIO m => H.Html -> ActionT m ()
blaze = html . TL.toStrict . renderHtml

-- runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
-- runSQL action =
--     runQuery $ \conn ->
--         runResourceT $ runNoLoggingT $ runSqlConn action conn

-- checkSession :: SpockActionCtx ctx SqlBackend Session st ()
-- checkSession = do
--     sess <- readSession
--     -- when (isNothing sess) $ redirect "/login"
--     mUser <- getUserFromSession sess
--     case mUser of
--         Nothing -> text "Sorry, no access!"
--         Just user -> return (user :&: oldCtx)

------------------------------------------
-- | The End.
------------------------------------------
