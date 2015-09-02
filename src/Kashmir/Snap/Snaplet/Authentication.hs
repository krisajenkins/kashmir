{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Kashmir.Snap.Snaplet.Authentication (initAuthentication,Authentication,requireUser) where

import           Control.Applicative
import           Control.Lens                               (makeLenses, view)
import           Control.Monad.CatchIO                      hiding (Handler)
import           Control.Monad.IO.Class
import qualified Control.Monad.State.Class                  as State
import           Crypto.BCrypt
import           Data.Aeson.TH                              (deriveJSON)
import           Data.ByteString
import qualified Data.ByteString.Char8
import qualified Data.Map                                   as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text                                  (Text)
import           Data.Text.Encoding
import           Data.Time.Clock
import           Data.Typeable
import           Data.UUID
import           Data.Yaml
import           Database.Esqueleto
import qualified Database.Persist
import           GHC.Generics                               (Generic)
import           Kashmir.Aeson
import           Kashmir.Database.Postgresql
import qualified Kashmir.Github                             as Github
import           Kashmir.Snap.Snaplet.Authentication.Config
import           Kashmir.Snap.Snaplet.Authentication.Schema
import           Kashmir.Snap.Snaplet.Postgresql
import           Kashmir.Snap.Snaplet.Random
import           Kashmir.UUID
import           Kashmir.Web
import           Snap                                       hiding (Config,
                                                             with)
import qualified Snap
import           Snap.CORS
import           Web.JWT                                    as JWT hiding
                                                                    (header)

data Authentication =
  Authentication {_dbPool                :: Snaplet ConnectionPool
                 ,_randomNumberGenerator :: Snaplet RandomNumberGenerator
                 ,_systemConfig          :: Config}
makeLenses ''Authentication


data AuthenticationException
  = MissingParameter ByteString
  | AccountNotFoundException
  | Unauthenticated
  deriving (Show,Typeable)

instance Exception AuthenticationException


data AuthenticationOptions = AuthenticationOptions {_github :: String}
  deriving (Eq,Show,Generic)

$(deriveJSON (dropPrefixJSONOptions "_")
             ''AuthenticationOptions)

------------------------------------------------------------

-- TODO I don't like the string concatenation here. But don't use the Network.URL package. It has a dodgy API.
makeAuthenticationOptions :: Github.Config -> AuthenticationOptions
makeAuthenticationOptions config =
  AuthenticationOptions $
  mconcat [view Github.authUrl config
          ,"?scope=user:email&client_id="
          ,view Github.clientId config]

authRequestUrlsHandler :: Handler b Authentication ()
authRequestUrlsHandler =
  do githubConfig <- view (systemConfig . github)
     writeJSON $ makeAuthenticationOptions githubConfig

sessionCookieName :: ByteString
sessionCookieName = "sessionId"

sessionIdName :: Text
sessionIdName = "accountId"

makeSessionJSON :: Text -> Secret -> UUID -> JSON
makeSessionJSON currentHostname theSecret key =
  encodeSigned
    HS256
    theSecret
    (JWT.def {iss = stringOrURI currentHostname
             ,unregisteredClaims =
                Map.fromList [(sessionIdName,String (toText key))]})

makeSessionCookie :: Text -> Secret -> UTCTime -> UUID -> Cookie
makeSessionCookie currentHostname theSecret expires key =
  Cookie {cookieName = sessionCookieName
         ,cookieValue =
            encodeUtf8 $
            makeSessionJSON currentHostname theSecret key
         ,cookieExpires = Just expires
         ,cookieDomain = Nothing
         ,cookiePath = Just "/"
         ,cookieSecure = True
         ,cookieHttpOnly = False}

------------------------------------------------------------

readAuthToken :: Handler b Authentication (Maybe UUID)
readAuthToken =
  do jwtSecret <-
       secret <$>
       view (systemConfig . webserver . jwtSecretKey)
     maybeCookie <- getCookie sessionCookieName
     return $
       do authenticationCookie <- maybeCookie
          verifiedToken <-
            decodeAndVerifySignature
              jwtSecret
              (decodeUtf8 $ cookieValue authenticationCookie)
          let theClaims = unregisteredClaims $ claims verifiedToken
          sessionId <-
            Map.lookup sessionIdName theClaims
          case sessionId of
            (String s) -> fromText s
            _ -> Nothing

removeAuthToken :: Handler b v ()
removeAuthToken = Snap.expireCookie sessionCookieName Nothing

writeAuthToken :: UTCTime -> UUID -> Handler b Authentication ()
writeAuthToken expires accountId =
  do jwtKey <-
       secret <$>
       view (systemConfig . webserver . jwtSecretKey)
     currentHostname <-
       view (systemConfig . webserver . hostname)
     modifyResponse $
       Snap.addResponseCookie (makeSessionCookie currentHostname jwtKey expires accountId)

------------------------------------------------------------

twoWeeks :: NominalDiffTime
twoWeeks = 60 * 60 * 24 * 7 * 2

upsertAccount :: Github.Config
              -> ConnectionPool
              -> UUID
              -> ByteString
              -> IO (UTCTime,Key Account)
upsertAccount githubConfig connection uuid code =
  do accessToken <-
       view Github.accessToken <$>
       Github.requestAccess githubConfig code
     user <- Github.getUser accessToken
     now <- getCurrentTime
     accountKey <-
       runSqlPersistMPool (createOrUpdateGithubUser uuid now accessToken user)
                          connection
     return (now,accountKey)

processGithubAccessToken :: ByteString -> Handler b Authentication ()
processGithubAccessToken code =
  do githubConfig <- view (systemConfig . github)
     currentHostname <-
       view (systemConfig . webserver . hostname)
     uuid <-
       Snap.with randomNumberGenerator getRandom
     connection <- Snap.with dbPool State.get
     (now,accountKey) <-
       liftIO $
       upsertAccount githubConfig connection uuid code
     logError $
       "Upserted account key: " <>
       (toStrictByteString . unAccountKey) accountKey
     writeAuthToken (addUTCTime twoWeeks now)
                    (unAccountKey accountKey)
     redirect $ encodeUtf8 currentHostname

githubSignupHandler :: Handler b Authentication ()
githubSignupHandler =
  let parameterName = "code"
  in do code <- getParam parameterName
        logError $
          "Signup using code: " <>
          fromMaybe "<none>" code
        case code of
          Nothing ->
            throw $
            MissingParameter parameterName
          Just c -> processGithubAccessToken c

------------------------------------------------------------

lookupByUsername :: Text -> SqlPersistM [(Entity Account,Entity PasswordAccount)]
lookupByUsername username =
  select $
  from $
  \(account `InnerJoin` passwordAccount) ->
    do on $ account ^. AccountAccountId ==. passwordAccount ^. PasswordAccountAccountId
       where_ (passwordAccount ^. PasswordAccountUsername ==. val username)
       return (account,passwordAccount)

processUsernamePassword :: ByteString -> ByteString -> Handler b Authentication ()
processUsernamePassword username password =
  do connection <- Snap.with dbPool State.get
     -- Select account pair.
     currentHostname <-
       view (systemConfig . webserver . hostname)
     [(account,passwordAccount)] <-
       liftIO $
       runSqlPersistMPool (lookupByUsername (decodeUtf8 username))
                          connection
     now <- liftIO getCurrentTime
     -- Validate password.
     if validatePassword (encodeUtf8 (passwordAccountPassword $ entityVal passwordAccount))
                         password
        then do writeAuthToken (addUTCTime twoWeeks now)
                               (accountAccountId $ entityVal account)
                redirect $ encodeUtf8 currentHostname
        else throw Unauthenticated

usernamePasswordLoginHandler :: Handler b Authentication ()
usernamePasswordLoginHandler =
  method POST $
  do username :: Maybe ByteString <- getPostParam "username"
     password <- getPostParam "password"
     logError $ (encodeUtf8 "Username was ") <> (Data.ByteString.Char8.pack $ show username)
     case (username,password) of
       (Just u,Just p) ->
         processUsernamePassword u p
       _ ->
         do modifyResponse $
              setResponseStatus 400 "Missing parameters."
            response <- getResponse
            finishWith response

------------------------------------------------------------
------------------------------------------------------------
-- | Require that an authenticated AuthUser is present in the current session.
-- This function has no DB cost - only checks to see if a user_id is present in the current session.
requireUser :: SnapletLens v Authentication -> Handler b v a -> Handler b v a -> Handler b v a
requireUser lens bad good =
  do authToken <- Snap.with lens readAuthToken
     case authToken of
       Nothing -> bad
       Just _ -> good

------------------------------------------------------------

userDetailsHandler :: Handler b Authentication ()
userDetailsHandler =
  do logError "Looking up user details."
     connection <- Snap.with dbPool State.get
     authToken <- readAuthToken
     logError $
       "Got auth token: " <>
       maybe "<none>" toStrictByteString authToken
     case authToken of
       Nothing -> removeAuthToken >> pass
       Just accountId ->
         do account <-
              liftIO $
              runSqlPersistMPool
                (Database.Persist.get $
                 AccountKey accountId)
                connection
            case account of
              Nothing ->
                throw AccountNotFoundException
              Just a -> writeJSON a

initAuthentication :: Config -> SnapletInit b Authentication
initAuthentication config =
  makeSnaplet "authentication" "Authentication Snaplet" Nothing $
  do liftIO $
       runSql (view database config)
              (runMigration migrateAccounts)
     pool <-
       nestSnaplet "dbPool" dbPool $
       initDb (view database config)
     randomSnap <-
       nestSnaplet "random" randomNumberGenerator initRandom
     addRoutes [("/callback/github",githubSignupHandler)
               ,("/login",usernamePasswordLoginHandler)
               ,("/status"
                ,method GET (userDetailsHandler <|> authRequestUrlsHandler))]
     wrapSite $ applyCORS defaultOptions
     return Authentication {_dbPool = pool
                           ,_randomNumberGenerator = randomSnap
                           ,_systemConfig = config}
