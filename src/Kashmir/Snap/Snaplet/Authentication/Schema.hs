{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Kashmir.Snap.Snaplet.Authentication.Schema where

import           Control.Lens                (view)
import           Control.Monad
import           Control.Monad.IO.Class
import           Crypto.BCrypt
import           Data.Aeson.TH               (deriveJSON)
import           Data.Maybe
import           Data.Text                   hiding (head)
import           Data.Text.Encoding
import           Data.Time.Clock
import           Database.Esqueleto
import           Database.Persist.TH
import           GHC.Generics                (Generic)
import           Kashmir.Aeson
import           Kashmir.Database.Postgresql
import           Kashmir.Github              as Github
import           Kashmir.UUID
import           Prelude                     hiding (id)

share [mkPersist sqlSettings,mkMigrate "migrateAccounts"]
      [persistLowerCase|
  Account
    accountId UUID sqltype=uuid default=uuid_generate_v4()
    created UTCTime default=now()
    Primary accountId
    deriving Read Show Eq Generic

  PasswordAccount
    accountId UUID sqltype=uuid
    username Text sqltype=text
    password Text sqltype=text
    Primary username
    deriving Read Show Eq Generic

  GithubAccount
    githubId Int
    accountId AccountId sqltype=uuid
    login Text
    email Text Maybe
    blog Text Maybe
    accessToken AccessToken sqltype=text
    Primary githubId
    Unique AccountIdUnique accountId
    deriving Read Show Eq Generic
  |]

$(deriveJSON (dropPrefixJSONOptions "account")
             ''Account)

-- TODO Update more details.
createOrUpdateGithubUser :: UUID -> UTCTime -> AccessToken -> Github.User -> SqlPersistM (Key Account)
createOrUpdateGithubUser uuid created theToken githubUser =
  let savepointName = "upsert_github"
  in do void $ createSavepoint savepointName
        accountKey <-
          insert $
          Account uuid created
        maybeGithubKey <-
          insertUnlessDuplicate
            GithubAccount {githubAccountGithubId =
                             view id githubUser
                          ,githubAccountAccountId = accountKey
                          ,githubAccountAccessToken = theToken
                          ,githubAccountLogin =
                             view login githubUser
                          ,githubAccountBlog =
                             view blog githubUser
                          ,githubAccountEmail =
                             view email githubUser}
        case maybeGithubKey of
          Just _ -> releaseSavepoint savepointName >> return accountKey
          Nothing ->
            do let match g =
                     where_ (g ^. GithubAccountGithubId ==.
                             val (view id githubUser))
               void $ rollbackToSavepoint savepointName
               update $
                 \g ->
                   do set g
                          [GithubAccountAccessToken =. val theToken
                          ,GithubAccountBlog =.
                           val (view blog githubUser)
                          ,GithubAccountEmail =.
                           val (view email githubUser)
                          ,GithubAccountLogin =.
                           val (view login githubUser)]
                      match g
               accountIds <-
                 select . from $
                 \g ->
                   do match g
                      return (g ^. GithubAccountAccountId)
               return . unValue . head $ accountIds

createPasswordUser :: UUID -> UTCTime -> Text -> Text -> SqlPersistM (Key Account)
createPasswordUser uuid created username password =
  do Just hashedPassword <-
       liftIO $ hashPasswordUsingPolicy fastBcryptHashingPolicy
                                            (encodeUtf8 password)
     accountKey <-
          insert $
          Account uuid created
     _ <-
          insert $
          PasswordAccount {passwordAccountAccountId = unAccountKey accountKey
                          ,passwordAccountUsername = username
                          ,passwordAccountPassword = decodeUtf8 hashedPassword}
     return accountKey
