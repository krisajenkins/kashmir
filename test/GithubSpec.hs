{-# LANGUAGE OverloadedStrings #-}
module GithubSpec where

import           Control.Lens
import           Data.Monoid
import           Data.Yaml
import           Kashmir.Github
import           Kashmir.Github.Types.Organization as GOrg
import           Kashmir.Github.Types.User         as GUser
import           Test.Hspec
import           Test.QuickCheck.Instances         ()

spec :: Spec
spec = do userSpec
          organisationsSpec

userSpec :: Spec
userSpec =
  describe "User fetching" $
  do it "Fetches the current user." $
       do user <- loadToken >>= getUser
          print user

organisationsSpec :: Spec
organisationsSpec =
  describe "Organisations fetching" $
  do it "Fetches the current organisations." $
       do orgs <- loadToken >>= getOrganizations
          print (view GOrg.login <$> orgs)

loadConfig :: IO (Either ParseException AccessToken)
loadConfig =
  do let configFile = "kashmir.yaml"
     putStrLn $ "Reading config: " <> configFile
     decodeFileEither configFile

loadToken :: IO AccessToken
loadToken =
  do config <- loadConfig
     case config of
       Left e -> fail (show e)
       Right aToken -> return aToken
