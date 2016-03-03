{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Kashmir.Github.Types.Hook where

import           Cases
import           Data.Aeson
import           Data.Set                    as Set
import           Data.Text
import           GHC.Generics
import           Kashmir.Github.Types.Common

asSnakeString :: Show a
              => a -> Value
asSnakeString = String . snakify . pack . show

data ContentType
  = Json
  | Form
  deriving (Show,Eq,Generic)

instance ToJSON ContentType where
  toJSON = asSnakeString

data HookConfig =
  HookConfig {url         :: URL
             ,contentType :: ContentType
             ,secret      :: Maybe Text
             ,insecureSsl :: Bool}
  deriving (Show,Eq,Generic,ToJSON)

data HookName = Web
  deriving (Show,Eq,Generic)

instance ToJSON HookName where
  toJSON = asSnakeString

data HookEvent
  = Push
  | PullRequest
  deriving (Show,Eq,Ord,Generic)

instance ToJSON HookEvent where
  toJSON = asSnakeString

data Hook =
  Hook {name   :: HookName
       ,config :: HookConfig
       ,events :: Set HookEvent
       ,active :: Bool}
  deriving (Show,Eq,Generic,ToJSON)
