{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, OverloadedStrings #-}

module Message where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Data.Data

import Util

type HashString = String
type URL = String
type Time = String
type Language = String

data User = User
    { userName :: String
    , userEmail :: Maybe String
    , userUsername :: Maybe String
    } deriving (Show, Read, Eq, Ord, Data, Typeable)

instance FromJSON User where
    parseJSON (Object o) = User
        <$> o .: "name"
        <*> o .:? "email"
        <*> o .:? "username"
    parseJSON _ = fail "User must be an object"

data Repository = Repository
    { repoId :: Int
    , repoName :: String
    , repoUrl :: URL
    , repoDescription :: String
    , repoHomepage :: Maybe URL
    , repoWatchers :: Int
    , repoStargazers :: Int
    , repoForks :: Int
    , repoFork :: Bool
    , repoSize :: Int
    , repoOwner :: User
    , repoPrivate :: Bool
    , repoOpen_issues :: Int
    , repoHas_issues :: Bool
    , repoHas_downloads :: Bool
    , repoHas_wiki :: Bool
    , repoLanguage :: Language
    , repoCreated_at :: Int
    , repoPushed_at :: Int
    , repoMaster_branch :: String
    } deriving (Show, Read, Eq, Ord, Data, Typeable)

instance FromJSON Repository where
    parseJSON (Object o) = Repository
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "url"
        <*> o .: "description"
        <*> o .:? "homepage"
        <*> o .: "watchers"
        <*> o .: "stargazers"
        <*> o .: "forks"
        <*> o .: "fork"
        <*> o .: "size"
        <*> o .: "owner"
        <*> o .: "private"
        <*> o .: "open_issues"
        <*> o .: "has_issues"
        <*> o .: "has_downloads"
        <*> o .: "has_wiki"
        <*> o .: "language"
        <*> o .: "created_at"
        <*> o .: "pushed_at"
        <*> o .: "master_branch"
    parseJSON _ = fail "Repository must be an object"

data Commit = Commit
    { commitId :: HashString
    , commitDistinct :: Bool
    , commitMessage :: String
    , commitTimestamp :: Time
    , commitUrl :: URL
    , commitAuthor :: User
    , commitCommitter :: User
    , commitAdded :: [FilePath]
    , commitRemoved :: [FilePath]
    , commitModified :: [FilePath]
    } deriving (Show, Read, Eq, Ord, Data, Typeable)
deriveFromJSON (dataFieldToKeyName "commit") ''Commit

data Message = Message
    { messageRef :: FilePath
    , messageAfter :: HashString
    , messageBefore :: HashString
    , messageCreated :: Bool
    , messageDeleted :: Bool
    , messageForced :: Bool
    , messageCompare :: URL
    , messageCommits :: [Commit]
    , messageHead_commit :: Commit
    , messageRepository :: Repository
    , messagePusher :: User
    } deriving (Show, Read, Eq, Ord, Data, Typeable)
deriveFromJSON (dataFieldToKeyName "message") ''Message
