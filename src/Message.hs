{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, OverloadedStrings #-}

module Message where

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
deriveJSON (dataFieldToKeyName "user") ''User

data Repository = Repository
    { repoId :: Int
    , repoName :: String
    , repoUrl :: URL
    , repoDescription :: String
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
deriveJSON (dataFieldToKeyName "repo") ''Repository

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
deriveJSON (dataFieldToKeyName "commit") ''Commit

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
deriveJSON (dataFieldToKeyName "message") ''Message
