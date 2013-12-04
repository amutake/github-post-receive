{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Github.PostReceive.Types
    ( Payload (..)
    , Commit (..)
    , Repository (..)
    , User (..)
      -- Re-exports
    , EmailAddress
    ) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Aeson (Value (..), FromJSON (..), (.:), (.:?))
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Text.Email.Validate (EmailAddress, emailAddress)

data Payload = Payload
    { payloadRef :: Text
    , payloadAfter :: Text
    , payloadBefore :: Text
    , payloadCreated :: Bool
    , payloadDeleted :: Bool
    , payloadForced :: Bool
    , payloadCompare :: Text
    , payloadCommits :: [Commit]
    , payloadHeadCommit :: Commit
    , payloadRepository :: Repository
    , payloadPusher :: User
    } deriving (Show, Eq, Typeable)

instance FromJSON Payload where
    parseJSON (Object o) = Payload
        <$> o .: "ref"
        <*> o .: "after"
        <*> o .: "before"
        <*> o .: "created"
        <*> o .: "deleted"
        <*> o .: "forced"
        <*> o .: "compare"
        <*> o .: "commits"
        <*> o .: "head_commit"
        <*> o .: "repository"
        <*> o .: "pusher"
    parseJSON _ = fail "Payload must be an object"

data Commit = Commit
    { commitId :: Text
    , commitDistinct :: Bool
    , commitMessage :: Text
    , commitTimestamp :: Text
    , commitUrl :: Text
    , commitAuthor :: User
    , commitCommitter :: User
    , commitAdded :: [FilePath]
    , commitRemoved :: [FilePath]
    , commitModified :: [FilePath]
    } deriving (Show, Eq, Typeable)

instance FromJSON Commit where
    parseJSON (Object o) = Commit
        <$> o .: "id"
        <*> o .: "distinct"
        <*> o .: "message"
        <*> o .: "timestamp"
        <*> o .: "url"
        <*> o .: "author"
        <*> o .: "committer"
        <*> o .: "added"
        <*> o .: "removed"
        <*> o .: "modified"
    parseJSON _ = fail "Commit must be an object"

data Repository = Repository
    { repoId :: Int
    , repoName :: Text
    , repoUrl :: Text
    , repoDescription :: Text
    , repoHomepage :: Maybe Text
    , repoWatchers :: Int
    , repoStargazers :: Int
    , repoForks :: Int
    , repoFork :: Bool
    , repoSize :: Int
    , repoOwner :: User
    , repoPrivate :: Bool
    , repoOpenIssues :: Int
    , repoHasIssues :: Bool
    , repoHasDownloads :: Bool
    , repoHasWiki :: Bool
    , repoLanguage :: Text
    , repoCreatedAt :: Int
    , repoPushedAt :: Int
    , repoMasterBranch :: Text
    } deriving (Show, Eq, Typeable)

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

data User = User
    { userName :: Text
    , userEmail :: Maybe EmailAddress
    , userUsername :: Maybe Text
    } deriving (Show, Eq, Typeable)

instance FromJSON User where
    parseJSON (Object o) = User
        <$> o .: "name"
        <*> o .:? "email"
        <*> o .:? "username"
    parseJSON _ = fail "User must be an object"

instance FromJSON EmailAddress where
    parseJSON (String t) = case emailAddress $ B.pack . T.unpack $ t of
        Just a -> pure a
        Nothing -> fail "failed to parse EmailAddress"
    parseJSON _ = fail "EmailAddress must be a text"
