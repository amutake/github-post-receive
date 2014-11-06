{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Github.PostReceive.Types
    ( Payload (..)
    , PushEvent (..)
    , StatusEvent (..)
    , Commit (..)
    , Repository (..)
    , User (..)
    , SimpleUser (..)
    , Branch (..)
    , SimpleCommit (..)
    , StatusCommit (..)
    , SimpleStatusCommit (..)
    , Tree (..)
    , Or (..)
    , toEither
      -- Re-exports
    , EmailAddress
    ) where

import Control.Applicative ((<$>), (<*>), pure, (<|>))
import Data.Aeson (Value (..), FromJSON (..), (.:), (.:?))
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Text.Email.Validate (EmailAddress, emailAddress)

data Payload = Push PushEvent
             | Status StatusEvent
             deriving (Show, Eq, Typeable)

instance FromJSON Payload where
    parseJSON v = Push <$> parseJSON v
              <|> Status <$> parseJSON v

data PushEvent = PushEvent
    { pushEventRef :: Text
    , pushEventBefore :: Text
    , pushEventAfter :: Text
    , pushEventCreated :: Bool
    , pushEventDeleted :: Bool
    , pushEventForced :: Bool
    , pushEventBaseRef :: Maybe Text
    , pushEventCompare :: Url
    , pushEventCommits :: [Commit]
    , pushEventHeadCommit :: Commit
    , pushEventRepository :: Repository
    , pushEventPusher :: SimpleUser
    , pushEventSender :: User
    } deriving (Show, Eq, Typeable)

instance FromJSON PushEvent where
    parseJSON (Object o) = PushEvent
        <$> o .: "ref"
        <*> o .: "before"
        <*> o .: "after"
        <*> o .: "created"
        <*> o .: "deleted"
        <*> o .: "forced"
        <*> o .:? "base_ref"
        <*> o .: "compare"
        <*> o .: "commits"
        <*> o .: "head_commit"
        <*> o .: "repository"
        <*> o .: "pusher"
        <*> o .: "sender"
    parseJSON _ = fail "PushEvent must be an object"

data StatusEvent = StatusEvent
    { statusEventId :: Int
    , statusEventSHA :: Text
    , statusEventName :: Text
    , statusEventTargetUrl :: Url
    , statusEventContext :: Text
    , statusEventDescription :: Text
    , statusEventState :: Text
    , statusEventCommit :: StatusCommit
    , statusEventBranches :: [Branch]
    , statusEventCreatedAt :: Text -- TODO: Change to date type
    , statusEventUpdatedAt :: Text -- TODO: Change to date type
    , statusEventRepository :: Repository
    , statusEventSender :: User
    } deriving (Show, Eq, Typeable)

instance FromJSON StatusEvent where
    parseJSON (Object o) = StatusEvent
        <$> o .: "id"
        <*> o .: "sha"
        <*> o .: "name"
        <*> o .: "target_url"
        <*> o .: "context"
        <*> o .: "description"
        <*> o .: "state"
        <*> o .: "commit"
        <*> o .: "branches"
        <*> o .: "created_at"
        <*> o .: "updated_at"
        <*> o .: "repository"
        <*> o .: "sender"
    parseJSON _ = fail "StatusEvent must be an object"

data Commit = Commit
    { commitId :: Text
    , commitDistinct :: Bool
    , commitMessage :: Text
    , commitTimestamp :: Text
    , commitUrl :: Url
    , commitAuthor :: SimpleUser
    , commitCommitter :: SimpleUser
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
    , repoFullName :: Text
    , repoOwner :: Or SimpleUser User
    , repoPrivate :: Bool
    , repoHtmlUrl :: Url
    , repoDescription :: Text
    , repoFork :: Bool
      -- urls
    , repoUrl :: Text
    , repoForksUrl :: Url
    , repoKeysUrl :: Url
    , repoCollaboratorsUrl :: Url
    , repoTeamsUrl :: Url
    , repoHooksUrl :: Url
    , repoIssueEventsUrl :: Url
    , repoEventsUrl :: Url
    , repoAssigneesUrl :: Url
    , repoBranchesUrl :: Url
    , repoTagsUrl :: Url
    , repoBlobsUrl :: Url
    , repoGitTagsUrl :: Url
    , repoGitRefsUrl :: Url
    , repoTreesUrl :: Url
    , repoStatusesUrl :: Url
    , repoLanguagesUrl :: Url
    , repoStargazersUrl :: Url
    , repoContributorsUrl :: Url
    , repoSubscribersUrl :: Url
    , repoSubscriptionUrl :: Url
    , repoCommitsUrl :: Url
    , repoGitCommitsUrl :: Url
    , repoIssueCommentUrl :: Url
    , repoContentsUrl :: Url
    , repoCompareUrl :: Url
    , repoMergesUrl :: Url
    , repoArchiveUrl :: Url
    , repoDownloadsUrl :: Url
    , repoIssuesUrl :: Url
    , repoPullsUrl :: Url
    , repoMilestonesUrl :: Url
    , repoNotificationsUrl :: Url
    , repoLabelsUrl :: Url
    , repoReleasesUrl :: Url
      -- date
    , repoCreatedAt :: Or Int Text -- Int or DateString
    , repoUpdatedAt :: Text
    , repoPushedAt :: Or Int Text -- Int or DateString
    , repoGitUrl :: Url
    , repoSshUrl :: Url
    , repoCloneUrl :: Url
    , repoSvnUrl :: Url
    , repoHomepage :: Maybe Url
    , repoSize :: Int
    , repoStargazersCount :: Int
    , repoWatchersCount :: Int
    , repoLanguage :: Text
    , repoHasIssues :: Bool
    , repoHasDownloads :: Bool
    , repoHasWiki :: Bool
    , repoHasPages :: Bool
    , repoForksCount :: Int
    , repoMirrorUrl :: Maybe Url
    , repoOpenIssuesCount :: Int
      -- for compatiblity?
    , repoForks :: Int
    , repoOpenIssues :: Int
    , repoWatchers :: Int
    , repoStargazers :: Maybe Int
    , repoMasterBranch :: Maybe Text
    } deriving (Show, Eq, Typeable)

instance FromJSON Repository where
    parseJSON (Object o) = Repository
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "full_name"
        <*> o .: "owner"
        <*> o .: "private"
        <*> o .: "html_url"
        <*> o .: "description"
        <*> o .: "fork"
        <*> o .: "url"
        <*> o .: "forks_url"
        <*> o .: "keys_url"
        <*> o .: "collaborators_url"
        <*> o .: "teams_url"
        <*> o .: "hooks_url"
        <*> o .: "issue_events_url"
        <*> o .: "events_url"
        <*> o .: "assignees_url"
        <*> o .: "branches_url"
        <*> o .: "tags_url"
        <*> o .: "blobs_url"
        <*> o .: "git_tags_url"
        <*> o .: "git_refs_url"
        <*> o .: "trees_url"
        <*> o .: "statuses_url"
        <*> o .: "languages_url"
        <*> o .: "stargazers_url"
        <*> o .: "contributors_url"
        <*> o .: "subscribers_url"
        <*> o .: "subscription_url"
        <*> o .: "commits_url"
        <*> o .: "git_commits_url"
        <*> o .: "issue_comment_url"
        <*> o .: "contents_url"
        <*> o .: "compare_url"
        <*> o .: "merges_url"
        <*> o .: "archive_url"
        <*> o .: "downloads_url"
        <*> o .: "issues_url"
        <*> o .: "pulls_url"
        <*> o .: "milestones_url"
        <*> o .: "notifications_url"
        <*> o .: "labels_url"
        <*> o .: "releases_url"
        <*> o .: "created_at"
        <*> o .: "updated_at"
        <*> o .: "pushed_at"
        <*> o .: "git_url"
        <*> o .: "ssh_url"
        <*> o .: "clone_url"
        <*> o .: "svn_url"
        <*> o .:? "homepage"
        <*> o .: "size"
        <*> o .: "stargazers_count"
        <*> o .: "watchers_count"
        <*> o .: "language"
        <*> o .: "has_issues"
        <*> o .: "has_downloads"
        <*> o .: "has_wiki"
        <*> o .: "has_pages"
        <*> o .: "forks_count"
        <*> o .:? "mirror_url"
        <*> o .: "open_issues_count"
        <*> o .: "forks"
        <*> o .: "open_issues"
        <*> o .: "watchers"
        <*> o .:? "stargazers"
        <*> o .:? "master_branch"
    parseJSON _ = fail "Repository must be an object"

data User = User
    { userLogin :: Text
    , userId :: Int
    , userAvatarUrl :: Url
    , userGravatarId :: Text
    , userUrl :: Url
    , userHtmlUrl :: Url
    , userFollowersUrl :: Url
    , userFollowingUrl :: Url
    , userGistsUrl :: Url
    , userStarredUrl :: Url
    , userSubscriptionsUrl :: Url
    , userOrganizationsUrl :: Url
    , userReposUrl :: Url
    , userEventsUrl :: Url
    , userReceivedEventsUrl :: Url
    , userType :: Text
    , userSiteAdmin :: Bool
    } deriving (Show, Eq, Typeable)

instance FromJSON User where
    parseJSON (Object o) = User
        <$> o .: "login"
        <*> o .: "id"
        <*> o .: "avatar_url"
        <*> o .: "gravatar_id"
        <*> o .: "url"
        <*> o .: "html_url"
        <*> o .: "followers_url"
        <*> o .: "following_url"
        <*> o .: "gists_url"
        <*> o .: "starred_url"
        <*> o .: "subscriptions_url"
        <*> o .: "organizations_url"
        <*> o .: "repos_url"
        <*> o .: "events_url"
        <*> o .: "received_events_url"
        <*> o .: "type"
        <*> o .: "site_admin"
    parseJSON _ = fail "User must be an object"

data SimpleUser = SimpleUser
    { simpleUserName :: Text
    , simpleUserEmail :: Maybe EmailAddress
    , simpleUserUsername :: Maybe Text
    , simpleUserDate :: Maybe Text
    } deriving (Show, Eq, Typeable)

instance FromJSON SimpleUser where
    parseJSON (Object o) = SimpleUser
        <$> o .: "name"
        <*> o .:? "email"
        <*> o .:? "username"
        <*> o .:? "date"
    parseJSON _ = fail "SimpleUser must be an object"

instance FromJSON EmailAddress where
    parseJSON (String t) = case emailAddress $ B.pack . T.unpack $ t of
        Just a -> pure a
        Nothing -> fail "failed to parse EmailAddress"
    parseJSON _ = fail "EmailAddress must be a text"

data Branch = Branch
    { branchName :: Text
    , branchCommit :: SimpleCommit
    } deriving (Show, Eq, Typeable)

instance FromJSON Branch where
    parseJSON (Object o) = Branch
        <$> o .: "name"
        <*> o .: "commit"
    parseJSON _ = fail "Branch must be an object"

data SimpleCommit = SimpleCommit
    { simpleCommitSha :: Text
    , simpleCommitUrl :: Url
    , simpleCommitHtmlUrl :: Maybe Url
    } deriving (Show, Eq, Typeable)

instance FromJSON SimpleCommit where
    parseJSON (Object o) = SimpleCommit
        <$> o .: "sha"
        <*> o .: "url"
        <*> o .:? "html_url"
    parseJSON _ = fail "SimpleCommit must be an object"

-- | used in StatusEvent
data StatusCommit = StatusCommit
    { statusCommitSHA :: Text
    , statusCommitCommit :: SimpleStatusCommit
    , statusCommitUrl :: Url
    , statusCommitHtmlUrl :: Url
    , statusCommitCommentsUrl :: Url
    , statusCommitAuthor :: User
    , statusCommitCommitter :: User
    , statusCommitParents :: [SimpleCommit]
    } deriving (Show, Eq, Typeable)

instance FromJSON StatusCommit where
    parseJSON (Object o) = StatusCommit
        <$> o .: "sha"
        <*> o .: "commit"
        <*> o .: "url"
        <*> o .: "html_url"
        <*> o .: "comment_url"
        <*> o .: "author"
        <*> o .: "committer"
        <*> o .: "parents"
    parseJSON _ = fail "StatusCommit must be an object"

data SimpleStatusCommit = SimpleStatusCommit
    { simpleStatusCommitAuthor :: SimpleUser
    , simpleStatusCommitCommitter :: SimpleUser
    , simpleStatusCommitMessage :: Text
    , simpleStatusCommitTree :: Tree
    , simpleStatusCommitUrl :: Url
    , simpleStatusCommitCommentCount :: Int
    } deriving (Show, Eq, Typeable)

instance FromJSON SimpleStatusCommit where
    parseJSON (Object o) = SimpleStatusCommit
        <$> o .: "author"
        <*> o .: "committer"
        <*> o .: "message"
        <*> o .: "tree"
        <*> o .: "url"
        <*> o .: "comment_count"
    parseJSON _ = fail "SimpleStatusCommit must be an object"

data Tree = Tree
    { treeSHA :: Text
    , treeUrl :: Url
    } deriving (Show, Eq, Typeable)

instance FromJSON Tree where
    parseJSON (Object o) = Tree
        <$> o .: "sha"
        <*> o .: "url"
    parseJSON _ = fail "Tree must be an object"

type Url = Text

-- | Or a b represents a or b
-- The reason why we don't use Either type is that Either Int String type parses { "left": 1 } or { "right": "foo" }, but we want to parse 1 or "foo".
data Or a b = L a | R b deriving (Show, Eq, Typeable)

instance (FromJSON a, FromJSON b) => FromJSON (Or a b) where
    parseJSON v = L <$> parseJSON v <|> R <$> parseJSON v

toEither :: Or a b -> Either a b
toEither (L a) = Left a
toEither (R b) = Right b
