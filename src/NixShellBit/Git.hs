{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NixShellBit.Git
  ( GitError(..)
  , gitArchiveUrl
  , gitClone
  , gitDiscoverRepo
  , gitTaggedVersions
  , gitRemoteGetUrl
  , gitRemoteList
  , git_
  ) where

import Bindings.Libgit2     (C'git_strarray(C'git_strarray), C'git_remote,
                             C'git_repository, c'GIT_ENOTFOUND, c'GIT_OK,
                             c'giterr_last, c'git_error'message,
                             c'git_remote_free, c'git_remote_list,
                             c'git_remote_load, c'git_remote_url,
                             c'git_repository_discover, c'git_repository_free,
                             c'git_repository_open, withLibGitDo)
import Control.Applicative  ((<|>))
import Control.Exception    (finally)
import Control.Monad        (void, when, (>=>))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Attoparsec.Text (Parser, char, choice, inClass, maybeResult, option,
                             parse, takeWhile1)
import Data.List            (intercalate)
import Data.Maybe           (mapMaybe)
import Data.Text            (Text)
import Foreign              (Ptr, alloca, allocaBytes, fromBool, peek, peekArray,
                             sizeOf)
import Foreign.C.String     (peekCString, withCString)
import Foreign.C.Types      (CChar, CInt, CSize)
import System.Exit          (ExitCode(ExitSuccess))
import System.FilePath      (searchPathSeparator)
import System.Process.Typed (proc, readProcess, readProcess_)
import UnliftIO.Exception   (Exception, Typeable, throwIO)

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T


type CmdName = String
type ErrMsg = String

data GitError
  = GitError CmdName ErrMsg
  deriving (Eq, Show, Typeable)

instance Exception GitError


gitArchiveUrl
  :: Text
  -> Text
  -> Maybe Text
gitArchiveUrl remoteUrl ref =
  do
    (host, path) <- maybeResult (parse components remoteUrl)
    sub <- subPath host
    Just $ T.intercalate "/" ["https://" <> host, path, sub]
  where
    components :: Parser (Text, Text)
    components =
        choice [ssh, https]
      where
        ssh :: Parser (Text, Text)
        ssh =
          do
            _ <- "git" >> char '@'
            hostAndPath (char ':')

        https :: Parser (Text, Text)
        https =
          do
            _ <- "https" >> "://"
            _ <- option "" (takeWhile1 (inClass "a-zA-Z0-9_-") >> "@")
            hostAndPath (char '/')

        hostAndPath :: Parser Char -> Parser (Text, Text)
        hostAndPath sep =
          do
            h <- host
            _ <- sep
            p <- path
            _ <- ext
            pure (h, p)

        host :: Parser Text
        host = choice
             [ "github.com"
             , "gitlab.com"
             , "bitbucket.org"
             ]

        path :: Parser Text
        path = takeWhile1 (inClass "a-zA-Z0-9_/-")

        ext :: Parser Text
        ext = option "" ".git"

    subPath :: Text -> Maybe Text
    subPath "github.com"    = Just ("archive/" <> ref <> ".tar.gz")
    subPath "gitlab.com"    = Just ("get/" <> ref <> ".tar.gz")
    subPath "bitbucket.org" = Just ("repository/archive.tar.gz?ref=" <> ref)
    subPath _               = Nothing


gitDiscoverRepo
  :: FilePath
  -> [FilePath]
  -> IO (Maybe FilePath)
gitDiscoverRepo startPath ceilingDirs =
    withLibGitDo $
      withCString startPath $ \start_path   ->
      withCString ceiling'  $ \ceiling_dirs ->
      allocaBytes bytes     $ \path_out     ->
        do
          exit <- c'git_repository_discover path_out
                                            path_size
                                            start_path
                                            across_fs
                                            ceiling_dirs
          case exit of
            x | x == c'GIT_OK        -> Just <$> peekCString path_out
              | x == c'GIT_ENOTFOUND -> pure Nothing
              | otherwise            -> throwGit "c'git_repository_discover"
  where
    ceiling' :: String
    ceiling' = intercalate [searchPathSeparator] ceilingDirs

    bytes :: Int
    bytes = 256 * sizeOf (undefined :: CChar)

    path_size :: CSize
    path_size = fromIntegral bytes

    across_fs :: CInt
    across_fs = fromBool False


gitRemoteList :: FilePath -> IO [String]
gitRemoteList repoPath =
  withLibGitDo $
    withRepo repoPath $ \repo ->
    alloca            $ \out  ->
      do
        exit <- c'git_remote_list out repo
        checkError exit "c'git_remote_list"
        C'git_strarray cstrings count <- peek out
        peekArray (fromIntegral count) cstrings >>=
          traverse peekCString


{-| libgit2 v0.27 has a git_remote_create_detached()
    function that could emulate this `git ls-remote`
    call. If/when hlibgit2 gets updated, use that.

    https://github.com/libgit2/libgit2/pull/4233
-}
gitTaggedVersions
  :: Text
  -> String
  -> IO [String]
gitTaggedVersions url name =
    runMaybeT (lsRemote <|> lsRemoteBackCompat) >>= \case
      Just out -> (pure . mapMaybe v . BSL8.lines) out
      Nothing  -> throwIO (GitError "git ls-remote" "failed")
  where
    lsRemote :: MaybeT IO BSL8.ByteString
    lsRemote =
      MaybeT $ git_ls_remote ["--tags", "--sort=version:refname"]

    lsRemoteBackCompat :: MaybeT IO BSL8.ByteString
    lsRemoteBackCompat =
      MaybeT $ git_ls_remote ["--tags"]

    git_ls_remote :: [String] -> IO (Maybe BSL8.ByteString)
    git_ls_remote opts =
      readProcess (proc "git" ("ls-remote" : opts ++ args)) >>= \case
        (ExitSuccess, out, _) -> pure (Just out)
        _                     -> pure Nothing

    args :: [String]
    args = [T.unpack url, name ++ "-*"]

    v :: BSL8.ByteString -> Maybe String
    v = fmap BS8.unpack
      . BS8.stripPrefix prefix
      . snd
      . BS8.breakSubstring prefix
      . BSL8.toStrict

    prefix :: BS8.ByteString
    prefix = "refs/tags/" <> BS8.pack name <> "-"


{-| hlibgit2 has a c'git_clone binding but using it would mean
    allocating and initializing some large structs, namely
    C'git_clone_options and C'git_clone_options.

    We only use this function in cases when an archive URL could
    not be determined, such as when pointing at a local repository.
-}
gitClone
  :: Text
  -> FilePath
  -> Text
  -> IO ()
gitClone url localPath ref =
    mapM_ git_ [clone, checkout]
  where
    clone = ["clone", "--quiet", "--template", "", T.unpack url, localPath]

    checkout = ["-C", localPath, "checkout", "--quiet", T.unpack ref]


git_ :: [String] -> IO ()
git_ = void . readProcess_ . proc "git"


gitRemoteGetUrl
  :: FilePath
  -> String
  -> IO String
gitRemoteGetUrl repoPath name =
  withLibGitDo $
    withRepo repoPath $ \repo ->
    withRemote repo name
      (c'git_remote_url >=> peekCString)


withRepo
  :: FilePath
  -> (Ptr C'git_repository -> IO a)
  -> IO a
withRepo path f =
  withCString path $ \path' ->
    bracketResource
      "c'git_repository_open"
      (`c'git_repository_open` path')
      c'git_repository_free
      f


withRemote
  :: Ptr C'git_repository
  -> String
  -> (Ptr C'git_remote -> IO a)
  -> IO a
withRemote repo name f =
  withCString name $ \name' ->
    bracketResource
      "c'git_remote_load"
      (\out -> c'git_remote_load out repo name')
      c'git_remote_free
      f


bracketResource
  :: forall a b.
     String
  -> (Ptr (Ptr a) -> IO CInt)
  -> (Ptr a -> IO ())
  -> (Ptr a -> IO b)
  -> IO b
bracketResource initName initCmd free f =
    alloca $ \pptr ->
      do
        initialize pptr
        resource <- peek pptr
        run resource
  where
    initialize :: Ptr (Ptr a) -> IO ()
    initialize pptr =
      do
        exit <- initCmd pptr
        checkError exit initName

    run :: Ptr a -> IO b
    run resource =
      f resource `finally` free resource


checkError :: CInt -> String -> IO ()
checkError exit =
  when (exit /= 0) . throwGit


throwGit :: String -> IO a
throwGit cmd =
  throwIO . GitError cmd =<< gitErrMsg


gitErrMsg :: IO String
gitErrMsg =
  do
    err <- peek =<< c'giterr_last
    peekCString (c'git_error'message err)
