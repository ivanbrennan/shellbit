{-# LANGUAGE ScopedTypeVariables #-}

module NixShellBit.Git
  ( gitDiscoverRepo
  , gitListVersions
  , gitRemoteGetUrl
  , gitRemoteList
  ) where

import Bindings.Libgit2     (C'git_strarray(C'git_strarray),
                             C'git_remote,
                             C'git_repository,
                             c'GIT_OK,
                             c'GIT_ENOTFOUND,
                             c'giterr_last,
                             c'git_error'message,
                             c'git_repository_discover,
                             c'git_repository_open, c'git_repository_free,
                             c'git_remote_list,
                             c'git_remote_url,
                             c'git_remote_load, c'git_remote_free,
                             withLibGitDo)
import Control.Exception    (finally)
import Control.Monad        (when, (<=<), (>=>))
import Data.List            (intercalate)
import Data.Maybe           (mapMaybe)
import Foreign              (Ptr, alloca, peek, sizeOf, allocaBytes,
                             fromBool, plusPtr)
import Foreign.C.String     (CString, peekCString, withCString)
import Foreign.C.Types      (CChar, CInt, CSize)
import NixShellBit.PPrint   (fatalError)
import System.FilePath      (searchPathSeparator)
import System.Process.Typed (proc, readProcessStdout_)

import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as L8


gitDiscoverRepo :: FilePath -> [String] -> IO (Maybe FilePath)
gitDiscoverRepo startPath ceilingDirs =
    withLibGitDo $
    withCString startPath  $ \start_path   ->
      withCString ceiling' $ \ceiling_dirs ->
        allocaBytes bytes  $ \path_out     ->
          do
            exit <- c'git_repository_discover path_out
                                              path_size
                                              start_path
                                              across_fs
                                              ceiling_dirs
            case exit of
              x | x == c'GIT_OK        -> Just <$> peekCString path_out
                | x == c'GIT_ENOTFOUND -> pure Nothing
                | otherwise            -> fatal "c'git_repository_discover"
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
      alloca          $ \out  ->
        do
          exit <- c'git_remote_list out repo
          checkError exit "c'git_remote_list"
          C'git_strarray cstrings count <- peek out

          mapM
            (peekCString <=< (peek . plusPtr cstrings) . (step *))
            [0 .. fromIntegral (count - 1)]
  where
    step :: Int
    step = sizeOf (undefined :: Ptr CString)


{-| libgit2 v0.27 has a git_remote_create_detached()
    function that could emulate this `git ls-remote`
    call. If/when hlibgit2 gets updated, use that.

    https://github.com/libgit2/libgit2/pull/4233
-}
gitListVersions
  :: String
  -> String
  -> IO [String]
gitListVersions url project =
  do
    out <- readProcessStdout_ (proc "git" ls_remote)
    pure $ mapMaybe v (L8.lines out)
  where
    ls_remote :: [String]
    ls_remote =
      [ "ls-remote"
      , "--tags"
      , "--sort=version:refname"
      , url
      , project ++ "-*"
      ]

    v :: L8.ByteString -> Maybe String
    v = fmap C8.unpack
      . C8.stripPrefix prefix
      . snd
      . C8.breakSubstring prefix
      . L8.toStrict

    prefix :: C8.ByteString
    prefix =
      C8.pack ("refs/tags/" ++ project ++ "-")


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
  when (exit /= 0) . fatal


fatal :: String -> IO a
fatal cmd =
  fatalError cmd =<< gitErrMsg


gitErrMsg :: IO String
gitErrMsg =
  do
    err <- peek =<< c'giterr_last
    peekCString (c'git_error'message err)
