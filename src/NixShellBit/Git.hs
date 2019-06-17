{-# LANGUAGE ScopedTypeVariables #-}

module NixShellBit.Git
  ( gitDiscoverRepo
  , gitRemoteGetUrl
  , gitRemoteList
  ) where

import Bindings.Libgit2   (C'git_strarray(C'git_strarray),
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
import Control.Exception  (finally)
import Control.Monad      (when, (<=<), (>=>))
import Data.List          (intercalate)
import Foreign            (Ptr, alloca, peek, sizeOf, allocaBytes,
                           fromBool, plusPtr)
import Foreign.C.String   (CString, peekCString, withCString)
import Foreign.C.Types    (CChar, CInt, CSize)
import NixShellBit.PPrint (Doc, red, die, text)


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
    ceiling' = intercalate ":" ceilingDirs

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
    die . doc =<< gitErrMsg
  where
    doc :: String -> Doc
    doc msg = red $ text $
      "nix-shell-bit: " ++ cmd ++ ": " ++ msg


gitErrMsg :: IO String
gitErrMsg =
  do
    err <- peek =<< c'giterr_last
    peekCString (c'git_error'message err)
