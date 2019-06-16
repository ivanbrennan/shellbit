{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Bindings.Libgit2  (C'git_config, C'git_remote, C'git_repository,
                          c'git_config_open_ondisk, c'git_config_free,
                          c'git_config_get_string, c'git_remote_load,
                          c'git_remote_url, c'git_remote_free,
                          c'git_repository_open, c'git_repository_free,
                          c'git_repository_open_ext,
                          c'git_repository_discover, withLibGitDo)
import Control.Exception (finally)
import Control.Monad     (when, (>=>))
import Data.List         (intercalate)
import Foreign           (Ptr, alloca, peek, sizeOf, allocaBytes, fromBool)
import Foreign.C.String  (CString, peekCString, withCString)
import Foreign.C.Types   (CChar, CInt, CUInt, CSize)
import System.Directory  (getCurrentDirectory, getHomeDirectory)
import System.FilePath   (takeBaseName, (</>))


newtype Project = Project String


main :: IO ()
main = do
    startPath   <- getCurrentDirectory
    ceilingDirs <- pure <$> getHomeDirectory
    repoPath    <- discoverRepo startPath ceilingDirs

    withLibGitDo $ do
      url <- withFoundRepo startPath ceilingDirs $ \repo ->
               withRemote repo "origin"
                 (c'git_remote_url >=> peekCString)

      args <- nixShellArgs (project url)
      print args

    withLibGitDo $ do
      url <- gitRemoteGetUrl repoPath "origin"
      args <- nixShellArgs (project url)
      print args

    withLibGitDo $ do
      url <- gitConfigGetString repoPath "remote.origin.url"
      args <- nixShellArgs (project url)
      print args
  where
    project :: FilePath -> Project
    project = Project . takeBaseName

    derivation :: String
    derivation = "https://github.com/Foo/nix-shells/archive/pkgset.tar.gz"

    nixShellOptions :: [String]
    nixShellOptions = []

    nixShellArgs :: Project -> IO [String]
    nixShellArgs (Project pjt) =
      pure $ [derivation, "--attr", pjt] ++ nixShellOptions


discoverRepo :: FilePath -> [String] -> IO FilePath
discoverRepo startPath ceilingDirs =
    withCString startPath  $ \start_path ->
      withCString ceiling' $ \ceiling_dirs ->
        allocaBytes bytes  $ \path_out ->
          discover start_path ceiling_dirs path_out
  where
    ceiling' :: String
    ceiling' = intercalate ":" ceilingDirs

    bytes :: Int
    bytes = 256 * sizeOf (undefined :: CChar)

    path_size :: CSize
    path_size = fromIntegral bytes

    across_fs :: CInt
    across_fs = fromBool False

    discover
      :: CString
      -> CString
      -> Ptr CChar
      -> IO FilePath
    discover start_path ceiling' path_out = do
      exit <- c'git_repository_discover
                path_out path_size start_path across_fs ceiling'
      -- TODO: Handle this more gracefully, since it
      -- will occur whenever run outside a git repo.
      checkError exit "c'git_repository_discover"
      peekCString path_out


gitRemoteGetUrl
  :: FilePath
  -> String
  -> IO String
gitRemoteGetUrl repoPath name =
  withRepo repoPath $ \repo ->
    withRemote repo name
      (c'git_remote_url >=> peekCString)


gitConfigGetString
  :: FilePath
  -> String
  -> IO String
gitConfigGetString repoPath key =
  withConfig (repoPath </> "config") $ \cfg ->
    withCString key                  $ \key' ->
      bracketResource
        "c'git_config_get_string"
        (\out -> c'git_config_get_string out cfg key')
        (const $ pure ())
        peekCString


withFoundRepo
  :: FilePath
  -> [String]
  -> (Ptr C'git_repository -> IO a)
  -> IO a
withFoundRepo startPath ceilingDirs =
    bracketResource
      "c'git_repository_open_ext"
      (\out ->
        withCString startPath  $ \start_path ->
          withCString ceiling' $ \ceiling_dirs ->
            c'git_repository_open_ext out start_path flags ceiling_dirs
      )
      c'git_repository_free
  where
    ceiling' :: String
    ceiling' = intercalate ":" ceilingDirs

    flags :: CUInt
    flags = 0


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


withConfig
  :: FilePath
  -> (Ptr C'git_config -> IO a)
  -> IO a
withConfig path f =
  withCString path $ \path' ->
    bracketResource
      "c'git_config_open_ondisk"
      (`c'git_config_open_ondisk` path')
      c'git_config_free
      f


bracketResource
  :: forall a b.
     String
  -> (Ptr (Ptr a) -> IO CInt)
  -> (Ptr a -> IO ())
  -> (Ptr a -> IO b)
  -> IO b
bracketResource initName initCmd free f =
    alloca $ \pptr -> do
      initialize pptr
      resource <- peek pptr
      run resource
  where
    initialize :: Ptr (Ptr a) -> IO ()
    initialize pptr = do
      exit <- initCmd pptr
      checkError exit initName

    run :: Ptr a -> IO b
    run resource =
      f resource `finally` free resource


checkError
  :: CInt
  -> String
  -> IO ()
checkError exit cmdName =
  when (exit /= 0) $ error (cmdName ++ " failed")


-- c'git_config_get_string   :: Ptr CString -> Ptr C'git_config -> CString -> IO CInt
-- c'git_repository_discover :: CString -> CSize -> CString -> CInt -> CString -> IO CInt
-- c'git_repository_open     :: Ptr (Ptr C'git_repository) -> CString -> IO CInt
-- c'git_repository_open_ext :: Ptr (Ptr C'git_repository) -> CString -> CUInt -> CString -> IO CInt
-- type C'git_repository_open_flag_t = CUInt
-- flags = 0 :: C'git_repository_open_flag_t
-- c'git_remote_load :: Ptr (Ptr C'git_remote) -> Ptr C'git_repository -> CString -> IO CInt
-- c'git_remote_url  :: Ptr C'git_remote -> IO CString

-- int git_config_get_string(const char **out, const git_config *cfg, const char *name);
-- int git_repository_discover(char *path_out, size_t path_size, const char *start_path, int across_fs, const char *ceiling_dirs);
-- int git_repository_open(git_repository **out, const char *path);
-- int git_repository_open_ext(git_repository **out, const char *path, unsigned int flags, const char *ceiling_dirs);
-- int git_remote_load(git_remote **out, git_repository *repo, const char *name);
-- const char * git_remote_url(const git_remote *remote);
