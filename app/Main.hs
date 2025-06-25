module Main where

import Control.Exception (SomeException, catch, throw, try)
import Control.Monad (unless, void)
import Control.Monad.State.Strict (MonadState, MonadTrans (lift), StateT, execStateT, modify)
import Data.Bool (bool)
import Data.ByteString qualified as ByteString
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.List (inits)
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import System.Directory
import System.Environment.Blank (getEnvDefault, getEnv)
import System.Environment (getExecutablePath, setEnv)
import System.FilePath (splitDirectories, takeDirectory, (</>))
import System.IO (hPrint, stderr)
import System.PosixCompat (FileMode, createSymbolicLink)
import System.PosixCompat.Files (setFileMode)
import Prelude
import System.Process (callProcess)

put :: (MonadState [w] m) => w -> m ()
put a = modify (a :)

newtype FileContent = FileContent ByteString
    deriving newtype (Eq)

instance Show FileContent where
    show (FileContent bs) = "<ByteString of length " <> show (ByteString.length bs) <> ">"

data Operation
    = CreateDirectory !FilePath
    | CreateFile !FilePath !FileMode !FileContent
    | CreateSymbolicLink !FilePath !FilePath
    | InstallNixRoot !FilePath
    deriving stock (Show, Eq)

perform :: Operation -> IO ()
perform (CreateDirectory d) = createDirectory d
perform (CreateFile f m c) = do
    ByteString.writeFile f $ coerce c
    setFileMode f m `catch` \(e :: SomeException) -> do
        Main.reverse (CreateFile f m c)
        throw e
perform (CreateSymbolicLink s d) = createSymbolicLink s d
perform (InstallNixRoot _) = callProcess "nah" ["nix-static", "profile", "-L", "install", "nixpkgs#nix"]

reverse :: Operation -> IO ()
reverse (CreateDirectory d) = removeDirectoryRecursive d
reverse (CreateFile f _ _) = removeFile f
reverse (CreateSymbolicLink _ f) = removeFile f
reverse (InstallNixRoot nixRoot) = pure ()

performAll :: [Operation] -> IO ()
performAll ops = go ops []
  where
    go :: [Operation] -> [Operation] -> IO ()
    go [] _ = pure ()
    go (x : xs) ys =
        try (perform x) >>= \case
            Right () -> go xs (x : ys)
            Left (e :: SomeException) -> do
                hPrint stderr e
                -- mapM_ Main.reverse ys

createDirectoryRecursive :: FilePath -> StateT [Operation] IO ()
createDirectoryRecursive f = for_ dirs \d -> do
    exists <- lift $ doesDirectoryExist d
    unless exists . put $ CreateDirectory d
  where
    dirs = fmap (foldr1 (</>)) . drop 1 . inits . splitDirectories $ f

createFile :: FilePath -> FileMode -> ByteString -> StateT [Operation] IO ()
createFile f m c = put . CreateFile f m $ FileContent c

readFile :: [(Text, Text)] -> FilePath -> IO ByteString
readFile [] f = ByteString.readFile f
readFile m f = ByteString.readFile f >>= \bs -> case Text.decodeUtf8' bs of
  Left e -> throw e
  Right t0 -> pure . Text.encodeUtf8 $ List.foldl' (\t (a, b) -> Text.replace a b t) t0 m

unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM f a = bool a (pure ()) =<< f

main :: IO ()
main = do
    progDir <- takeDirectory <$> getExecutablePath
    homeDir <- getHomeDirectory
    binDir <- getEnvDefault "NAH_BIN_DIR" $ homeDir </> ".local" </> "bin"
    nixRoot <- getEnvDefault "NAH_NIX_ROOT" $ homeDir </> "nixroot"

    setEnv "PATH" . maybe binDir ((binDir <> ":") <>) =<< getEnv "PATH"

    let createFileFromStatic :: [(Text, Text)] -> FilePath -> FileMode -> FilePath -> StateT [Operation] IO ()
        createFileFromStatic subs f m s = unlessM (lift $ doesFileExist f) $ createFile f m =<< lift (readFile subs $ progDir </> s)
        createSpecialisation :: FilePath -> StateT [Operation] IO ()
        createSpecialisation ((binDir </>) -> f) = unlessM (lift $ doesFileExist f) . put $ CreateSymbolicLink "nix" f
        mode755 :: FileMode
        mode755 = 0b111101101
        mode644 :: FileMode
        mode644 = 0b110100100

    let nixStaticFile = binDir </> "nix-static"
    let nahFile = binDir </> "nah"
    let nixFile = binDir </> "nix"
    nixConfigDir <- getXdgDirectory XdgConfig "nix"
    let nixConfigFile = nixConfigDir </> "nix.conf"
    ops <-
        List.reverse <$> flip execStateT mempty do
            createDirectoryRecursive binDir
            createFileFromStatic [] nixStaticFile mode755 "nix-static"
            createFileFromStatic [("__NAH_NIX_ROOT_DEFAULT__", Text.pack nixRoot)] nahFile mode755 "nah.sh"
            createFileFromStatic [] nixFile mode755 "nix.sh"
            mapM_ @[]
                createSpecialisation
                [ "nix-shell"
                , "nix-build"
                , "nix-channel"
                , "nix-collect-garbage"
                , "nix-copy-closure"
                , "nix-daemon"
                , "nix-env"
                , "nix-hash"
                , "nix-instantiate"
                , "nix-prefetch-url"
                , "nix-store"
                ]
            createDirectoryRecursive nixConfigDir
            unlessM (lift $ doesFileExist nixConfigFile) $
                createFileFromStatic [] nixConfigFile mode644 "../share/nix.conf"

            createDirectoryRecursive $ nixRoot </> "var" </> "nix"
            put $ InstallNixRoot nixRoot
    mapM_ print ops
    performAll ops
