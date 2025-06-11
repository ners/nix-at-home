module Main where

import Control.Exception (SomeException, catch, throw, try)
import Control.Monad (unless)
import Control.Monad.State.Strict (MonadState, MonadTrans (lift), StateT, execStateT, modify)
import Data.Bool (bool)
import Data.ByteString qualified as ByteString
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.List (inits)
import Data.List qualified as List
import System.Directory
import System.Environment.Blank (getEnvDefault, getExecutablePath)
import System.FilePath (splitDirectories, takeDirectory, (</>))
import System.IO (hPrint, stderr)
import System.PosixCompat (FileMode, createSymbolicLink)
import System.PosixCompat.Files (setFileMode)
import System.Process (callProcess)
import Prelude

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
perform (InstallNixRoot nixStatic) = callProcess nixStatic ["profile", "install", "nixpkgs#nix"]

reverse :: Operation -> IO ()
reverse (CreateDirectory d) = removeDirectoryRecursive d
reverse (CreateFile f _ _) = removeFile f
reverse (CreateSymbolicLink _ f) = removeFile f
reverse (InstallNixRoot _) = pure ()

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
                mapM_ Main.reverse ys

createDirectoryRecursive :: FilePath -> StateT [Operation] IO ()
createDirectoryRecursive f = for_ dirs \d -> do
    exists <- lift $ doesDirectoryExist d
    unless exists . put $ CreateDirectory d
  where
    dirs = fmap (foldr1 (</>)) . drop 1 . inits . splitDirectories $ f

createFile :: FilePath -> FileMode -> ByteString -> StateT [Operation] IO ()
createFile f m c = put . CreateFile f m $ FileContent c

unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM f a = bool a (pure ()) =<< f

main :: IO ()
main = do
    progDir <- takeDirectory <$> getExecutablePath
    homeDir <- getHomeDirectory
    binDir <- getEnvDefault "NAH_BIN_DIR" $ homeDir </> ".local" </> "bin"
    nixRoot <- getEnvDefault "NAH_NIX_ROOT" $ homeDir </> "nixroot"

    let createFileFromStatic :: FilePath -> FileMode -> FilePath -> StateT [Operation] IO ()
        createFileFromStatic f m s = createFile f m =<< lift (ByteString.readFile $ progDir </> s)
        createSpecialisation :: FilePath -> StateT [Operation] IO ()
        createSpecialisation f = unlessM (lift $ doesFileExist f) . put . CreateSymbolicLink "nix" $ binDir </> f
        mode755 :: FileMode
        mode755 = 0b111101101

    let nixStaticFile = binDir </> "nix-static"
    let nahFile = binDir </> "nah"
    let nixFile = binDir </> "nix"
    nixConfigDir <- getXdgDirectory XdgConfig "nix"
    let nixConfigFile = nixConfigDir </> "nix.conf"
    ops <-
        List.reverse <$> flip execStateT mempty do
            createDirectoryRecursive binDir
            createFileFromStatic nixStaticFile mode755 "nix-static"
            createFileFromStatic nahFile mode755 "nah.sh"
            createFileFromStatic nixFile mode755 "nix.sh"
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
                createFileFromStatic nixConfigFile 644 "../share/nix.conf"

            let nixRootVar = nixRoot </> "var" </> "nix"
            createDirectoryRecursive nixRootVar
            put . InstallNixRoot $ binDir </> "nix-static"
    mapM_ print ops
    performAll ops
