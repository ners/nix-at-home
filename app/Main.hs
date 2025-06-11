{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception (SomeException, catch, throw)
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
import System.FilePath (splitDirectories, (</>), takeDirectory)
import System.PosixCompat (FileMode)
import System.PosixCompat.Files (setFileMode)
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
    deriving stock (Show, Eq)

perform :: Operation -> IO ()
perform (CreateDirectory d) = createDirectory d
perform (CreateFile f m c) = do
    ByteString.writeFile f $ coerce c
    setFileMode f m `catch` \(e :: SomeException) -> do
        Main.reverse (CreateFile f m c)
        throw e

reverse :: Operation -> IO ()
reverse (CreateDirectory d) = removeDirectory d
reverse (CreateFile f _ _) = removeFile f

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

    let createFileFromStatic :: FilePath -> FileMode -> FilePath -> StateT [Operation] IO ()
        createFileFromStatic f m s = createFile f m =<< lift (ByteString.readFile $ progDir <> "/../share/static/" <> s)

    homeDir <- getHomeDirectory
    binDir <- getEnvDefault "NAH_BIN_DIR" $ homeDir </> ".local" </> "bin"
    let nixFile = binDir </> "nix-static"
    let nahFile = binDir </> "nah"
    nixConfigDir <- getXdgDirectory XdgConfig "nix"
    let nixConfigFile = nixConfigDir </> "nix.conf"
    mapM_ print . List.reverse =<< flip execStateT mempty do
        createDirectoryRecursive binDir
        createFileFromStatic nixFile 755 "nix"
        createFileFromStatic nahFile 755 "script.sh"
        createDirectoryRecursive nixConfigDir
        unlessM (lift $ doesFileExist nixConfigFile) $
            createFileFromStatic nixConfigFile 644 "nix.conf"
