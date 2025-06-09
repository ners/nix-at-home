{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.ByteString qualified as ByteString
import Data.FileEmbed (embedFile, makeRelativeToProject)
import System.Directory
    ( Permissions
    , XdgDirectory (XdgData)
    , createDirectoryIfMissing
    , doesFileExist
    , getPermissions
    , getXdgDirectory
    , setOwnerExecutable
    , setPermissions
    )
import System.FilePath ((</>))
import Prelude

nixBin :: ByteString
nixBin = $(embedFile =<< makeRelativeToProject "nix")

updatePermissions :: (Permissions -> Permissions) -> FilePath -> IO ()
updatePermissions u f = setPermissions f . u =<< getPermissions f

main :: IO ()
main = do
    nixDir <- getXdgDirectory XdgData "nix"
    let nixFile = nixDir </> "nix"
    doesFileExist nixFile >>= \case
        True -> fail "Nix already exists"
        False -> do
            createDirectoryIfMissing True nixDir
            ByteString.writeFile nixFile nixBin
            updatePermissions (setOwnerExecutable True) nixFile
