module Main (main) where

import           Data.Maybe (fromJust, isNothing)
import           Data.Semigroup ((<>))
import           Options.Applicative
import           Prelude (
    Bool(..),
    IO,
    String,
    (=<<),
    any,
    error,
    fmap,
    zipWith
  )
import           System.Directory
import           System.FilePath

import           Config.JSON
import           Config.JSON.Types
import           Config.JSON.Options.Parse

main :: IO ()
main = joinFiles =<< execParser opts where
  opts = info (optionParser <**> helper) (
         fullDesc
      <> progDesc longDescription
      <> header "JSON File Joiner - Join Specific Configuration to Common"
    )

longDescription :: String
longDescription = "Factor out common configuration from JSON configuration files "
  <> "and join them as needed to avoid duplication of effort and error-prone copy paste."

joinFiles :: Options -> IO ()
joinFiles (Options commonFilePath srcDir targetDir) = do
  srcConfigPaths <- getDirectoryContents srcDir
  createDirectoryIfMissing True targetDir
  let targetConfigPaths = fmap (replaceDirectory targetDir) srcConfigPaths
      envFiles = zipWith envConfigFile srcConfigPaths targetConfigPaths
  if any isNothing envFiles
    then error "Source file paths may not be the same as the target file path"
    else joinMain (CommonConfigFile commonFilePath) (fmap fromJust envFiles)
