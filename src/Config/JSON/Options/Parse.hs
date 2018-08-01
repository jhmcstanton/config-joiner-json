module Config.JSON.Options.Parse (Options(..), optionParser) where

import           Data.Semigroup ((<>))
import           Options.Applicative
import           Prelude (FilePath, String, (<$>))

data Options = Options {
    commonFile :: FilePath,
    sourceDirectory :: FilePath,
    targetDirectory :: FilePath
  }

optionParser :: Parser Options
optionParser = Options
  <$> strOption (
             long "common-file"
          <> short 'c'
          <> help commonHelp
        )
  <*> strOption (
             long "source-directory"
          <> short 's'
          <> help sourceHelp
        )
  <*> strOption (
             long "target-directory"
          <> short 't'
          <> help targetHelp
        )

commonHelp :: String
commonHelp = "The path to the common configuration file "
  <> "containing configuration details common across all environments"

sourceHelp :: String
sourceHelp = "The path to the directory containing all the environment files.\n"
  <> "May not match the target directory."

targetHelp :: String
targetHelp = "The path to the output directory. Any files matching the source file "
  <> "names will be overridden.\n"
  <> "May not match the source directory."
