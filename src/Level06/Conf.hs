{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Level06.Conf
  ( parseOptions,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Data.Semigroup (Last (getLast))
import GHC.Word (Word16)
import Level06.AppM (AppM, liftEither)
import Level06.Conf.CommandLine (commandLineParser)
import Level06.Conf.File (parseJSONConfigFile)
import Level06.Types
  ( Conf (..),
    ConfigError (MissingDBFilePath, MissingPort),
    DBFilePath (DBFilePath),
    PartialConf (..),
    Port (Port),
  )

-- | For the purposes of this application we will encode some default values to
-- ensure that our application continues to function in the event of missing
-- configuration values from either the file or command line inputs.
defaultConf :: PartialConf
defaultConf =
  PartialConf
    { pcPort = pure . pure $ Port 3000,
      pcDBFilePath = pure . pure $ DBFilePath "app_db.db"
    }

-- | We need something that will take our PartialConf and see if can finally build
-- a complete ``Conf`` record. Also we need to highlight any missing values by
-- providing the relevant error.
makeConfig :: PartialConf -> Either ConfigError Conf
makeConfig PartialConf {pcPort, pcDBFilePath} = do
  let withError e = maybe (Left e) (pure . getLast)
  confPort <- withError MissingPort pcPort
  DBFilePath confDbFile <- withError MissingDBFilePath pcDBFilePath
  return Conf {confPort, confDbFile}

-- | This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.
--
-- Remember that we want the command line configuration to take precedence over
-- the File configuration, so if we think about combining each of our ``Conf``
-- records. By now we should be able to write something like this:
parseOptions :: FilePath -> AppM ConfigError Conf
parseOptions configPath = do
  configFromFile <- parseJSONConfigFile configPath
  configFromCli <- liftIO commandLineParser
  liftEither . makeConfig $ defaultConf <> configFromFile <> configFromCli
