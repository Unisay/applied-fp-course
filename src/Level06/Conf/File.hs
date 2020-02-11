{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Level06.Conf.File where

import Control.Exception (Exception, SomeException, displayException, try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Attoparsec.ByteString as AB
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Monoid (Last (Last))
import Data.Text (Text, pack)
import Level06.AppM (AppM (runAppM), liftEither)
import Level06.Types (ConfigError (..), PartialConf (PartialConf), partialConfDecoder)
import Waargonaut (Json)
import qualified Waargonaut.Decode as D
import Waargonaut.Decode.Error (DecodeError (ParseFailed))

-- $setup
-- >>> :set -XOverloadedStrings

-- | The configuration file is in the JSON format, so we need to write a
-- 'waargonaut' 'Decoder' to go from JSON to our 'PartialConf'.
--
-- Update these tests when you've completed this function.
--
-- >>> runAppM $ readConfFile "badFileName.no"
-- Left (<YourErrorConstructorHere> "badFileName.no: openBinaryFile: does not exist (No such file or directory)")
-- >>> runAppM $ readConfFile "files/test.json"
-- Right "{\n  \"foo\": 33\n}\n"
readConfFile :: FilePath -> AppM ConfigError ByteString
readConfFile path = liftIO (first ConfigIOError <$> try (BS.readFile path)) >>= liftEither

-- | Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile :: FilePath -> AppM ConfigError PartialConf
parseJSONConfigFile fp =
  readConfFile fp
    >>= liftEither
    . first (BadConfFile . fst)
    . D.pureDecodeFromByteString AB.parseOnly partialConfDecoder
-- Go to 'src/Level06/Conf.hs' n ext.
