{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

-- REMINDER
-- This level is not an isolated module to complete. This level exists as one
-- starting module: `test/Test.hs`. Which you are to import your most recently
-- completed `Application` to be tested.
--
-- As you progress through the course, you are encouraged to return to this
-- `test/Test.hs` and update it so you're able to be confident that your
-- application will behave as you expect. You may also write your tests before
-- you write your functions, this can be useful when trying to think through a
-- problem.

-- This is the only location for tests as you progress through the course.

-- This module starts our very sparse. There are only the imports required to
-- have these initial tests work. Additional tests are for you to build. and may
-- require you to import other modules as you require.
--
-- As you progress through the levels, the initialisation of the 'app' will
-- become more complicated as more components are introduced. Part of the
-- exercise is to work out how to integrate your application with this testing
-- framework.

-- 'tasty' takes care of managing all of our test cases, running them,
-- checking results and then providing us with a report.

-- 'tasty-wai' makes it easier to create requests to submit to our
-- application, and provides some helper functions for checking our assertions.

-- For running unit tests for individual functions, we have included the
-- 'tasty-hunit' package. More information is available on the Hackage page:
-- https://hackage.haskell.org/package/tasty-hunit.
--
-- import qualified Test.Tasty.HUnit as HU
--

-- This import is provided for you so you can check your work from Level02. As
-- you move forward, come back and import your latest 'Application' so that you
-- can test your work as you progress.

import Control.Monad.IO.Class (liftIO)
import qualified Database.SQLite.Simple as Sql
import Level07.AppM (Env (..))
import qualified Level07.Core as Core
import Level07.Types (Conf (..), FirstAppDB (..), Port (Port))
import Network.HTTP.Types as HTTP
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Wai
  ( assertBody,
    assertStatus',
    get,
    post,
    testWai,
  )

main :: IO ()
main = do
  app <- initApp
  defaultMain $
    testGroup
      "Applied FP Course - Tests"
      [ testWai app "List Topics" $
          get "fudge/view" >>= assertStatus' HTTP.status200,
        testWai app "Empty Input" $ do
          resp <- post "fudge/add" ""
          assertStatus' HTTP.status400 resp
          assertBody "Empty Comment" resp,
        testWai app "Empty Input" $ do
          resp <- post "//add" ""
          assertStatus' HTTP.status400 resp
          assertBody "Empty Topic" resp
      ]
  where
    envLoggingFn = liftIO . print
    envConfig = Conf {port = Port 3000, dbFilePath = "DBFilePath"}
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (\
      \id INTEGER PRIMARY KEY, \
      \topic TEXT, \
      \comment TEXT, \
      \time INTEGER\
      \)"
    initApp = do
      con <- Sql.open ":memory:"
      _ <- Sql.execute_ con createTableQ
      return $ Core.app Env {envDB = FirstAppDB con, ..}
