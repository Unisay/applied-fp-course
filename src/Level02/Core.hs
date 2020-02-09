{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Level02.Core
  ( runApp,
    app,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Either (either)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Level02.Types
  ( ContentType (..),
    Error (..),
    RqType (..),
    mkCommentText,
    mkTopic,
    renderContentType,
  )
import Network.HTTP.Types
  ( Status (statusCode),
    StdMethod (..),
    hContentType,
    parseMethod,
    status200,
    status400,
    status404,
  )
import Network.Wai
  ( Application,
    Request,
    Response,
    pathInfo,
    requestMethod,
    responseLBS,
    strictRequestBody,
  )
import Network.Wai.Handler.Warp (run)

-- | -------------------------------------------|
--  |- Don't start here, go to Level02.Types!  -|
--  |-------------------------------------------|

-- | Some helper functions to make our lives a little more DRY.
mkResponse :: Status -> ContentType -> LBS.ByteString -> Response
mkResponse status ct = responseLBS status [(hContentType, renderContentType ct)]

resp200 :: ContentType -> LBS.ByteString -> Response
resp200 = mkResponse status200

resp404 :: ContentType -> LBS.ByteString -> Response
resp404 = mkResponse status404

resp400 :: ContentType -> LBS.ByteString -> Response
resp400 = mkResponse status400

-- | ----------------------------------------------------------------------------------
--  These next few functions will take raw request information and construct         --
--  one of our types.                                                                --
--                                                                                   --
--  By breaking out these smaller functions, we're able to isolate our               --
--  validation requirements into smaller components that are simpler to maintain     --
--  and verify. It also allows for greater reuse and it also means that              --
--  validation is not duplicated across the application, maybe incorrectly.          --
--------------------------------------------------------------------------------------
mkAddRequest :: Text -> LBS.ByteString -> Either Error RqType
mkAddRequest topicText commentText = do
  topic <- mkTopic topicText
  comment <- mkCommentText . decodeUtf8 . LBS.toStrict $ commentText
  return $ AddRq topic comment

mkViewRequest :: Text -> Either Error RqType
mkViewRequest = fmap ViewRq . mkTopic

mkListRequest :: Either Error RqType
mkListRequest = pure ListRq

-- | ----------------------------------
--  end of RqType creation functions --
--------------------------------------
mkErrorResponse :: Error -> Response
mkErrorResponse = \case
  EmptyTopic ->
    resp400 PlainTextContent "Topic is empty"
  EmptyComment ->
    resp400 PlainTextContent "Comment is empty"
  ResourceNotFound method path ->
    resp404 PlainTextContent $
      LBS.concat
        [ "Method '",
          toLazyBs $ Text.pack $ show method,
          "' isn't defined for path '",
          toLazyBs $ Text.intercalate "/" path,
          "'"
        ]
  UnsupportedMethod _ ->
    resp400 PlainTextContent "Unsupported method"
  where
    toLazyBs = LBS.fromStrict . encodeUtf8

-- | Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest :: Request -> IO (Either Error RqType)
mkRequest request =
  case parseMethod (requestMethod request) of
    Left bsMethod -> pure . Left . UnsupportedMethod $ bsMethod
    Right stdMethod -> case (stdMethod, pathInfo request) of
      (GET, ["list"]) -> pure mkListRequest
      (GET, [topic, "view"]) -> pure . mkViewRequest $ topic
      (POST, [topic, "add"]) -> mkAddRequest topic <$> strictRequestBody request
      (method, path) -> pure . Left $ ResourceNotFound method path

-- | If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest :: RqType -> Either Error Response
handleRequest = \case
  AddRq _ _ -> Right $ resp200 PlainTextContent "Not implemented yet"
  ViewRq _ -> Right $ resp200 PlainTextContent "Not implemented yet"
  ListRq -> Right $ resp200 PlainTextContent "Not implemented yet"

-- | Reimplement this function using the new functions and ``RqType`` constructors as a guide.
app :: Application
app request cb = do
  errorOrRq <- mkRequest request
  cb $ case errorOrRq >>= handleRequest of
    Left err -> mkErrorResponse err
    Right rq -> rq

runApp :: IO ()
runApp = do
  putStrLn "Listening on localhost:3000"
  run 3000 app
