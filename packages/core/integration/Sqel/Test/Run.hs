module Sqel.Test.Run where

import Control.Exception.Lifted (bracket)
import qualified Data.Text.IO as Text
import Exon (exon)
import qualified Hasql.Connection as Connection
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Session as Session
import Hasql.Session (Session)
import Hedgehog (TestT, evalEither)
import Hedgehog.Internal.Property (failWith)
import System.Environment (lookupEnv)

import Sqel.Class.CompleteCodec (completeCodec)
import Sqel.Data.Sql (Sql)
import Sqel.Class.ResultShape (ResultShape)
import Sqel.Statement (unsafeRunSql)

data DbConfig =
  DbConfig {
    host :: Text,
    port :: Int,
    name :: Text,
    user :: Text,
    password :: Text
  }
  deriving stock (Eq, Show, Generic)

data EnvDb =
  EnvDb {
    envPrefix :: String,
    dbName :: Text,
    dbUser :: Text,
    dbPassword :: Text,
    fatal :: Bool,
    notify :: Bool
  }
  deriving stock (Eq, Show, Generic)

envDb :: String -> EnvDb
envDb name =
  EnvDb {
    envPrefix = name,
    dbName = fromString name,
    dbUser = fromString name,
    dbPassword = fromString name,
    fatal = False,
    notify = True
  }

instance IsString EnvDb where
  fromString = envDb

envDbConfig ::
  EnvDb ->
  TestT IO (Either Text DbConfig)
envDbConfig EnvDb {..} =
  cons =<< liftIO (lookupEnv hostVar)
  where
    cons = \case
      Just host -> do
        port <- parsePort =<< (fromMaybe "5432" <$> liftIO (lookupEnv portVar))
        pure (Right (DbConfig (fromString host) port dbName dbUser dbPassword))
      Nothing ->
        pure (Left (toText hostVar))
    parsePort p =
      case readMaybe p of
        Just a -> pure a
        Nothing -> failWith Nothing [exon|Invalid port in env var '$#{portVar}': #{p}|]
    hostVar = [exon|#{envPrefix}_test_host|]
    portVar = [exon|#{envPrefix}_test_port|]

connectionSettings ::
  DbConfig ->
  Connection.Settings
connectionSettings (DbConfig host port dbName user password) =
  Connection.settings (encodeUtf8 host) (fromIntegral port) (encodeUtf8 user) (encodeUtf8 password) (encodeUtf8 dbName)

runOrSkip :: (DbConfig -> TestT IO ()) -> TestT IO ()
runOrSkip run =
  envDbConfig "sqel" >>= \case
    Right conf -> run conf
    Left err -> liftIO (Text.putStrLn err)

stmt ::
  ∀ r a .
  ResultShape a r =>
  Decoders.Value a ->
  Sql ->
  Session r
stmt dec s =
  unsafeRunSql () s mempty (completeCodec dec)

stmt_ :: Sql -> Session ()
stmt_ s = unsafeRunSql () s mempty unit

integrationTest ::
  HasCallStack =>
  ((∀ x . Session x -> TestT IO x) -> TestT IO ()) ->
  TestT IO ()
integrationTest use =
  withFrozenCallStack $
  runOrSkip \ conf -> bracket (acquire conf) release \ conn -> do
    use \ session -> evalEither =<< liftIO (Session.run session conn)
  where
    acquire conf = evalEither =<< liftIO (Connection.acquire (connectionSettings conf))
    release = liftIO . Connection.release
