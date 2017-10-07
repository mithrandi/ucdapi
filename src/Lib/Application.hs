module Lib.Application (appMain) where

import           Control.Monad.Logger (MonadLogger, runStdoutLoggingT, runNoLoggingT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Pool (Pool)
import           Database.Persist.Sqlite (SqlBackend, createSqlitePool, runSqlPool, runMigration)
import           Network.Wai.Handler.Warp (run)

import qualified Prometheus
import           Prometheus.Metric.GHC (ghcMetrics)
import           Servant
import           Web.HttpApiData (parseUrlPiece)

import           Lib.API
import           Lib.Character
import           Lib.Instrument (requestDuration, instrumentApp)
import           Lib.Load
import           Lib.Middleware
import           Lib.Prelude
import           Lib.UCD

createPool :: (MonadBaseControl IO m, MonadLogger m, MonadIO m) => Int -> m (Pool SqlBackend)
createPool = createSqlitePool "db/UCD.sqlite3"

appMain :: IO ()
appMain = do
  args <- getArgs
  case args of
    ["server"] -> serverMain
    ("load":xs) -> loadMain (toS <$> xs)
    _ -> die "Pass one of: server, load"

app :: Pool SqlBackend -> Application
app pool = serve (Proxy :: Proxy API) (enter convert server)
  where convert :: AppM :~> Handler
        convert = NT (`runSqlPool` pool)

serverMain :: IO ()
serverMain = do
  void $ Prometheus.register ghcMetrics
  requests <- Prometheus.registerIO requestDuration
  nc <- getNumCapabilities
  pool <- runNoLoggingT (createPool nc)
  let middleware =
        instrumentApp requests "ucdapi"
        . healthz
        . forceHttpsProxy
  run 3000 (middleware (app pool))

loadMain :: [Text] -> IO ()
loadMain args = do
  -- Everything is single-threaded at the moment
  setNumCapabilities 1
  runSqlPool (runMigration migrate) =<< runStdoutLoggingT (createPool 1)
  pool <- runNoLoggingT (createPool 1)
  case args of
    [parseUrlPiece -> Right ver, inputFileName] -> runSqlPool (loadUCD ver inputFileName) pool
    _ -> die "Usage: load <UCD version> <UCD flat XML file>"
