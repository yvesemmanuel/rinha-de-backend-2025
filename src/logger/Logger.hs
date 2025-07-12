{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Logger.Logger
  ( -- * Types
    LogLevel(..)
  , LogEntry(..)
  , LogContext(..)
  , LoggerConfig(..)
  , LogFormat(..)
  , ConsoleLogger(..)
    -- * Logger class
  , Logger(..)
    -- * Context builders
  , emptyContext
  , withCorrelationId
  , withComponent
  , withOperation
  , addCorrelationId
  , addMetadata
    -- * Configuration
  , defaultLoggerConfig
  , errorLoggerConfig
  , debugLoggerConfig
    -- * Running logger
  , runConsoleLogger
  ) where

import Data.Time
  ( getCurrentTime, formatTime, defaultTimeLocale, UTCTime )
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (Handle, stdout, stderr)
import Text.Printf (printf)

data LogLevel = DEBUG | INFO | WARN | ERROR | FATAL
  deriving (Show, Eq, Ord)

data LogEntry = LogEntry
  { logLevel :: LogLevel
  , logTimestamp :: UTCTime
  , logMessage :: Text
  , logContext :: LogContext
  } deriving (Show)

data LogContext = LogContext
  { correlationId :: Maybe UUID
  , requestId :: Maybe Text
  , component :: Text
  , operation :: Maybe Text
  , metadata :: [(Text, Text)]
  } deriving (Show)

data LoggerConfig = LoggerConfig
  { logHandle :: Handle
  , logMinLevel :: LogLevel
  , logFormat :: LogFormat
  , logTimestampFormat :: String
  } deriving (Show)

data LogFormat = JSON | PLAIN
  deriving (Show, Eq)

class Monad m => Logger m where
  logDebug :: Text -> m ()
  logInfo :: Text -> m ()
  logWarn :: Text -> m ()
  logError :: Text -> m ()
  logFatal :: Text -> m ()
  
  logDebugWith :: LogContext -> Text -> m ()
  logInfoWith :: LogContext -> Text -> m ()
  logWarnWith :: LogContext -> Text -> m ()
  logErrorWith :: LogContext -> Text -> m ()
  logFatalWith :: LogContext -> Text -> m ()

newtype ConsoleLogger a = ConsoleLogger (IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance Logger ConsoleLogger where
  logDebug = logWithLevel DEBUG emptyContext
  logInfo = logWithLevel INFO emptyContext
  logWarn = logWithLevel WARN emptyContext
  logError = logWithLevel ERROR emptyContext
  logFatal = logWithLevel FATAL emptyContext
  
  logDebugWith = logWithLevel DEBUG
  logInfoWith = logWithLevel INFO
  logWarnWith = logWithLevel WARN
  logErrorWith = logWithLevel ERROR
  logFatalWith = logWithLevel FATAL

emptyContext :: LogContext
emptyContext = LogContext
  { correlationId = Nothing
  , requestId = Nothing
  , component = "unknown"
  , operation = Nothing
  , metadata = []
  }

withCorrelationId :: UUID -> LogContext
withCorrelationId uuid = emptyContext { correlationId = Just uuid }

withComponent :: Text -> LogContext
withComponent comp = emptyContext { component = comp }

withOperation :: Text -> LogContext -> LogContext
withOperation op ctx = ctx { operation = Just op }

addCorrelationId :: UUID -> LogContext -> LogContext
addCorrelationId uuid ctx = ctx { correlationId = Just uuid }

addMetadata :: Text -> Text -> LogContext -> LogContext
addMetadata key value ctx = ctx { metadata = (key, value) : metadata ctx }

defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = LoggerConfig
  { logHandle = stdout
  , logMinLevel = INFO
  , logFormat = PLAIN
  , logTimestampFormat = "%Y-%m-%d %H:%M:%S UTC"
  }

errorLoggerConfig :: LoggerConfig
errorLoggerConfig = defaultLoggerConfig { logHandle = stderr }

debugLoggerConfig :: LoggerConfig
debugLoggerConfig = defaultLoggerConfig { logMinLevel = DEBUG }

logWithLevel :: LogLevel -> LogContext -> Text -> ConsoleLogger ()
logWithLevel level ctx message = ConsoleLogger $ do
  let config = defaultLoggerConfig -- In a real app, this would come from configuration
  when (level >= logMinLevel config) $ do
    timestamp <- getCurrentTime
    let entry = LogEntry level timestamp message ctx
    writeLogEntry config entry

writeLogEntry :: LoggerConfig -> LogEntry -> IO ()
writeLogEntry config entry = do
  let formatted = formatLogEntry config entry
  TIO.hPutStrLn (logHandle config) formatted

formatLogEntry :: LoggerConfig -> LogEntry -> Text
formatLogEntry config entry = case logFormat config of
  PLAIN -> formatPlainLog config entry
  JSON -> formatJsonLog config entry

formatPlainLog :: LoggerConfig -> LogEntry -> Text
formatPlainLog config entry = 
  let timestamp = formatTime defaultTimeLocale (logTimestampFormat config) (logTimestamp entry)
      level = show (logLevel entry)
      componentName = T.unpack (component $ logContext entry)
      correlationPart = case correlationId (logContext entry) of
        Just uuid -> " [" <> UUID.toString uuid <> "]"
        Nothing -> ""
      operationPart = case operation (logContext entry) of
        Just op -> " (" <> T.unpack op <> ")"
        Nothing -> ""
      metadataPart = if null (metadata $ logContext entry)
        then ""
        else " " <> formatMetadata (metadata $ logContext entry)
  in T.pack $ printf "[%s] %-5s %s%s%s: %s%s" 
       timestamp level componentName correlationPart operationPart 
       (T.unpack $ logMessage entry) metadataPart

formatJsonLog :: LoggerConfig -> LogEntry -> Text
formatJsonLog config entry = 
  let timestamp = formatTime defaultTimeLocale (logTimestampFormat config) (logTimestamp entry)
      ctx = logContext entry
      correlationStr = maybe "null" (\uuid -> "\"" <> T.pack (UUID.toString uuid) <> "\"") (correlationId ctx)
      operationStr = maybe "null" (\op -> "\"" <> op <> "\"") (operation ctx)
  in T.pack $ printf 
    "{\"timestamp\":\"%s\",\"level\":\"%s\",\"component\":\"%s\",\"correlationId\":%s,\"operation\":%s,\"message\":\"%s\"}"
    timestamp (show $ logLevel entry) (T.unpack $ component ctx) 
    (T.unpack correlationStr) (T.unpack operationStr) (T.unpack $ logMessage entry)

formatMetadata :: [(Text, Text)] -> String
formatMetadata [] = ""
formatMetadata pairs = "{" <> T.unpack (T.intercalate ", " $ map formatPair pairs) <> "}"
  where
    formatPair (k, v) = k <> "=" <> v

runConsoleLogger :: ConsoleLogger a -> IO a
runConsoleLogger (ConsoleLogger action) = action

when :: Monad m => Bool -> m () -> m ()
when True action = action
when False _ = return ()