module Stream where

import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           Lightstreamer

-- For creating efficient string
import qualified Data.ByteString.Internal as B

-- To handle child threads
import           Control.Concurrent.MVar
import           Control.Monad

import           Text.Printf

-- Datatype representing the termination notice
data Notice = Ok | Error String

-- | Synonym type for SQL queries
type SqlQuery = String

-- This encapsulates a 'MVar' for communicating with the thread
-- receiving lightStreamer subscription
data PostgresStreamHandler = PostgresStreamHandler
  { notice     :: MVar Notice
  , connection :: Maybe Connection
  , update     :: Maybe ([B.ByteString] -> SqlQuery)
  }

-- 'PostgresStreamHandler' implements 'StreamHandler' such that when stream is
-- being closed, the internal 'MVar' is supplied with an empty value to notify
-- that the program can terminate
instance StreamHandler PostgresStreamHandler where
  streamData handler = msum.fmap (processItem handler)
  streamClosed handler = maybe (return ()) disconnect (connection handler) >> putMVar (notice handler) Ok
  streamCorrupted handler reason = putMVar (notice handler) (Error reason)

processItem :: PostgresStreamHandler -> StreamItem -> IO()
-- processItem handler End                                = return ()
-- processItem handler (EndSnapshot info)                 = return ()
-- processItem handler (EndWithCause code msg)            = return ()
-- processItem handler Loop                               = return ()
-- processItem handler (Message info)                     = return ()
-- processItem handler (ParseError msg)                   = return ()
-- processItem handler Probe                              = return ()
-- processItem handler (Overflow info)                    = return ()
-- processItem handler (Undefined msg)                    = return ()
processItem handler (Update (TableEntry item name vs)) = handleUpdate handler vs
processItem handler item = print item

handleUpdate :: PostgresStreamHandler -> [B.ByteString] -> IO()
handleUpdate handler [] = return ()
handleUpdate handler xs =
  case connection handler of
    Just conn -> case update handler of
                    Just fun -> void (run conn (fun xs) [])
                    Nothing -> return ()
    Nothing -> return ()
