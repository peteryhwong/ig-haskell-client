-- making string literals polymorphic over the 'IsString' type class
{-# LANGUAGE OverloadedStrings #-}

module IG.PostgresStreamHandle where

import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           Lightstreamer

-- For creating efficient string
import qualified Data.ByteString as DB (null)
import qualified Data.ByteString.Internal as B

-- To handle child threads
import           Control.Concurrent.MVar
import           Control.Monad

-- | Datatype representing the termination notice
data Notice = Ok | Error String

-- | Type synonym for SQL queries
type SqlQuery = String

-- | Wait for stream to end, this should be a blocking function.
class StreamHandler h => StatefulStreamHandler h where
  wait :: h -> IO()

-- | This encapsulates a 'MVar' for communicating with the thread
-- receiving lightStreamer subscription, and a 'Connection' and
-- an 'update' function for updating specified Postgres database
data PostgresStreamHandler = PostgresStreamHandler
  { notice :: MVar Notice
  , conn   :: Connection
  , update :: [B.ByteString] -> IO SqlQuery
  }

-- | 'PostgresStreamHandler' implements 'StreamHandler' such that when stream is
-- being closed, the internal 'MVar' is supplied with an empty value to notify
-- that the program can terminate
instance StreamHandler PostgresStreamHandler where
  streamCorrupted sink reason = putMVar (notice sink) (Error reason)
  streamData sink = msum.fmap (processItem sink)
  streamClosed sink = (disconnect.conn) sink >> putMVar (notice sink) Ok

-- | 'PostgresStreamHandler' also implements 'StatefulStreamHandler' such that
-- the main thread can wait for the stream to close
instance StatefulStreamHandler PostgresStreamHandler where
  wait sink = void ((takeMVar.notice) sink)

-- | Process 'StreamItem'
processItem :: PostgresStreamHandler -> StreamItem -> IO()
processItem sink (Update entry) =
  let sanitize vs = if any DB.null vs then Nothing else Just vs
  in maybe (print (Update entry)) (updateDb sink) ((sanitize.teValues) entry)
processItem _ items = print items
-- processItem handler End                                = return ()
-- processItem handler (EndSnapshot info)                 = return ()
-- processItem handler (EndWithCause code msg)            = return ()
-- processItem handler Loop                               = return ()
-- processItem handler (Message info)                     = return ()
-- processItem handler (ParseError msg)                   = return ()
-- processItem handler Probe                              = return ()
-- processItem handler (Overflow info)                    = return ()
-- processItem handler (Undefined msg)                    = return ()

-- | Insert and commit an 'Update'
updateDb :: PostgresStreamHandler -> [B.ByteString] -> IO()
updateDb sink vs = update sink vs >>= \q -> print q >> run (conn sink) q [] >> commit (conn sink)
