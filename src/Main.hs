-- making string literals polymorphic over the 'IsString' type class
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified IG.AuthenticationApi        as A
import qualified IG.LightstreamerApi         as L

-- Postgres handle
import qualified IG.PostgresStreamHandle     as P

-- For creating efficient string
import           Data.ByteString          (intercalate, append)
import qualified Data.ByteString.Internal as B

-- C printf-like string formatting
import           Text.Printf

import           Lightstreamer            (ConnectionSettings (..),
                                           StreamHandler, StreamRequest,
                                           SubscriptionMode (..),
                                           TableOperation (..),
                                           TlsSettings (..), streamClosed,
                                           streamData)

import           Data.List                (isPrefixOf)
import           System.Environment       (getArgs)

-- To handle child threads
import           Control.Concurrent.MVar
import           Control.Monad

-- Generate UUID
import           Data.UUID
import           Data.UUID.V4

-- Postgresql
import           Database.HDBC.PostgreSQL

-- Timestamp
import           Data.Time.Clock
import           Data.Time.Calendar

-- This encapsulates a 'MVar' for communicating with the thread
-- receiving lightStreamer subscription
newtype StatefulHandler = StatefulHandler { notice :: MVar() }

-- 'StatefulHandler' implements 'StreamHandler' such that when stream is
-- being closed, the internal 'MVar' is supplied with an empty value to notify
-- that the program can terminate
instance StreamHandler StatefulHandler where
  streamData _ = print
  streamClosed handler = putMVar (notice handler) ()

instance P.StatefulStreamHandler StatefulHandler where
  wait handler = void ((takeMVar.notice) handler)

-- Connect to lightStreamer server and subscribe according to 'Subscription'
-- this function uses 'StatefulHandler' (an implementation of 'StreamHandler')
-- to wait and get notification about 'streamClosed'.
lightStreamer :: P.StatefulStreamHandler h => h -> L.Subscription -> L.LSSetting -> IO()
lightStreamer handler sub stg = L.connectAndSubscribe stg handler sub >> P.wait handler

authenticate :: [String] -> IO A.AuthenticationResponse
authenticate args =
  maybe (error "invalid param") return (A.parse args)
    >>= \(ev,ak,ar) -> A.authenticate ev ak ar

-- Convert IG authentication response to IG LightStreamer connecting setting
setting :: A.AuthenticationResponse -> L.LSSetting
setting (A.AuthenticationResponse un tk ip pn tls) = L.LSSetting ip pn tls "DEFAULT" (Just un) (Just tk)

-- epic, env, key, user, pwd
stream :: [String] -> IO()
stream [] =
  examplePostgresHandler
    >>= \h -> lightStreamer h exampleSubscription exampleLightStreamerSetting
stream (epic:args) =
  fmap setting (authenticate args)
    >>= \lss -> createPrintHandler
     >>= \h -> lightStreamer h (createIgSubscription epic) lss

-- Example Connection setting to a local Lightstreamer server according to
-- the Lightstreamer Network Protocol Tutorial
exampleLightStreamerSetting :: L.LSSetting
exampleLightStreamerSetting = L.LSSetting
  { L.lsIP  = "192.168.99.100"
  , L.lsPN  = 80
  , L.lsTLS = False
  , L.lsASN = "WELCOME"
  , L.lsUsername = Nothing
  , L.lsPassword  = Nothing
  }

-- Example Subscription described in the Lightstreamer Network Protocol Tutorial
exampleSubscription :: L.Subscription
exampleSubscription = L.Subscription
  { L.lsItemNames   = ["item2"]
  -- , L.lsFieldNames  = ["bid", "ask", "min", "max", "time"]
  , L.lsFieldNames  = ["bid", "ask", "time"]
  , L.lsTableId     = "2"
  , L.lsDataAdapter = Just "STOCKS"
  }

-- Connects a local database example
examplePostgresHandler :: IO P.PostgresStreamHandler
examplePostgresHandler =
  newEmptyMVar
    >>= \var -> connectPostgreSQL "dbname=example"
      >>= \con -> return (hr var con)
  where hr var con = P.PostgresStreamHandler
                    { P.notice = var
                    , P.conn   = con
                    , P.update = exampleInsert
                    }

-- Today's date
todayDate :: IO Day
todayDate = fmap utctDay getCurrentTime

-- Expects ["bid", "ask", "min", "max", "time"]
-- Generates an insert statement to table 'public.exampleentry'
exampleInsert :: [B.ByteString] -> IO P.SqlQuery
exampleInsert vs = nextUid >>= \uid -> time >>= \tt -> return (printf format uid values tt)
  where format = "INSERT INTO public.exampleentry2 (itemId, bid, ask, entryTime) VALUES (%s,%s,%s)"
  -- where format = "INSERT INTO public.exampleentry2 (itemId, bid, ask, min, max, entryTime) VALUES (%s,%s,%s)"
        -- generate UUID
        nextUid = fmap (quote.toString) nextRandom
        -- generate timestamp without time zone
        time = fmap (quote.(++ (B.unpackChars.last) vs).(++ " ").show) todayDate
        -- generate printf argument
        values = B.unpackChars (intercalate "," (init vs))

-- quote SQL value
quote :: String -> String
quote s = "\'" ++ s ++ "\'"

createPrintHandler :: IO StatefulHandler
createPrintHandler = fmap StatefulHandler newEmptyMVar

-- Example IG Lightstreamer Subscription
createIgSubscription :: String -> L.Subscription
createIgSubscription epic = L.Subscription
  { L.lsItemNames   = [epic]
  , L.lsFieldNames  = ["BID", "OFFER", "HIGH", "LOW"]
  , L.lsTableId     = "1"
  , L.lsDataAdapter = Nothing
  }

main :: IO()
main = getArgs >>= stream
