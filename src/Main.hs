-- making string literals polymorphic over the 'IsString' type class
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified AuthenticationApi        as A
import qualified LightstreamerApi         as L

-- For creating efficient string
import qualified Data.ByteString.Internal as B

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
import           Control.Monad            (when)
import           GHC.Conc.Sync

-- This encapsulates a 'MVar' for communicating with the thread
-- receiving lightStreamer subscription
newtype StatefulHandler = StatefulHandler { streamEnd :: MVar() }

-- 'StatefulHandler' implements 'StreamHandler' such that when stream is
-- being closed, the internal 'MVar' is supplied with an empty value to notify
-- that the program can terminate
instance StreamHandler StatefulHandler where
  streamData _ = print
  streamClosed handler = putMVar (streamEnd handler) ()

-- Connect to lightStreamer server and subscribe according to 'Subscription'
-- this function uses 'StatefulHandler' (an implementation of 'StreamHandler')
-- to wait and get notification about 'streamClosed'.
lightStreamer :: L.Subscription -> L.LSSetting -> IO()
lightStreamer sub stg = fmap StatefulHandler newEmptyMVar
  >>= (\handler -> L.connectAndSubscribe stg handler sub >> (takeMVar.streamEnd) handler)

authenticate :: [String] -> IO A.AuthenticationResponse
authenticate args =
  maybe (error "invalid param") return (A.parse args)
    >>= \(ev,ak,ar) -> A.authenticate ev ak ar

-- Convert IG authentication response to IG LightStreamer connecting setting
setting :: A.AuthenticationResponse -> L.LSSetting
setting (A.AuthenticationResponse un tk ip pn tls) = L.LSSetting ip pn tls "DEFAULT" (Just un) (Just tk)

-- epic, env, key, user, pwd
stream :: [String] -> IO()
stream [] = lightStreamer exampleSubscription exampleLightStreamerSetting
stream (epic:args) = fmap setting (authenticate args) >>= lightStreamer (createIgSubscription epic)

-- Example Connection setting to a local Lightstreamer server according to
-- the Lightstreamer Network Protocol Tutorial
exampleLightStreamerSetting :: L.LSSetting
exampleLightStreamerSetting = L.LSSetting
  { L.lsIP  = "192.168.99.100"
  , L.lsPN  = 80
  , L.lsTLS = False
  , L.lsASN = "WELCOME"
  , L.lsUn  = Nothing
  , L.lsPW  = Nothing
  }

-- Example Subscription described in the Lightstreamer Network Protocol Tutorial
exampleSubscription :: L.Subscription
exampleSubscription = L.Subscription
  { L.lsFieldNames  = ["item2", "item11", "item18"]
  , L.lsItemNames   = ["last_price", "time", "pct_change", "bid_quantity", "bid", "ask", "ask_quantity", "min", "max", "ref_price", "open_price"]
  , L.lsTableId     = "2"
  , L.lsDataAdapter = Just "STOCKS"
  }

-- Example IG Lightstreamer Subscription
createIgSubscription :: String -> L.Subscription
createIgSubscription epic = L.Subscription
  { L.lsFieldNames  = [epic]
  , L.lsItemNames   = ["BID", "OFFER", "HIGH", "LOW"]
  , L.lsTableId     = "1"
  , L.lsDataAdapter = Nothing
  }

main :: IO()
main = getArgs >>= stream
