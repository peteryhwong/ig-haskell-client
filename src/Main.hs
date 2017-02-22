-- making string literals polymorphic over the 'IsString' type class
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified AuthenticationApi        as A
import qualified LightstreamerApi         as L

-- For creating efficient string
import qualified Data.ByteString.Internal as B

import           Lightstreamer            (ConnectionSettings (..),
                                           StreamRequest, SubscriptionMode (..),
                                           TableOperation (..),
                                           TlsSettings (..))

import           Data.List                (isPrefixOf)
import           System.Environment       (getArgs)

-- To handle child threads
import           GHC.Conc.Sync
import           Control.Monad            (when)

-- Settings for connecting to a Lightstreamer server
data LSSetting = LSSetting
  { lsIP  :: L.Hostname
  , lsPN  :: L.Port
  , lsTLS :: Bool
  , lsASN :: L.AdapterSetName
  , lsUn  :: Maybe String
  , lsPW  :: Maybe String
  } deriving (Show)

  -- Settings for subscribing to a stream
data Subscription = Subscription
  { lsFieldNames  :: [String]
  , lsItemNames   :: [String]
  , lsTableId     :: B.ByteString
  , lsDataAdapter :: Maybe B.ByteString
  } deriving (Show)

connectionSetting :: L.Hostname -> L.Port -> Bool -> ConnectionSettings
connectionSetting ip pn True = ConnectionSettings ip pn (Just (TlsSettings True))
connectionSetting ip pn False = ConnectionSettings ip pn Nothing

connect :: LSSetting -> IO (ThreadId, L.SessionId)
connect (LSSetting ip pn tls aName un tk) =
  let sreq = streamRequest (LSSetting ip pn tls aName un tk)
      csetting = connectionSetting ip pn tls
  in L.connect csetting sreq L.SHandler

streamRequest :: LSSetting -> StreamRequest
streamRequest (LSSetting _ _ _ an (Just un) (Just tk)) = L.createStreamRequest (Just (L.Credential un tk)) an
streamRequest (LSSetting _ _ _ an _ _) = L.createStreamRequest Nothing an

subscribe :: LSSetting -> Subscription -> L.SessionId -> IO ()
subscribe (LSSetting ip pn tls _ _ _) sub sid =
  let itemgroup = L.ItemNames (lsFieldNames sub)
      fieldSchema = L.FieldNames (lsItemNames sub)
      tableInfo = L.createTableInfo (lsDataAdapter sub) Merge itemgroup fieldSchema
      request = L.createSubscriptionRequest sid (lsTableId sub) (TableAdd tableInfo)
  in L.control (connectionSetting ip pn tls) request

lightStreamer :: Subscription -> LSSetting -> IO()
lightStreamer sub stg = connect stg >>= \(tid, sid) -> subscribe stg sub sid >> test tid
    where isLive = flip notElem [ThreadFinished, ThreadDied]
          test tid = threadStatus tid >>= \status -> when (isLive status) $ test tid

authenticate :: [String] -> IO A.AuthenticationResponse
authenticate args =
  maybe (error "invalid param") return (A.parse args)
      >>= \(ev,ak,ar) -> A.authenticate ev ak ar

-- Convert IG authentication response to IG LightStreamer connecting setting
setting :: A.AuthenticationResponse -> LSSetting
setting (A.AuthenticationResponse un tk ip pn tls) = LSSetting ip pn tls "DEFAULT" (Just un) (Just tk)

-- epic, env, key, user, pwd
stream :: [String] -> IO()
stream [] = lightStreamer exampleSubscription exampleLightStreamerSetting
stream (epic:args) = fmap setting (authenticate args) >>= lightStreamer (createIgSubscription epic)

-- Example Connection setting to a local Lightstreamer server according to
-- the Lightstreamer Network Protocol Tutorial
exampleLightStreamerSetting :: LSSetting
exampleLightStreamerSetting = LSSetting
  { lsIP  = "192.168.99.100"
  , lsPN  = 80
  , lsTLS = False
  , lsASN = "WELCOME"
  , lsUn  = Nothing
  , lsPW  = Nothing
  }

-- Example Subscription described in the Lightstreamer Network Protocol Tutorial
exampleSubscription :: Subscription
exampleSubscription = Subscription
  { lsFieldNames  = ["item2", "item11", "item18"]
  , lsItemNames   = ["last_price", "time", "pct_change", "bid_quantity", "bid", "ask", "ask_quantity", "min", "max", "ref_price", "open_price"]
  , lsTableId     = "2"
  , lsDataAdapter = Just "STOCKS"
  }

-- Example IG Lightstreamer Subscription
createIgSubscription :: String -> Subscription
createIgSubscription epic = Subscription
  { lsFieldNames  = [epic]
  , lsItemNames   = ["BID", "OFFER", "HIGH", "LOW"]
  , lsTableId     = "1"
  , lsDataAdapter = Nothing
  }

main :: IO()
main = getArgs >>= stream
