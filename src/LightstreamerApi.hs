-- making string literals polymorphic over the 'IsString' type class
{-# LANGUAGE OverloadedStrings #-}

module LightstreamerApi where

import           Data.List                (intercalate)
import           Lightstreamer

-- To handle child threads
import           GHC.Conc.Sync            (ThreadId)

-- For creating efficient string
import qualified Data.ByteString.Internal as B

-- For getting command line arguments
import           System.Environment

type CertificateValidation = Bool
-- Hostname
type Hostname = String
-- Port number
type Port = Int
-- Session Id
type SessionId = B.ByteString
-- logical name that identifies the Adapter Set
type AdapterSetName = String
-- Progressive identification number of the Table to which
-- the operation specified by 'TableInfo' applies.
type TableName = B.ByteString
-- A set of field names that have been subscribed to by a Client in relation to
-- an item. A 'FieldSchema' is identified either by an array of field names or by
-- a unique string that should be known to the Client and to the Metadata
-- Adapter.
data FieldSchema = FieldNames [String] | SchemaName String
-- A set of items subscribed to by a Client with a common field schema and a
-- common supplier Data Adapter. An 'ItemGroup' is identified either by an array
-- of item names or by a unique name that should be known to the Client and to
-- the Metadata Adapter.
data ItemGroup = ItemNames [String] | GroupName String

-- Credential for authenticating against the
data Credential = Credential
  { username :: String
  , password :: String
  }

-- The configured name of a Data Adapters available in the Adapter Set
type DataAdapterName = B.ByteString

-- 'ConnectionListener' receives notifications of connection activity
-- class (StreamHandler h) => ConnectionListener h where
--   streamData :: h -> [StreamItem] -> IO ()

-- 'SHandler' receives notifications of connection activity
data SHandler = SHandler

instance StreamHandler SHandler where
  streamData _ = print

-- Create 'StreamRequest' containing credentials, the name of the Adapter Set
-- that serves and provides data for this stream connection.)
createStreamRequest :: Maybe Credential -> AdapterSetName -> StreamRequest
createStreamRequest   Nothing     adapter = defaultStreamRequest adapter
createStreamRequest   (Just cred) adapter = StreamRequest
    { srAdapterSet = adapter
    , srConnectionMode = Left $ KeepAliveMode 600
    , srContentLength = Nothing
    , srPassword = Just $ password cred
    , srRequestedMaxBandwidth = Nothing
    , srReportInfo = Nothing
    , srUser = Just $ username cred
    }

createTableInfo :: Maybe DataAdapterName
                -> SubscriptionMode
                -> ItemGroup
                -> FieldSchema
                -> TableInfo
createTableInfo da sm ig fs = TableInfo
    { tiDataAdapter = da
    , tiId = toTableInfoId ig
    , tiMode = sm
    , tiRequestedBufferSize = Nothing
    , tiRequestedMaxFrequency = Nothing
    , tiSchema = toTableInfoSchema fs
    , tiSelector = Nothing
    , tiSnapshot = Just SnapTrue
    }

toTableInfoId :: ItemGroup -> B.ByteString
toTableInfoId (GroupName nm) = B.packChars nm
toTableInfoId (ItemNames ns) = join "+" ns

toTableInfoSchema :: FieldSchema -> B.ByteString
toTableInfoSchema (SchemaName nm) = B.packChars nm
toTableInfoSchema (FieldNames ns) = join "+" ns

join :: String -> [String] -> B.ByteString
join sep = B.packChars . intercalate sep

-- Create a 'SubscriptionRequest' for subscription control connections.
createSubscriptionRequest :: SessionId
                          -> TableName
                          -> TableOperation
                          -> SubscriptionRequest
createSubscriptionRequest = SubscriptionRequest

-- Create a stream connection/session
-- It returns a 'SessionId', the Lightstreamer Server internal string
-- representing the Session. This string must be sent with every following
-- Control Connection.
connect :: StreamHandler h
        => ConnectionSettings
        -> StreamRequest
        -> h
        -> IO (ThreadId,SessionId)
connect cs sr cl = newStreamConnection cs sr cl >>= either (error.show) (return.extract)
  where extract ctx = (threadId ctx, (sessionId.info) ctx)

-- Table (i.e. subscription) management (creation, activation, deletion).
-- 'SubscriptionRequest' specifies 'TableOperation'.
-- 'TableAdd' creates and activate a new table.
-- 'TableAddSilent' creates a new table (without sending realtime updates).
-- 'TableStart' activate a table previously created with 'TableAddSilent'.
-- 'TableDelete' deletes the specified table.
control :: ConnectionSettings -> SubscriptionRequest -> IO()
control cs sr = subscribe cs sr >>= either (error.show) (\x -> return ())
