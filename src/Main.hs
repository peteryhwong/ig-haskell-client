-- making string literals polymorphic over the 'IsString' type class
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified AuthenticationApi  as A
import qualified LightstreamerApi   as L

import           Lightstreamer      (ConnectionSettings (..),
                                     SubscriptionMode (..), TableOperation (..),
                                     TlsSettings (..))

import           Data.List          (isPrefixOf)
import           System.Environment (getArgs)

authenticate :: [String] -> IO A.AuthenticationResponse
authenticate args =
  maybe (error "invalid param") return (A.parse args)
      >>= \(ev,ak,ar) -> A.authenticate ev ak ar

createConnectionSetting :: A.Hostname -> A.Portnumber -> Bool -> ConnectionSettings
createConnectionSetting ip pn True = ConnectionSettings ip pn (Just (TlsSettings True))
createConnectionSetting ip pn False = ConnectionSettings ip pn Nothing

connect :: A.AuthenticationResponse -> IO (Maybe L.SessionId)
connect (A.AuthenticationResponse un tk ip pn tls) =
  let sreq = L.createStreamRequest (Just (L.Credential un tk)) "DEFAULT"
      csetting = createConnectionSetting ip pn tls
  in L.connect csetting sreq L.SHandler{}

subscribe :: A.AuthenticationResponse -> String -> L.SessionId -> IO ()
subscribe (A.AuthenticationResponse _ _ ip pn tls) epic sid =
  let itemgroup = L.ItemNames [epic]
      fieldSchema = L.FieldNames ["BID", "OFFER", "HIGH", "LOW"]
      tableInfo = L.createTableInfo Nothing Merge itemgroup fieldSchema
      request = L.createSubscriptionRequest sid "1" (TableAdd tableInfo)
  in L.control (createConnectionSetting ip pn tls) request

stream :: [String] -> IO()
stream [env, key, user, pwd, epic] =
  authenticate [env, key, user, pwd]
    >>= (\rp -> connect rp >>= maybe (error "Cannot get id") (subscribe rp epic))
stream _ = error "Invalid Argument"

main :: IO()
main = getArgs >>= stream
