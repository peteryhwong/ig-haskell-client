-- making string literals polymorphic over the 'IsString' type class
{-# LANGUAGE OverloadedStrings #-}
-- The LANGUAGE pragma (and Generic instance) let us write empty FromJSON and
-- ToJSON instances for which the compiler will generate sensible default
-- implementations.
{-# LANGUAGE DeriveGeneric     #-}

module IG.AuthenticationApi where

-- For processing HTTP JSON response
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens               (key, _String)
import qualified Data.Text                     as T
import           GHC.Generics

-- For making HTTP request
import qualified Data.ByteString.Lazy.Char8    as LBC
import qualified Data.ByteString.Lazy.Internal as LB
import qualified Network.HTTP.Types.Header     as H
import           Network.Wreq

-- For getting command line arguments
import           System.Environment            (getArgs)

-- For resolving hostname
import qualified Data.ByteString.Internal      as B
import           Data.List                     (isPrefixOf)
import           Network.DNS.Lookup
import           Network.DNS.Resolver

type Environment = String
type ApiKey = String
type Hostname = String
type Portnumber = Int

-- | AuthenticationRequest
data AuthenticationRequest = AuthenticationRequest
    { identifier        :: String
    , password          :: String
    , encryptedPassword :: Maybe Bool
    } deriving (Show, Generic)

instance ToJSON AuthenticationRequest

toRequest :: String -> String -> AuthenticationRequest
toRequest un pw = AuthenticationRequest
    { identifier = un
    , password = pw
    , encryptedPassword = Nothing
    }

toRequestBody :: AuthenticationRequest -> String
toRequestBody = LBC.unpack.encode

data AuthenticationResponse = AuthenticationResponse
    { username  :: String
    , token     :: String
    , ipAddress :: Hostname
    , port      :: Portnumber
    , tls       :: Bool
    } deriving (Show)

-- | Check to see if the given string is prefixed with 'https://'
isTLS :: String -> Bool
isTLS = isPrefixOf "https://"

-- | Assume default ports for http and https
portNumber :: String -> Portnumber
portNumber endpoint = if isTLS endpoint then 443 else 80

-- | Drop the scheme (http:// or https://)
stripScheme :: String -> Hostname
stripScheme endpoint = drop (if isTLS endpoint then 8 else 7) endpoint

-- | Resolves the given endpoint using the system's default /etc/resolv.conf
-- into IPv4 address, 'Portnumber' and whether
-- it accepts TLS connection over HTTP
resolve :: String -> IO (Hostname, Portnumber, Bool)
resolve endpoint =
    makeResolvSeed defaultResolvConf
      >>= resolve' ((LBC.toStrict.LBC.pack.stripScheme) endpoint)
        >>= \ip -> return (ip, portNumber endpoint, isTLS endpoint)

-- | Resolves the given hostname into IPv4 address using the given DNS Resolver seed
resolve' :: B.ByteString -> ResolvSeed -> IO String
resolve' hostname seed =
    withResolver seed (`lookupA` hostname)
      >>= either (error.show) (return.show.head)

-- | Include X-IG-API-KEY and Content-Type as HTTP headers
createHeaders :: ApiKey -> Options
createHeaders key = toHeaders [("X-IG-API-KEY", strictKey), ("Content-Type", "application/json")]
    where strictKey = (LBC.toStrict.LB.packChars) key
          toHeaders = foldr (\(h,v) hdr -> hdr & header h .~ [v]) defaults

getTopLevelField :: Response LB.ByteString -> T.Text -> String
getTopLevelField resp name = T.unpack (resp ^. responseBody . key name . _String)

getHeader :: Response LB.ByteString -> H.HeaderName -> String
getHeader resp name = (LBC.unpack.LBC.fromStrict) (resp ^. responseHeader name)

parseResponse :: Response LB.ByteString -> IO AuthenticationResponse
parseResponse resp =
    resolve (getTopLevelField resp "lightstreamerEndpoint") >>=
      (\(hostname, portnumber, tls) ->
          let
            cstToken = getHeader resp "CST"
            securityToken = getHeader resp "X-SECURITY-TOKEN"
            response = AuthenticationResponse
                { username = getTopLevelField resp "currentAccountId"
                , token = "CST-" ++ cstToken ++ "|" ++ "XST-" ++ securityToken
                , ipAddress = hostname
                , port = portnumber
                , tls = tls
                }
          in return response)

authenticate :: Environment
             -> ApiKey
             -> AuthenticationRequest
             -> IO AuthenticationResponse
authenticate env key auth =
    let session = env ++ "/session"
    in postWith (createHeaders key) session (encode auth) >>= parseResponse

parse :: [String] -> Maybe (Environment, ApiKey, AuthenticationRequest)
parse [env, apiKey, uname, pword] = Just (env, apiKey, toRequest uname pword)
parse _                           = Nothing
