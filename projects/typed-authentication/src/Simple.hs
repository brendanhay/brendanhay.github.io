{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- |
-- Module      : Simple
-- Copyright   : (c) 2016 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Simple where

import           Control.Monad              (unless, void)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT (..), asks)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as Build
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.Coerce                (coerce)
import           Data.Default.Class         (def)
import           Data.List                  (intersperse)
import           Data.Monoid                ((<>))
import           Data.String
import           Network.HTTP.Client        (BodyReader, Manager, Request,
                                             RequestBody (..), Response)
import qualified Network.HTTP.Client        as Client
import qualified Network.HTTP.Types         as HTTP
import           System.Exit                (exitFailure)
import           System.Info                (os)
import           System.Process             (rawSystem)

newtype Bucket = Bucket ByteString deriving (Eq, Show, IsString)
newtype Key    = Key    ByteString deriving (Eq, Show, IsString)

objectPath :: Bucket -> Key -> BS.ByteString
objectPath (Bucket bkt) (Key key) =
    LBS.toStrict ("storage/v1/b/" <> bkt <> "/o/" <> key)

data InsertObject = InsertObject Bucket Key RequestBody
data GetObject    = GetObject    Bucket Key
data DeleteObject = DeleteObject Bucket Key

newtype Scope = Scope ByteString deriving (Eq, Show, IsString)

queryEncodeScopes :: [Scope] -> ByteString
queryEncodeScopes =
      Build.toLazyByteString
    . mconcat
    . intersperse "+"
    . map (HTTP.urlEncodeBuilder True . LBS.toStrict)
    . coerce

fullControl, readOnly, readWrite :: Scope
fullControl = "https://www.googleapis.com/auth/devstorage.full_control"
readOnly    = "https://www.googleapis.com/auth/devstorage.read_only"
readWrite   = "https://www.googleapis.com/auth/devstorage.read_write"

class ToRequest a where
    toRequest :: a -> Request

instance ToRequest InsertObject where
    toRequest (InsertObject bkt key bdy) =
        def { Client.secure      = True
            , Client.host        = "https://www.googleapis.com"
            , Client.method      = "POST"
            , Client.path        = objectPath bkt key
            , Client.requestBody = bdy
            }

instance ToRequest GetObject where
    toRequest (GetObject bkt key) =
        def { Client.secure = True
            , Client.host   = "https://www.googleapis.com"
            , Client.method = "GET"
            , Client.path   = objectPath bkt key
            }

instance ToRequest DeleteObject where
    toRequest (DeleteObject bkt key) =
        def { Client.secure = True
            , Client.host   = "https://www.googleapis.com"
            , Client.method = "DELETE"
            , Client.path   = objectPath bkt key
            }

newtype Code  = Code  ByteString deriving (Eq, Show, IsString)
newtype Token = Token ByteString

authorise :: Request -> Token -> Request
authorise rq (Token t) = rq
    { Client.requestHeaders =
        (HTTP.hAuthorization, LBS.toStrict ("Bearer: " <> t))
            : filter ((HTTP.hAuthorization /=) . fst) (Client.requestHeaders rq)
    }

data Client = Client
    { identifier :: ByteString
    , secret     :: ByteString
    }

redirectURI :: ByteString
redirectURI = "urn:ietf:wg:oauth:2.0:oob"

formURL :: Client -> [Scope] -> ByteString
formURL c ss =
       "https://accounts.google.com/o/oauth2/auth"
    <> "?response_type=code"
    <> "&redirect_uri=" <> redirectURI
    <> "&client_id="    <> identifier c
    <> "&scope="        <> queryEncodeScopes ss

redirectPrompt :: Client -> [Scope] -> IO Code
redirectPrompt c ss = do
    let url = LBS8.unpack (formURL c ss)
    putStrLn $ "Opening URL " ++ url
    case os of
        "darwin" -> rawSystem "open"      [url]
        "linux"  -> rawSystem "gvfs-open" [url]
        _        -> putStrLn "Unsupported OS" >> exitFailure
    putStrLn "Please input the authorisation code: "
    Code . LBS.fromStrict <$> BS.getLine

exchangeCode :: Client -> Code -> Manager -> IO Token
exchangeCode c (Code n) m =
    Token . Client.responseBody <$> Client.httpLbs rq m
  where
    rq = def
        { Client.host           = "accounts.google.com"
        , Client.port           = 443
        , Client.secure         = True
        , Client.method         = "POST"
        , Client.path           = "/o/oauth2/token"
        , Client.requestHeaders =
            [(HTTP.hContentType, "application/x-www-form-urlencoded")]
        , Client.requestBody    =
            RequestBodyLBS $
                   "grant_type=authorization_code"
                <> "&client_id="     <> identifier c
                <> "&client_secret=" <> secret     c
                <> "&code="          <> n
                <> "&redirect_uri="  <> redirectURI
        }

data Env = Env
    { manager :: Manager
    , token   :: Token
    }

type Context = ReaderT Env IO

runContext :: Env -> Context a -> IO a
runContext = flip runReaderT

send :: ToRequest a => a -> Context (Response ByteString)
send x = do
    rq <- asks (authorise (toRequest x) . token)
    m  <- asks manager
    lift (Client.httpLbs rq m)

exampleReadWrite :: Client -> IO ()
exampleReadWrite c = do
    m <- Client.newManager Client.defaultManagerSettings
    n <- redirectPrompt c [readWrite, readOnly]
    t <- exchangeCode c n m

    runContext (Env m t) $ do
        let bucket  = "bucket"
            key     = "object/key/payload.txt"
            payload = "foo"

        void . send $ InsertObject bucket key payload
        void . send $ GetObject    bucket key
