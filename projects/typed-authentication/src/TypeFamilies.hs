{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
-- Module      : TypeFamilies
-- Copyright   : (c) 2016 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module TypeFamilies where


import           Control.Monad              (unless, void)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           System.Exit                (exitFailure)
import           System.Info                (os)
import           System.Process             (rawSystem)
import           Data.Type.Bool               (type (||))
import           Data.Type.Equality           (type (==))
import GHC.Exts (Constraint)
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
import           Data.Proxy                 (Proxy (..))
import           Data.String
import           GHC.TypeLits               (KnownSymbol, Symbol, symbolVal)
import           Network.HTTP.Client        (BodyReader, Manager, Request,
                                             RequestBody (..), Response)
import qualified Network.HTTP.Client        as Client
import qualified Network.HTTP.Types         as HTTP

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

type FullControl = "https://www.googleapis.com/auth/devstorage.full_control"
type ReadOnly    = "https://www.googleapis.com/auth/devstorage.read_only"
type ReadWrite   = "https://www.googleapis.com/auth/devstorage.read_write"

class ToRequest a where
    type Scopes a :: [Symbol]

    toRequest :: a -> Request

instance ToRequest InsertObject where
    type Scopes InsertObject = '[FullControl, ReadWrite]

    toRequest (InsertObject bkt key bdy) =
        def { Client.method      = "POST"
            , Client.path        = objectPath bkt key
            , Client.requestBody = bdy
            }

instance ToRequest GetObject where
    type Scopes GetObject = '[FullControl, ReadOnly]

    toRequest (GetObject bkt key) =
        def { Client.method = "GET"
            , Client.path   = objectPath bkt key
            }

instance ToRequest DeleteObject where
    type Scopes DeleteObject = '[FullControl]

    toRequest (DeleteObject bkt key) =
        def { Client.method = "DELETE"
            , Client.path   = objectPath bkt key
            }

newtype Code  s = Code  ByteString deriving (Eq, Show, IsString)
newtype Token s = Token ByteString

data Client = Client
    { identifier :: ByteString
    , secret     :: ByteString
    }

class GetScopes a where
    getScopes :: proxy a -> [Scope]

instance GetScopes '[] where
    getScopes _ = []

instance (KnownSymbol x, GetScopes xs) => GetScopes (x ': xs) where
    getScopes _ = scope (Proxy :: Proxy x) : getScopes (Proxy :: Proxy xs)
      where
        scope = Scope . LBS8.pack . symbolVal

redirectURI :: ByteString
redirectURI = "urn:ietf:wg:oauth:2.0:oob"

formURL :: GetScopes s => Client -> Proxy s -> ByteString
formURL c p =
       "https://accounts.google.com/o/oauth2/token"
    <> "?response_type=code"
    <> "&redirect_uri=" <> redirectURI
    <> "&client_id="    <> identifier c
    <> "&scope="        <> queryEncodeScopes (getScopes p)

redirectPrompt :: forall s. GetScopes s => Client -> IO (Code s)
redirectPrompt c = do
    let url = LBS8.unpack (formURL c (Proxy :: Proxy s))
    putStrLn $ "Opening URL " ++ url
    case os of
        "darwin" -> rawSystem "open"      [url]
        "linux"  -> rawSystem "gvfs-open" [url]
        _        -> putStrLn "Unsupported OS" >> exitFailure
    putStrLn "Please input the authorisation code: "
    Code . LBS.fromStrict <$> BS.getLine

exchangeCode :: Client -> Code s -> Manager -> IO (Token s)
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

data Env s = Env
    { client  :: Client
    , token   :: Token s
    , manager :: Manager
    }

type Context s = ReaderT (Env s) IO

runContext :: Env s -> Context s a -> IO a
runContext = flip runReaderT

type family HasScope (s :: [Symbol]) a :: Constraint where
    HasScope s a = (s `HasScope'` Scopes a) ~ 'True

type family HasScope' s a where
    HasScope' s         '[] = 'True
    HasScope' (x ': xs) a   = x ∈ a || HasScope' xs a

type family (∈) a b where
    (∈) x '[]       = 'False
    (∈) x (y ': xs) = x == y || x ∈ xs

type family (++) xs ys where
    (++) xs       '[]       = xs
    (++) '[]       ys        = ys
    (++) (x ': xs) ys = x ': (xs ++ ys)

send :: (ToRequest a, HasScope s a) => a -> Context s (Response ByteString)
send x = asks manager >>= lift . Client.httpLbs (toRequest x)

exampleReadWrite :: Client -> IO ()
exampleReadWrite c = do
    m <- Client.newManager Client.defaultManagerSettings
    n <- redirectPrompt c
    t <- exchangeCode c n m

    let env = Env c t m :: Env '[ReadWrite, ReadOnly]

    runContext env $ do
        let bucket  = "bucket"
            key     = "object/key/payload.txt"
            payload = "foo"

        void . send $ InsertObject bucket key payload
        void . send $ GetObject    bucket key

exampleDelete :: Client -> IO ()
exampleDelete c = do
    m <- Client.newManager Client.defaultManagerSettings
    n <- redirectPrompt c
    t <- exchangeCode c n m

    let env = Env c t m :: Env '[FullControl]

    runContext env $ do
        let bucket  = "bucket"
            key     = "object/key/payload.txt"
            payload = "foo"

        void . send $ InsertObject bucket key payload
        void . send $ GetObject    bucket key
        void . send $ DeleteObject bucket key
