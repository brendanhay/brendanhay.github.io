---
draft: true
layout: post
title: Typed Authorisation Scopes
---

# DRAFT

> Example project here: https://github.com/brendanhay/brendanhay.github.io/tree/master/projects/typed-authentication

Previously in the [`gogol`](http://hackage.haskell.org/packages/#cat:Google)
libraries, you would supply the credentials to the top-level
[`runGoogle`](https://github.com/brendanhay/gogol/blob/0.0.1/gogol/src/Network/Google.hs#L174-L176)
function (which unwraps the
[`Google`](https://github.com/brendanhay/gogol/blob/0.0.1/gogol/src/Network/Google.hs#L119)
monad) and any remote API operations performed within that context would be
assumed to have the correct scopes authorised, otherwise a run-time error from
the remote API denoting forbidden or invalid access-levels would be raised.

A contrived example of this usage for storing/retrieving the same object to
Google Storage is:

{% highlight haskell %}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens                 ((?~))
import Control.Monad.Trans.Resource (runResourceT)
import Data.Function                ((&))
import Network.Google               (runGoogle, newEnv, sourceBody)
import Network.Google.Storage       (objectsGet, objectsInsert, oiName)

main :: IO
main = do
    let file = "/tmp/file-path.xz"
        bkt  = "storage-bucket"
        key  = "/prefix/key"
        obj  = object' & objContentType ?~ "application/octet-stream"

    -- Which credentials are retrieved is determined identically
    -- to the other Google SDKs by the following 'newEnv' call:
    env  <- newEnv
    body <- sourceBody file

    runResourceT . runGoogle env $ do
        _ <- upload   (objectsInsert bkt obj & oiName ?~ key) bdy
        _ <- download (objectsGet bkt key)
        pure ()
{% endhighlight %}

The problem with this approach is when the credentials are retrieved
via `newEnv` (calling the underlying [`getAuth`](https://github.com/brendanhay/gogol/blob/0.0.1/gogol/src/Network/Google/Auth.hs#L203-L217))
there is no check for correspondence between the scopes the code within the `runGoogle` context
requires, and the discovered credentials.

Ryan Newton
[raised the idea](https://github.com/brendanhay/gogol/issues/7#issuecomment-151133749)
that having strongly typed scopes could mitigate invalid credentials, or at
least self-document the authorisation requirements a particular segment of
code has. This post explores a small example of using the type system, specifically [data type promotion](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#ghc-flag--XDataKinds), [type-level literals](singletons), and [type-level lists](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#promoted-list-and-tuple-types)) to implement a simpler
version of what will be released as part of [`gogol-0.2`](http://hackage.haskell.org/package/gogol).

Various assumptions about you, the reader, include familiarity with
Google's Services, Haskell and general HTTP API communication are made.

# Google's Use of OAuth

Google secures their public facing APIs using [OAuth2](https://developers.google.com/identity/protocols/OAuth2).

In all our examples we'll be using the [Installed Application](https://developers.google.com/identity/protocols/OAuth2InstalledApp) flow since it is
identical to the [Web Server Application](https://developers.google.com/identity/protocols/OAuth2WebServer) flow and doesn't require us to serve
callback URI.

Of particular note is how OAuth scopes are used to break up permissions
required to perform the various operations that the APIs expose. For example,
if one wished to programmatically read an object stored in Google Storage
you would required the following OAuth scope to be authorised for the credentials
that are used to authenticate with the API:

{% highlight haskell %}
"https://www.googleapis.com/auth/devstorage.read_only"
{% endhighlight %}

If an attempt was made to write an object to Google Storage and the credentials
used only had the above scope, an authorisation error would occur.

To allow write access (which naturally permits read access), the following scope
would be used:

{% highlight haskell %}
"https://www.googleapis.com/auth/devstorage.read_write"
{% endhighlight %}

And likewise, to perform further administrative operations on metadata and buckets
you would required the administrative scope:

{% highlight haskell %}
"https://www.googleapis.com/auth/devstorage.full_control"
{% endhighlight %}

Each of these scopes permits the previous, that is, they are a hierarchy of permissions.

For each API, Google exposes a number of differing scopes that allow granular control
over the operations an API client can perform.

It is generally a best practice to request scopes incrementally, at the time
access is required, rather than up front. For example, an app that wants to
support purchases should not request Google Wallet access until the user
presses the “buy” button; see Incremental authorization.





The credentials for the Web Service Application would work like ..

Create web application credentials for your project in the Developers Console
and obtain the client-id and client-secret, which will look something like:

We'll use InstalledApplication since it doesn't require running a local HTTP server
to receive the OAuth callback like the Web Server Application flow.

{% highlight yaml %}
client-id: 1234567890.dns.apps.googleusercontent.com
client-secret: aXfvg40-_Splqf349fZtvn
{% endhighlight %}

This will be represented in Haskell as:

{% highlight haskell %}
data Client = Client
    { identifier :: ByteString
    , secret     :: ByteString
    }
{% endhighlight %}

# An Oversimplification of the Domain

To illustrate the use of OAuth scopes for authorization purposes, we'll use a
faux Google Cloud Storage API (Google Storage).

The simplified API will have 3 operations, each requiring a different scope:

* InsertObject
  - Write a new object payload, addressed by a storage bucket and key.
  - `https://www.googleapis.com/auth/devstorage.read_write` or `https://www.googleapis.com/auth/devstorage.full_control`
  - The actual Google Storage API accepts `multipart/related` Content-Type, with
    object metadata and payload sent in a delimited multipart stream.
* GetObject
  - Retrieve an existing object, via bucket and key.
  - `https://www.googleapis.com/auth/devstorage.read_only` or `https://www.googleapis.com/auth/devstorage.full_control`
* DeleteObject
  - remove an existing object, via bucket and key.
  - `https://www.googleapis.com/auth/devstorage.full_control`

These operations will be represented by concrete data types, which, for this
trivial example is overkill compared to a function per operation. In an API where
operations have large numbers of parameters, passing only the required parameters
to a smart constructor which instantiates an underlying data type by setting
default and optional parameters proves more manageable. See Gogol + Amazonka examples.

{% highlight haskell %}
newtype Bucket = Bucket ByteString deriving (Eq, Show, IsString)
newtype Key    = Key    ByteString deriving (Eq, Show, IsString)

data InsertObject = InsertObject Bucket Key RequestBody
{% endhighlight %}

Each operation will be an HTTP request to the `www.googleapis.com/storage/v1` endpoint
and will return a streaming HTTP response upon success.

{% highlight haskell %}

{% endhighlight %}

# The Simple Version

Specify credentials by passing scopes to form a url, and exchange that for a code


The scopes will be represented by newtypes:

{% highlight haskell %}
newtype Scope = Scope ByteString deriving (Eq, Show, IsString)

fullControl, readOnly, readWrite :: Scope
fullControl = "https://www.googleapis.com/auth/devstorage.full_control"
readOnly    = "https://www.googleapis.com/auth/devstorage.read_only"
readWrite   = "https://www.googleapis.com/auth/devstorage.read_write"
{% endhighlight %}

To obtain an OAuth code that can be exchanged for an access token per the
Web Server Application flow, we first convert the list of scopes that we
wish the user to authorise to a query string:

{% highlight haskell %}
queryEncodeScopes :: [Scope] -> ByteString
queryEncodeScopes =
      Build.toLazyByteString
    . mconcat
    . intersperse "+"
    . map (HTTP.urlEncodeBuilder True . LBS.toStrict)
    . coerce
{% endhighlight %}

> `Data.Coerce.coerce` is used here to safely un-newtype the list of scopes to their
underlying bytestring. More detail.

Then, the `Client` identifier can be used to form a URL that the user will be
directed to, to confirm authorisation of the desired scopes:

{% highlight haskell %}
formURL :: Client -> [Scope] -> ByteString
formURL c ss =
       "https://accounts.google.com/o/oauth2/token"
    <> "?response_type=code"
    <> "&redirect_uri=" <> redirectURI
    <> "&client_id="    <> identifier c
    <> "&scope="        <> queryEncodeScopes ss
{% endhighlight %}

Pasting the URL created by `formURL` into your browser, will prompt you to authorise
the specified scopes for the given client identifier. Confirming this will display
an OAuth code that can be copied and pasted back into our toy application:

{% highlight haskell %}
newtype Code = Code  ByteString deriving (Eq, Show, IsString)

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
{% endhighlight %}

This code can then be used to obtain a valid access token:

{% highlight haskell %}
newtype Token = Token ByteString

exchangeCode :: Client -> Manager -> Code -> IO Token
exchangeCode c m (Code code) =
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
                <> "&code="          <> code
                <> "&redirect_uri="  <> redirectURI
        }
{% endhighlight %}

Now we have a valid access token authorised to the desired scopes, we can make
actual API requests.

# A Basic Request Environment

To provide a uniform interface for serialisation of our operation datatypes
into HTTP requests, a simplistic typeclass is introduced:

{% highlight haskell %}
class ToRequest a where
    toRequest :: a -> Request
{% endhighlight %}

And a `ReaderT` is used to pass around the environment:

{% highlight haskell %}
data Env = Env
    { client  :: Client
    , token   :: Token
    , manager :: Manager
    }

type Context = ReaderT Env IO

runContext :: Client -> Token -> Manager -> Context a -> IO a
runContext c t m = flip runReaderT (Env c t m)
{% endhighlight %}

`Context` and `ToRequest` are then combined to provide a uniform interface to
send our operation data types and receive a raw HTTP response for `2xx` status codes:

{% highlight haskell %}
send :: ToRequest a => a -> Context (Response ByteString)
send x = asks manager >>= lift . Client.httpLbs (toRequest x)
{% endhighlight %}

For example:

{% highlight haskell %}
example :: Client -> Code -> IO ()
example c code = do
    m <- Client.newManager Client.defaultManagerSettings
    t <- exchangeCode c code m

    runContext c t m $ do
        let bucket  = "bucket"
            key     = "object/key/payload.txt"
            payload = "foo"

        void . send $ InsertObject bucket key payload
        void . send $ GetObject    bucket key
{% endhighlight %}

If we run the above example - how would we know the `Code` used was authorised
with the correct scopes for all three operations above? That is, either `fullControl`
or both `readOnly` and `readWrite` should have been authorised by the user or
an authorisation error will occur at runtime when communicating with the remote API.

# Annotating Requests with Required Scopes

Since we know the scopes each respective request within a given `Context` requires,
how can we ensure the user is prompted with the correct scopes to authorise?

Firstly, we'll make a modification to the `ToRequest` class to annotate each request
with its respective scopes:

{% highlight haskell %}
class ToRequest a where
    type Scopes = [Symbol]

    toRequest :: a -> Request
{% endhighlight %}

This results in:

{% highlight haskell %}
instance ToRequest InsertObject where
    type Scopes InsertObject =
        '[ "https://www.googleapis.com/auth/devstorage.read_write"
         , "https://www.googleapis.com/auth/devstorage.full_control"
         ]
    ...

instance ToRequest GetObject where
    type Scopes GetObject =
        '[ "https://www.googleapis.com/auth/devstorage.read_only"
         , "https://www.googleapis.com/auth/devstorage.full_control"
         ]
    ...

instance ToRequest DeleteObject where
    type Scopes DeleteObject =
        '[ "https://www.googleapis.com/auth/devstorage.full_control"
         ]
    ...
{% endhighlight %}

The associated type-level list of symbols is used to represent a 'set' of scopes,
of which _one_ (preferrably with the least privilege) is required.


# Promoting Scopes

.. by Parameterising the `Token`, `Env`, and `Context` over the list of scopes
that were used to form the authorisation URL.

This parameter is introduced trivially, as:

{% highlight haskell %}
newtype Token s = Token ByteString

data Env s = Env
    { client  :: Client
    , token   :: Token s
    , manager :: Manager
    }

type Context s = ReaderT (Env s) IO

runContext :: Env s -> Context s a -> IO a
runContext = flip runReaderT
{% endhighlight %}

And a class is introduced to retrieve the scopes from our parameterised kind, `k -> *`:

{% highlight haskell %}
class GetScopes a where
    getScopes :: proxy a -> [Scope]

instance GetScopes '[] where
    getScopes _ = []

instance (KnownSymbol x, GetScopes xs) => GetScopes (x ': xs) where
    getScopes _ = scope (Proxy :: Proxy x) : getScopes (Proxy :: Proxy xs)
      where
        scope = Scope . LBS8.pack . symbolVal
{% endhighlight %}

This changes the signatures used to do the OAuth danse macabre to:

{% highlight haskell %}
formURL        :: GetScopes s => Client -> Proxy s            -> ByteString
redirectPrompt :: GetScopes s => Client                       -> IO (Code  s)
exchangeCode   ::                Client -> Code  s -> Manager -> IO (Token s)
{% endhighlight %}

# Ensuring Correspondence

Unfortunately, despite having the ability to infer the correct authorisation
URL based on the type of `Token` and `Env`, there is still a piece missing,
namely the use of each operation's scopes match up with the scopes the `Token`
was authorised for.

This can be acheived by adding a constraint to `send`, which requires that the
scopes over which the `Context` (and `Env`) are parameterised over contain at
least one of the scopes required by the request passed to `send`. That is, the
intersection of scopes the `Token` was authorised for, and the request's
supported scopes is not empty.

This can be achieved by testing if the supplied scope `a` is a member of the set `s`:

{% highlight haskell %}
type family HasScope (s :: [Symbol]) a :: Constraint where
    HasScope s a = (s `HasScope'` Scopes a) ~ 'True

type family HasScope' s a where
    HasScope' s         '[] = 'True
    HasScope' (x ': xs) a   = x ∈ a || HasScope' xs a

type family (∈) a b where
    (∈) x '[]       = 'False
    (∈) x (y ': xs) = x == y || x ∈ xs
{% endhighlight %}

The result of `HasScope` is a `Constraint`, which can be used directly on `send`:

{% highlight haskell %}
send :: (ToRequest a, HasScope s a) => a -> Context s (Response ByteString)
send x = asks manager >>= lift . Client.httpLbs (toRequest x)
{% endhighlight %}

This updates the example to:

{% highlight haskell %}
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
{% endhighlight %}

If an overly restrictive annotation is used, say `Env '[ReadOnly]`, the following
error will occur during compilation:

{% highlight haskell %}
...
{% endhighlight %}

# Future Improvements (Give Me a Lift in Your Delorean)

Having to explicitly annotate `Env` with the set of scopes seems superfluous, it'd
be nice to have the compiler infer the type of `Env`.

Over specification of scopes, that is, using `FullControl` in the above example
when only requiring `ReadOnly` is currently allowed. Optionally ensuring the
least priviledged set of scopes is used would also be an interesting possible
improvement.


# TL;DR
