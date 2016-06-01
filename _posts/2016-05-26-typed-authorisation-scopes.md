---
draft: true
layout: post
title: Typed Authorisation Scopes
categories:
  - Google
  - Haskell
  - OAuth
---

**DRAFT**

This post explores a small example of using Haskell's type system, specifically
[data type promotion](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#ghc-flag--XDataKinds),
[type-level literals](singletons), and
[type-level lists](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#promoted-list-and-tuple-types))
to implement compile-time checks that your supplied Google's OAuth2 credentials
have the required authorisation scopes for a particular monadic context.

Various assumptions about you, the reader, include familiarity with
Google's Services, Haskell and general HTTP API communication are made.

> You can find the entire code example as a runnable [stack](http://docs.haskellstack.org/) project [here](https://github.com/brendanhay/brendanhay.github.io/tree/master/projects/typed-authentication).
This is a minimal version of what will be released as part of
[gogol-0.2](http://hackage.haskell.org/package/gogol).

<h2>Contents</h2>
* TOC
{:toc}


## Introduction

Previously in the [`gogol-*`](http://hackage.haskell.org/packages/#cat:Google)
libraries, you would supply credentials to the top-level
[`runGoogle`](https://github.com/brendanhay/gogol/blob/0.0.1/gogol/src/Network/Google.hs#L174-L176)
function which ran the
[`Google`](https://github.com/brendanhay/gogol/blob/0.0.1/gogol/src/Network/Google.hs#L119)
monad and any remote API operations performed would be assumed to have the correct authorisation scopes,
else a run-time error from the API denoting forbidden or invalid access-levels would be raised.

> TODO: make the following more descriptive/specific.

This led to error prone ways to form the required OAuth2 flow URLs
to authorise the particular scopes that were used within a particular `runGoogle`
context.

> A short introduction to Google's use of OAuth2 can be read at the [bottom of this
article](#aside-googles-use-of-oauth2).

A contrived example of storing/retrieving an object using the Google Cloud
Storage API (referred to herein as Google Storage) is:

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
        -- Perform a remote 'ObjectsInsert' call, requiring at minimum the
        -- "https://www.googleapis.com/auth/devstorage.read_write" scope.
        _ <- upload (objectsInsert bkt obj & oiName ?~ key) bdy

        -- Perform a remote 'ObjectsGet' call, requiring at minimum the
        -- "https://www.googleapis.com/auth/devstorage.read_only" scope.
        _ <- download (objectsGet bkt key)

        pure ()
{% endhighlight %}

The problem with this approach is when the credentials are retrieved via
`newEnv` (calling the underlying
[getAuth](https://github.com/brendanhay/gogol/blob/0.0.1/gogol/src/Network/Google/Auth.hs#L203-L217))
there is no assurance that the code within the `runGoogle` context has the required
scopes authorised. If new credentials were created through the Google Developer
Console and we failed to authorise any scopes for Google Storage, when attempting
to run the above example a runtime error would occur.

Ryan Newton
[raised the idea](https://github.com/brendanhay/gogol/issues/7#issuecomment-151133749)
that having strongly typed scopes could mitigate some classes of invalid credentials, or at
least self-document the authorisation requirements a particular segment of
code has.


## An Oversimplification of the Domain

To illustrate usage of OAuth scopes for authorization purposes, we'll use a
faux Google Storage API. This simplified API will have 3 operations, each requiring a different scope:

* **GetObject** - Retrieve an existing object. (Addressed by storage bucket and object key.)
  - `"https://www.googleapis.com/auth/devstorage.read_only"`
  - `"https://www.googleapis.com/auth/devstorage.full_control"`
* **InsertObject** - Writes a new object.
  - `"https://www.googleapis.com/auth/devstorage.read_write"`
  - `"https://www.googleapis.com/auth/devstorage.full_control"`
* **DeleteObject** - Remove an existing object.
  - "https://www.googleapis.com/auth/devstorage.full_control"`

The 'real' Google Storage API accepts [multipart/related](https://tools.ietf.org/html/rfc2387) content-type
where object metadata and payload are sent in a delimited multipart stream. An `Accept` header
can then be used to retrieve either the metadata or payload using the same object key.

For our example, we'll just assume a streaming payload with no metadata where each
operation address an storage bucket and object prefix/key.


### Defining Credentials

Our faux API's authentication and authorisation will model the real API as closely
as possible. We'll assume credentials were created in the [Google Developers Console](https://console.developers.google.com/apis/credentials)
via **Create credentials** > **OAuth client ID** > **Other**, which results
in a generated client identifier and secret:

<img src="/public/images/typed-authentication/credentials.png" />

We will represent this client information in Haskell as the following data type:

{% highlight haskell %}
data Client = Client
    { identifier :: ByteString
    , secret     :: ByteString
    }
{% endhighlight %}

> The **Other** credentials type is used so we do not have to serve a callback URI
to obtain the authorisation code.


### Defining Operations

The common parameters for each operation can be modelled as newtypes with a
utility function `objectPath` to encode the full Google Storage path to an object
relative to the `https://www.googleapis.com` host:

{% highlight haskell %}
newtype Bucket = Bucket ByteString deriving (Eq, Show, IsString)
newtype Key    = Key    ByteString deriving (Eq, Show, IsString)

objectPath :: Bucket -> Key -> BS.ByteString
objectPath (Bucket bkt) (Key key) =
    LBS.toStrict ("storage/v1/b/" <> bkt <> "/o/" <> key)
{% endhighlight %}

Then for each operation, we'll declare a data type that represents the parameters
that will be sent to the `www.googleapis.com/storage/v1` API endpoint as part
of an HTTP request:

{% highlight haskell %}
data InsertObject = InsertObject Bucket Key RequestBody
data GetObject    = GetObject    Bucket Key
data DeleteObject = DeleteObject Bucket Key
{% endhighlight %}

Each operation is assumed to return a streaming HTTP response upon success.

> For this contrived example it could be considered overkill defining a data
type instead of a function per operation.  However, in an API where operations
have large numbers of parameters, passing only required parameters to
a smart constructor and defaulting the rest proves more palatable. A real-world example of this can
be found
[here](https://github.com/brendanhay/gogol/blob/0.0.1/gogol-container/gen/Network/Google/Resource/Container/Projects/Zones/Clusters/Create.hs#L80-L143).


### Defining Scopes

Similarly, an OAuth2 scope and the specific set we require for our faux Google Storage API
can be represented as:

{% highlight haskell %}
newtype Scope = Scope ByteString deriving (Eq, Show, IsString)

fullControl, readOnly, readWrite :: Scope
fullControl = "https://www.googleapis.com/auth/devstorage.full_control"
readOnly    = "https://www.googleapis.com/auth/devstorage.read_only"
readWrite   = "https://www.googleapis.com/auth/devstorage.read_write"
{% endhighlight %}

To perform HTTP requests against the remote API an `Authorization`
header containing a valid access token is required. This access token denotes
that we are operating on behalf of the client identified by our
[credentials](#credentials) for some authorised set of scopes.

This is done by firstly prompting the user to authorise desired scopes, yielding an
authorisation code. This authorisation code can then be exchanged along with our
credentials' secret to obtain an access token. You can read more about this particular OAuth2 flow [here](https://developers.google.com/identity/protocols/OAuth2InstalledApp).

Firstly, to obtain the authorisation code we encode the scopes into a URL:

{% highlight haskell %}
formURL :: Client -> [Scope] -> ByteString
formURL c ss =
       "https://accounts.google.com/o/oauth2/auth"
    <> "?response_type=code"
    <> "&redirect_uri=" <> redirectURI
    <> "&client_id="    <> identifier c
    <> "&scope="        <> queryEncodeScopes ss

queryEncodeScopes :: [Scope] -> ByteString
queryEncodeScopes =
      Build.toLazyByteString
    . mconcat
    . intersperse "+"
    . map (HTTP.urlEncodeBuilder True . LBS.toStrict)
    . coerce
{% endhighlight %}

Pasting the URL created by `formURL` into your browser, will prompt you to authorise
the specified scopes for the given client identifier:

<img src="/public/images/typed-authentication/authorisation.png" />

Clicking **Allow** will display an OAuth code that can be copied and pasted back into our toy application
via the following data type and a simple `IO` helper using `getLine`:

{% highlight haskell %}
newtype Code = Code ByteString deriving (Eq, Show, IsString)

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

This `Code` can then be exchanged along with the `Client` secret to obtain a
valid access token:

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
            [ (HTTP.hContentType, "application/x-www-form-urlencoded")
            ]
        , Client.requestBody    =
            RequestBodyLBS $
                   "grant_type=authorization_code"
                <> "&client_id="     <> identifier c
                <> "&client_secret=" <> secret     c
                <> "&code="          <> code
                <> "&redirect_uri="  <> redirectURI
        }
{% endhighlight %}

The `Token` can then be added to an HTTP `Request` by inserting the `Authorization` header:

{% highlight haskell %}
authorise :: Token -> Request -> Request
authorise (Token t) rq = rq
    { Client.requestHeaders =
        (HTTP.hAuthorization, LBS.toStrict ("Bearer: " <> t))
            : filter ((HTTP.hAuthorization /=) . fst)
                     (Client.requestHeaders rq)
    }
{% endhighlight %}

Assuming the planets align and horrors of the internet lie dormant, we now have a
valid access token to perform API requests for the authorised scopes.


## A Common Request Environment

> TODO: .. this adhoc overloading is improper - what about laws etc?

To provide a uniform interface for serialisation of our operation datatypes
into HTTP requests, a typeclass is introduced and an instance for each operation
is defined:

{% highlight haskell %}
class ToRequest a where
    toRequest :: a -> Request

instance ToRequest GetObject where
    toRequest (GetObject bkt key) =
        def { Client.secure = True
            , Client.host   = "https://www.googleapis.com"
            , Client.method = "GET"
            , Client.path   = objectPath bkt key
            }
...
{% endhighlight %}

Performing HTTP requests using
[http-client](https://hackage.haskell.org/package/http-client) requires the use
of a connection manager. This `Manager` and the valid exchanged `Token` will be
used to send a request containing the operation parameters and some additional
authentication information. This common environment can be made available
through use of a `ReaderT` to keep things tidy:

{% highlight haskell %}
data Env = Env
    { token   :: Token
    , manager :: Manager
    }

type Context = ReaderT Env IO

runContext :: Env -> Context a -> IO a
runContext = flip runReaderT
{% endhighlight %}

`Context` and `ToRequest` are then combined to provide a uniform interface to
send our operation data types and receive a raw HTTP response for `2xx` status codes:

{% highlight haskell %}
send :: ToRequest a => a -> Context (Response ByteString)
send x = do
    Env {..} <- ask
    lift $ Client.httpLbs (authorise token (toRequest x)) manager
{% endhighlight %}

When then tyes nicely into the following example:

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

One question we still haven't answered from the introduction is - how would we know the `Code` used
was authorised with the correct scopes for all three operations above? That is, either `fullControl`
*or* both `readOnly` and `readWrite` should have been authorised else
an error will occur when the example is run.

## Associating Requests with Their Respective Scopes

Since we know the scopes each respective request within a given `Context` requires,
how can we ensure the user is prompted with the correct scopes to authorise?

Firstly, we'll make a modification to the `ToRequest` class to annotate each request
with its respective scopes:

{% highlight haskell %}
class ToRequest a where
    type Scopes :: [Symbol]

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


## Promotion of Scopes

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


## Ensuring Correspondence

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

> TODO: Add note about how use of `HasScope` vs `HasScope'` emits nicer errors
due to the reduction (or lack thereof) of the LHS vs RHS.

The result of `HasScope` is a `Constraint`, which can be used directly on `send`:

{% highlight haskell %}
send :: (ToRequest a, HasScope s a) => a -> Context s (Response ByteString)
send x = do
    m <- asks manager
    t <- asks token
    lift $ Client.httpLbs (authorise t (toRequest x)) m
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


> TODO: Define some combinators to avoid ScopedTypeVariables.

> TODO: Note about the additional supported credential types in the `gogol-0.2` release.


## Future Improvements (Give Me a Lift in Your Delorean)

Having to explicitly annotate `Env` with the set of scopes seems superfluous, it'd
be nice to have the compiler infer the type of `Env`.

Over specification of scopes, that is, using `FullControl` in the above example
when only requiring `ReadOnly` is currently allowed. Optionally ensuring the
least priviledged set of scopes is used would also be an interesting possible
improvement.


## TL;DR


## Aside: Google's Use of OAuth2

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

If we made an attempt to write an object to Google Storage, we'd get an authorisation error
If an attempt was made to write an object to Google Storage and the credentials
used only had the above scope, an authorisation error would occur.

To allow write access (which naturally permits read access), the following scope
would be used:

{% highlight haskell %}
"https://www.googleapis.com/auth/devstorage.read_write"
{% endhighlight %}

And likewise, to perform further administrative operations on metadata and buckets
we would require the administrative scope:

{% highlight haskell %}
"https://www.googleapis.com/auth/devstorage.full_control"
{% endhighlight %}

Each of these scopes permits the previous, that is, they are typically hierarchial permissions.

For each API, Google exposes a number of differing scopes that allow granular control
over the operations an API client can perform.

> It is generally a best practice to request scopes incrementally, at the time
access is required, rather than up front. For example, an app that wants to
support purchases should not request Google Wallet access until the user
presses the “buy” button; see Incremental authorization. This is not (yet) handled
by `gogol`.
