---
draft: true
published: false
layout: post
title: Typed Authentication
---

> **DRAFT**

> Example project here: https://github.com/brendanhay/brendanhay.github.io/tree/master/projects/typed-authentication

This post is a simplification of an approach used in the gogol libraries to
assist in the use of the cthulhic horror that is OAuth.

Various assumptions about you, the reader, include familiarity with
Haskell and general HTTP API communication.

# What Is This OAuth You Speak Of?

- Background
- Terminology, scopes

AuthN vs AuthZ and Pseudo-Authentication? Or just focus on the AuthZ aspects
for brevity.

Going to gloss over OAuth and Google's usage of it.



# Google's Use of OAuth

Obtain OAuth 2.0 credentials from the Google Developers Console.

The set of values varies based on what type of application you are
building. For example, a JavaScript application does not require a secret, but
a web server application does.

A single access token can grant varying degrees of access to multiple APIs. A
variable parameter called scope controls the set of resources and operations
that an access token permits. During the access-token request, your application
sends one or more values in the scope parameter.

Some requests require an authentication step where the user logs in with their
Google account. After logging in, the user is asked whether they are willing to
grant the permissions that your application is requesting. This process is
called user consent.

It is generally a best practice to request scopes incrementally, at the time
access is required, rather than up front. For example, an app that wants to
support purchases should not request Google Wallet access until the user
presses the “buy” button; see Incremental authorization.


Web Server Applications:
The authorization sequence begins when your application redirects a browser to
a Google URL; the URL includes query parameters that indicate the type of
access being requested. Google handles the user authentication, session
selection, and user consent. The result is an authorization code, which the
application can exchange for an access token and a refresh token.

Installed Applications:
Applications that are installed on devices such as computers, mobile devices, and tablets.
Flow is identical to Web Service Applications.

Client-side Applications:
JavaScript Applications that run in a user's browser.

Limited Input Devices:
Video Game Consoles, Cameras, Printers etc.

Service Accounts:
Server-to-server communication where identity is proven but no additional
consent is needed, such as a compute instance talking to the Google Cloud Storage APIs.

Focusing on the Web Service Application scenario, although Service Accounts would
also be suitable here.


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
