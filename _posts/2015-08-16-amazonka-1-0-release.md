---
layout: post
title: Amazonka 1.0 Released
categories:
  - AWS
  - Haskell
---

After 4 months, nearly 900 commits and an inordinate number of `ExitFailure (-9)`
build errors, version `1.0` of the Haskell [Amazonka](http://hackage.haskell.org/packages/#cat:AWS)
AWS SDK has been released.

Some of the features include significant changes to the underlying
generation mechanisms, along with changes to the external surface APIs which are
outlined below.

Looking back at the initial commits for Amazonka show that it's taken 2 years
and nearly 3,300 commits reach this milestone. The entire suite now consists of
55 libraries over 200K LOC and is in use by a diverse set of individuals and
companies.

I'd like to thank everybody who contributed to the release. If you have feedback
or encounter any problems, please open a [GitHub issue](https://github.com/brendanhay/amazonka/issues),
reach out via the maintainer email located in the cabal files, or join the freshly
minted [Gitter chat](https://gitter.im/brendanhay/amazonka).

A whirlwind summary of some of the changes you can find in `1.0` follows, in
no particular order.

<h3>Contents</h3>
* TOC
{:toc}

### Errors

Previously the individual services either had a service-specific error type such as `EC2Error`,
a generated type, or shared one of the `RESTError` or `XMLError` types.

In place of these, there is now a single unified `Error` type containing HTTP,
serialisation and [service specific errors](https://github.com/brendanhay/amazonka/blob/c4c56a06b9ff7c2b7d168fedd337037a5a8d34ba/core/src/Network/AWS/Types.hs#L224-L231).

In addition to this change to the underlying errors, changes have also been made
to the exposed interfaces in `amazonka`, which commonly had signatures such as
`Either Error (Rs a)` and in turn the `AWST` transformer lifted this result into
an internal `ExceptT`.

Since the previous approach was not amenable to composition due to the concrete
`Error`, functional dependencies and instances `MonadError`/`MonadReader`, the
library still passes around `Either Error a` internally, but externally it
exposes a `MonadThrow` constraint and I recommend using `Control.Exception.Lens`
and the various `Prism`s available from [AsError](https://github.com/brendanhay/amazonka/blob/c4c56a06b9ff7c2b7d168fedd337037a5a8d34ba/core/src/Network/AWS/Types.hs#L262-L278)
to catch/handle specific errors.

For example:

{% highlight haskell %}
trying _Error (send $ ListObjects "bucket-name")
    :: Either Error ListObjectsResponse

trying _TransportError (send $ ListObjects "bucket-name")
    :: Either HttpException ListObjectsResponse

trying _SerializeError (send $ ListObjects "bucket-name")
    :: Either SerializeError ListObjectsResponse

trying _ServiceError (send $ ListObjects "bucket-name")
    :: Either ServiceError ListObjectsResponse
{% endhighlight %}

The individual service libraries now generate error matchers compatible
with the above idiom. For example, the [amazonka-dynamodb](https://github.com/brendanhay/amazonka/blob/c4c56a06b9ff7c2b7d168fedd337037a5a8d34ba/amazonka-dynamodb/gen/Network/AWS/DynamoDB/Types.hs#L312-L360)
library contains the following generated error matcher:

{% highlight haskell %}
_InternalServerError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerError = _ServiceError . hasCode "InternalServerError"
{% endhighlight %}

Which can be used in the same fashion as the previous example. Check out the individual
library's main service interface `Network.AWS.<ServiceName>` to see what error
matchers are available.


### Free Monad

The core logic of sending requests, retrieving EC2 metadata and presigning are
now provided by interpretations for a free monad. This works by the regular functions
exposed from `Network.AWS` and `Control.Monad.Trans.AWS` constructing layers of
a `FreeT Command` AST which will be interpreted by using `runAWS` or `runAWST`.

This allows for mocking AWS logic in your program by replacing any `runAWS` or
`runAWST` call with a custom interpretation of the `FreeT Command` AST.


### Network.AWS vs Control.Monad.Trans.AWS

Due to the previously mentioned changes to `Error` and `ExceptT` usage, the surface
API for the main modules offered by the `amazonka` library have changed somewhat.

Firstly, you'll now need to manually call `runResourceT` to unwrap any `ResourceT`
actions, whereas previously it was internalised into the `AWST` stack.

Secondly, errors now need to be explicitly caught and handled via the aforementioned
error/exception mechanisms.

The primary use case for `Network.AWS` is the fact that since `AWS` is
simply `AWST` specialised to `IO`, a `MonadAWS` type class is provided to automatically
lift the functions from `Network.AWS` without having to `lift . lift ...`
through an encompassing application monad stack.

But that said, `Network.AWS` is simply built upon `Control.Monad.Trans.AWS`, which in
turn is built upon `Network.AWS.Free`. All of these modules are exposed and most
of the functions compose with respect to `MonadFree Command m` constraints.


### Authentication

The mechanisms for supplying AuthN/AuthZ information have minor changes to
make the library consistent with the official AWS SDKs.

For example, when retrieving credentials from the environment the following
variables are used:

{% highlight haskell %}
export AWS_ACCESS_KEY_ID = "*****"
export AWS_SECRET_ACCESS_KEY = "*****"
export AWS_SESSION_TOKEN = "*****"
{% endhighlight %}

With `AWS_SESSION_TOKEN` being optional.

A credentials file is now also supported. It is located in
`$HOME/.aws/credentials` on Linux/OSX and `C:\\Users\<user>\.aws\credentials` on Windows.

It is `INI`-formatted and can contain the following keys per `[profile]` heading:

{% highlight haskell %}
[default]
aws_access_key_id     = *****
aws_secret_access_key = *****
aws_session_token     = *****
{% endhighlight %}

Multiple `[profile]` sections can co-exist and the selected profile is determined
by arguments to `getAuth`, with `[default]` being used for `Discover`.

You can read more information about the standard AWS credential mechanisms on
the [AWS security blog](http://blogs.aws.amazon.com/security/post/Tx3D6U6WSFGOK2H/A-New-and-Standardized-Way-to-Manage-Credentials-in-the-AWS-SDKs).


### Configuring Requests

[Service](https://github.com/brendanhay/amazonka/blob/c4c56a06b9ff7c2b7d168fedd337037a5a8d34ba/core/src/Network/AWS/Types.hs#L325-L334)
configuration such as endpoints or timeouts can be overridden per request via the
`*With` [suffixed functions](https://github.com/brendanhay/amazonka/blob/c4c56a06b9ff7c2b7d168fedd337037a5a8d34ba/amazonka/src/Network/AWS/Free.hs#L94-L100).
For example, changing the timeout to 10 seconds for a particular request:

{% highlight haskell %}
sendWith (svcTimeout ?~ 10) (getObject "bucket-name" "object-key")
{% endhighlight %}

In fact, since modifying timeouts and retry logic is so common, functions are provided
to do this for one or more actions in the form of:

* `once    ::            m a -> m a`
* `timeout :: Seconds -> m a -> m a`
* `within  :: Region ->  m a -> m a`



### Field Naming

The way lens prefixes are generated has been completely re-implemented. This is for a number
of reasons such as stability of ordering, stability of a historically selected
prefix with regards to introduced fields and a desire to reduce the number of suffixed
ordinals that needed to be introduced to disambiguate fields.

Additionally, casing mechanisms now universally treat an acronym such as `Vpc`
into the form of `VPC`. This is pervasive and consistent through naming of operations,
types, module namespaces, etc.

Both of these are breaking changes, but are considerably more future proof than
the previous implementation.


### Generator

The previous generator predominantly used textual template rendering to emit
Haskell declarations and a fair amount of logic was tied up in templating code.
The new(er) generator now constructs a Haskell AST and then pretty prints code
declarations. Actual layout, spacing and comments are still done by templates.

This results in less code, including templating logic and defers any sort
of formatting to tools like `hindent` and `stylish-haskell`.

As an artifact of these changes, it is now considerably slower. :)


### Additional Services

Since the initial public release of Amazonka, an additional 12 libraries have
been added to the suite, consisting of:

* `amazonka-cloudhsm` [Cloud HSM](https://aws.amazon.com/cloudhsm/)
* `amazonka-codecommit` [Code Commit](https://aws.amazon.com/codecommit/)
* `amazonka-codepipeline` [Code Pipeline](https://aws.amazon.com/codepipeline/)
* `amazonka-devicefarm` [Device Farm](https://aws.amazon.com/device-farm/)
* `amazonka-ds` [Directory Service](https://aws.amazon.com/directoryservice/)
* `amazonka-dynamodb-streams` [DynamoDB Streams](http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Streams.html)
* `amazonka-efs` [Elastic File System](http://aws.amazon.com/efs/)
* `amazonka-ecs` [Elastic Container Service](http://aws.amazon.com/ecs/)
* `amazonka-glacier` [Glacier Storage Service](http://aws.amazon.com/glacier/)
* `amazonka-ml` [Machine Learning Service](http://aws.amazon.com/machine-learning/)
* `amazonka-ssm` [Simple Systems Manager](http://docs.aws.amazon.com/ssm/latest/APIReference/Welcome.html)
* `amazonka-workspaces` [WorkSpaces](http://aws.amazon.com/workspaces/)

Many of these libraries have only been tested for trivial cases (as in, operations that won't cost me anything)
and feedback is needed from users to continue to improve the APIs.


### Miscellaneous Changes

* More consistent documentation.
* Removal of pre-release warnings.
* Many bug fixes. (Thanks to various contributors.)
* Addition of `Read`, `Show`, `Data`, `Typeable`, `Generic` for all types where possible, at
  the expense of the added possibility to break invariants.
* Better semantic consistency of optional vs required parameters for smart constructors.
  Unspecified parameters should not appear on the wire.
* CI now builds documentation for `develop` - [brendanhay.nz/amazonka-doc](http://brendanhay.nz/amazonka-doc).
* `Query` protocol services that submit `POST` requests now serialise the entirety of their
  contents as `application/x-www-form-urlencoded` to avoid URL length issues.
* Placeholder fixtures and tests are now generated for every request and response.
* Per project examples have been removed in favour of a single amazonka-examples project.
* All modules are now exported from `amazonka-core` but the interface is only considered
  stable with regard to other `amazonka-*` libraries. Any use of `amazonka-core` should be treated
  as if every module was `.Internal`.


### Supported GHC Versions

The currently supported GHC versions are `7.8.4` and `7.10`, built against
stackage `lts-2.*` and `nightly-*` respectively. The libraries will probably
work on `7.6.3` as well, but active testing is not done for reasons of personal scale.


### Cabal vs Stack

In place of `cabal sandbox`, `stack` is now used for all development due to the
multi-lib nature of the project. This has been a huge improvement to my
development workflow, but because of this testing with `cabal-install` has become
somewhat limited. For now, if you're trying to build the project from git, I suggest
sticking to `stack` and using the supplied `stack-*.yml` configurations.
