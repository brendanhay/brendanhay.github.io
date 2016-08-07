---
draft: true
layout: post
title: Managing Secure Credentials
categories:
  - Haskell
  - Amazon
---

Today I'd like to announce the release of a new Haskell library named
[credentials](https://hackage.haskell.org/package/credentials), which allows
you to securely retrieve shared encrypted credentials (secrets) from within your
Haskell applications. An administration CLI has also been released which allows
you to manage the credentials or access them from non-Haskell applications.

This work is based on Fugue's [credstash](https://github.com/fugue/credstash)
and is similar to HashiCorp's [vault](https://github.com/hashicorp/vault), albeit simplified.
It was motivated by work with my previous colleagues at [Fugue](https://fugue.co),
and a desire for similar functionally directly embeddable as a library in Haskell.

<h2>Contents</h2>
* TOC
{:toc}


## Introduction

When deploying modern applications either on-premise or within a cloud
environment such as AWS, you typically need access to a multitude of secrets
such as database credentials, API keys for external third parties, or
credentials for inter-service communication with our micro-service overlords.

For example, typical use cases might consist of:

* Retrieve a database connection URI such as
  `postgresql://domain.com/production?user=fred&password=secret` when a web
  application server starts.
* Retrieve a GitHub access token to push source code from an internal
  continuous integration server.

In this post I'll introduce the `credentials` library and some of the considerations
for managing secure storage of credentials, as well as the specific use of
Amazon's [Key Management Service (KMS)](http://aws.amazon.com/kms/) and
[DynamoDB](http://aws.amazon.com/dynamodb/).

The [credentials](https://hackage.haskell.org/package/credentials) library
is designed to rely on a minimal number of moving parts that are as close
to operations-free as possible.

In addition there is also a CLI, [credentials-cli](https://hackage.haskell.org/package/credentials-cli),
which can be used to setup the necessary DynamoDB table and administer your stored credentials.

Some of the features of the library and CLI include:

* Secure localised encryption and decryption of plaintext.
* Encrypted data at rest.
* Encrypted data in transit.
* Means for verifying data integrity.
* Sharing of credentials.
* Centralised management of credentials, such as creation, deletion, and querying.
* Revokation.
* Granular access control of all facets of the system.

In the following sections, I will go into detail about the use of KMS, DynamoDB,
and the actual encryption routine. If you just want to know how to use the library
or CLI, skip ahead to the [Usage](#usage) section.

## Key Management Service

Encryption and decryption of credentials use a master encryption
key, which usually creates the additional problem of how to securely store and
manage said key. When the key used to encrypt any number of credentials becomes compromised,
all credentials themselves are also compromised.

Secure administration and usage of the master keys is a difficult problem
typically requiring a solution such as dedicated Hardware Security Modules
(HSMs) to support features like revokation, renewal, and replacement.

KMS provides a programmatic API resembling a remote HSM. It offers a number of
useful features such as centralised key management, secure storage, key renewal
and revokation, as well as auditing via
[CloudTrail](https://aws.amazon.com/cloudtrail).

One basic model using the
[Encrypt](http://docs.aws.amazon.com/kms/latest/APIReference/API_Encrypt.html)
and
[Decrypt](http://docs.aws.amazon.com/kms/latest/APIReference/API_Decrypt.html)
API operations is similar to that of an HSM:

1. You generate a master key via the API or developer console. This key, once
   generated is never accessible, and the address is only used as a parameter to
   additional API calls via an Amazon Resource Name (ARN) or alias you assign
   when creating the key.

2. You send data to encrypt/decrypt to the API and say encrypt/decrypt this blob
   with the specified master key, and the resulting ciphertext or plaintext is
   returned.

This has two drawbacks. Firstly, your data is transmitted unencrypted to Amazon.
Secondly, each API operation has a `4KB` limit for the blob (plaintext or ciphertext)
you transmit.

To avoid this, we'll use a slightly different model based on the
[GenerateDataKey](http://docs.aws.amazon.com/kms/latest/APIReference/API_GenerateDataKey.html)
API operation:

1. You generate a master key via the API or developer console. This key, once
   generated is never accessible, and the address is only used as a parameter to
   additional API calls via an Amazon Resource Name (ARN) or alias you assign
   when creating the key.
2. Create a fresh data key using the `GenerateDataKey` API operation.
3. Use the data key to encrypt your data locally.
4. Use KMS to encrypt the data key with the specified master key. This is called
   key wrapping, and the encrypted data is now called a "wrapped key".
5. Store the encrypted data and the wrapped key.  Note: this is not "encraption"
   (the practice of storing a key next to the data that it protects), because
   without access to the master key that wraps the data key, the data key is
   useless. It is an opaque blob.

To decrypt the data, we now:

1. Retrieve the encrypted data and the wrapped key.
2. Use KMS to decrypt the wrapped key, to obtain the data key.
3. Use the data key to decrypt the encrypted data.

Hence the terminology "master key" when referring to the keys provided by
KMS. These keys are not infact used for encryption of your actual data, but
instead a single master key can be used to protect many data keys, which in
turn are used to encrypt your actual data.

> Current KMS charges are 1 USD per month for a single active master key.


## DynamoDB

Once credentials have been encrypted, the ciphertext and related encryption
parameters need to be stored in a centralised location to allow sharing, and
preferrably in storage that is operations-free.

Since one Amazon service is already in use (KMS), we'll use DynamoDB although
there is nothing preventing the use of other storage backends such as S3,
Postgres, or Redis outside of request latency, ciphertext size, etc.

To store the encrypted credentials, we'll support only insertion of a new
version of the contents for the given credential name. This allows use-cases
such as rolling out a new service while an existing version of the service is
still using a similarly older version of the same credential.

This dicates the schema for our DynamoDB table since we'll need to query via
the credential name, and possibility it's version.

To ensure the version is correctly incremented in the presence of concurrent writes
we'll use the following Optimistic Locking strategy:

1. Perform a
   [strongly consistent read](http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadConsistency.html)
   to obtain the latest version for name.
2. Increment the version.
3. Attempt to insert with a conditional check that the incremented version for the name
   doesn't exist.
4. On `ConditionalCheckFailedException` error response, delay and then retry by
   returning to step 1, otherwise exit successfully

Additionally since the internal representation of version is convenient as an integer
(for sorting, incrementing), if we support version deletion this sequence will be
non-monotonic if the latest is deleted, and will contain gaps if historical versions are deleted.

To hide this from the end user we'll instead return an opaque revision which can be
used along with the name as part of an efficient secondary index query.

> By default the library uses the minimum provisioned throughput when creating
the DynamoDB table. This qualifies for the free usage tier.


## Identity and Access Management

Granular access control of this scheme can be controlled via standard
Amazon Identity and Access Management (IAM) features, for example:

* Restriction of KMS master keys, such as production vs development.
* Restriction of different DynamoDB credential tables, such as production vs
  development, or individual credential names, via [DynamoDB conditions](
  http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/specifying-conditions.html).
* Limiting the API operations a specific AWS user may perform:
    - Grant DynamoDB `CreateTable` and `DeleteTable` only to administrators.
    - Grant KMS `GenerateDataKey` and DynamoDB `Query` / `PutItem` to power users.
    - Grant only KMS `GenerateDataKey` and DynamoDB `Query` to read only users.


## Encryption Routine

-- TODO: Investigate using DataKeySpec instead of manually specifying number of bytes

The encryption routine can be condensed into the following Haskell code:

```haskell
-- We call KMS to generate a 64 byte data key, which returns both
-- the ciphertext and plaintext variants of the key.
(ciphertextKey, plaintextKey) <- KMS.generateDataKey (64 bytes)

-- Then we split the plaintext key into two 32 byte parts, one is used
-- to initialise the block cipher and the other is to compute an
-- HMAC of the encrypted ciphertext.
(dataKey, hmacKey) = ByteString.splitAt 32 plaintextKey

-- A random nonce is generated to something about how this needs to be unique
-- for each encryption input to the ctr operation.
nonce  <- generateRandomBytes (16 bytes)
output <- ctrCombine (AES128 dataKey) (IV nonce) input
digest <- hmac hmacKey output

-- The following components are all stored in the storage backend,
-- and are made available to the decryption routine.
return ( nonce         -- The IV used to initialise the AES cipher.
       , ciphertextKey -- The encrypted data key from generateDataKey.
       , output        -- The resulting encrypted ciphertext.
       , digest        -- A digest used to check ciphertext integrity.
       )
```

It's important to make note of a few things here:

1. A randomly generated initialisation vector (IV).
2. An AES128 block cipher running in CTR mode.
3. Generating an HMAC digest of the encrypted ciphertext (Encrypt-then-MAC).

> There exists debate around the use of a random IV in relation to CTR mode vs CBC mode,
and likewise using an Encrypt-then-MAC scheme vs an authenticated block cipher mode.
See the following references for detailed explanations:

* [Cryptographic Right Answers](http://www.daemonology.net/blog/2009-06-11-cryptographic-right-answers.html)
* [Encrypt-then-MAC](http://www.daemonology.net/blog/2009-06-24-encrypt-then-mac.html)


## Usage

### Prerequisites

You will need your AWS credentials available in either the standard
`~/.aws/credentials` file, or as `AWS_ACCESS_KEY_ID` and
`AWS_SECRET_ACCESS_KEY` environment variables.

You will also need to create a KMS master key under Encryption Keys in the
Identity and Access Management section of the Amazon developer's console:

<img src="/public/images/credentials/encryption-keys-console.png" />

If you are likely to be using only one master key initially, it's recommended to
create a new key with the alias `credentials`, as that is what the tooling defaults to.

The complete list of AWS KMS and DynamoDB permissions the various operations will
require are:

* Setup and teardown:
    - `CreateTable`
    - `DestroyTable`
    - `ListTables`
* List credentials and revisions:
    - `Scan`
* Insert a new encrypted credential revision:
    - `GenerateDataKey`
    - `Query`
    - `PutItem`
* Select and decrypt an existing credential revision:
    - `GenerateDataKey`
    - `Query`
* Deletion of a credential revision, or revisions:
    - `Query`
    - `DeleteItem`

It's recommended you allow only the minimal set of permissions as your usecase
requires.

### Basic CLI Commands

Installation of the CLI can be achieved by either `cabal
install credentials-cli` or `stack install credentials-cli`. This will
installed an executable named `credentials` to either `cabal` or `stack`s
preferred location.

A basic example of using the CLI is as follows:

```
$ credentials setup
Setting up dynamo:///credentials in eu-central-1.
Running ...
dynamo://dynamodb.eu-central-1.amazonaws.com:443/credentials:
  status: created
Done.
```

```
$ credentials put --name foo --secret "A magical secret."
Writing new revision of foo to dynamo:///credentials in eu-central-1...
dynamo://dynamodb.eu-central-1.amazonaws.com:443/credentials:
  name: foo
  revision: 82687c4
Done.
```

```
$ credentials list
Listing contents of dynamo:///credentials in eu-central-1...
dynamo://dynamodb.eu-central-1.amazonaws.com:443/credentials:
  foo:
      - 82687c4 # latest
Done.
```

```
$ credentials get --name foo
Retrieving foo from dynamo:///credentials in eu-central-1...
dynamo://dynamodb.eu-central-1.amazonaws.com:443/credentials:
  name: foo
  revision: 82687c4
  secret: A magical secret.
Done.
```

Additional means of formatting the output and logging suitable for use in shell scripts is
available, see the `--help` text for more information.

### Library API

Once the KMS master key and DynamoDB table (via `credentials setup`) exists,
you can use the `credentials` library by adding it to the `build-depends`
section of your project's cabal file. The AWS credentials used for
authentication and authorisation are discovered by the underlying
[amazonaka](github.com/brendanhay/amazonka) library.

The basic API is defined as part of a type classes resembling the storage interface,
which for the default DynamoDB backend looks as follows:

```haskell
newtype DynamoDB a = DynamoDB { runDynamo :: AWS a }

newtype Table = Table Text
newtype Name  = Name  Text

setup     :: Table -> DynamoDB Setup
teardown  :: Table -> DynamoDB ()
revisions :: Table -> Conduit.Source DynamoDB (Name, NonEmpty Revision)
delete    :: Name  -> Maybe Revision -> Table -> DynamoDB ()

insert :: KeyId
       -> Context
       -> Name
       -> ByteString
       -> Table
       -> DynamoDB Revision

select :: Context
       -> Name
       -> Maybe Revision
       -> Table
       -> DynamoDB (ByteString, Revision)
```

Please see the [source](https://github.com/brendanhay/credentials) or
[documentation](https://hackage.haskell.org/package/credentials) for more
information.


## Future Work

* Leases and temporary credentials as well as access expiry.
* Different storage backends, such as S3 for large file storage.
* An audit trail for individual credential modification.
