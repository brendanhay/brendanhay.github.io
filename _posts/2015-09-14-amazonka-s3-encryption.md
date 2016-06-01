---
layout: post
title: S3 Encryption Addons
categories:
  - Cryptography
  - Amazon
  - Haskell
---

I wanted to share an overview of a new library named `amazonka-s3-encryption`,
which was created to supplement `amazonka-s3` with client-side encryption.
Client-side encryption allows transmission and storage of sensitive
information (Data in Motion), whilst ensuring that Amazon never receives any of
your unencrypted data. Previously `amazonka-s3` only supported server-side encryption
(Data at Rest), which requires transmission of unencrypted data to S3. The cryptographic
techniques used within the library are modeled as closely as possible upon the
official AWS SDKs, specifically the Java AWS SDK. Haddock documentation is available
[here](http://brendanhay.nz/amazonka-doc/amazonka-s3-encryption/).

<h3>Contents</h3>
* TOC
{:toc}

## Chunked Encoding

The version 4 signing algorithm supports two modes for signing requests when communicating
with S3. The first requires a `SHA256` hash of the payload to calculate
the request signature and the second allows incremental signature calculation for
fixed or variable chunks of the payload. Up until now, `amazonka` (and all other SDKs excepting Java)
only supported the first method.

This poses a problem for encryption, where the need to calculate the `SHA256` hash
of the encrypted contents requires the use of a temporary file or another buffering
mechanism. For example, the `aws-sdk-ruby` library performs the following procedure
to send an encrypted `PutObject` request:

* Copy and encrypt the payload to a temporary file.
* Obtain the `SHA256` hash and file size of the encrypted file.
* Stream the file contents to the socket during transmission.

This means whatever the payload size is, you have to stream/encrypt a complete copy
of the payload contents to a temporary file before sending.

To avoid this same pitfall, `amazonka-s3` now uses [streaming signature calculation](http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-streaming.html)
when sending requests. This removes the need for the pre-calculated `SHA256` hash
and allows the encryption and signing to be performed incrementally as the request
is sent.

Unfortunately, despite the documentation claiming that `Transfer-Encoding: chunked`
is supported - it appears that you need to estimate the encrypted `Content-Length`
(including metadata) and send this without the `Transfer-Encoding` header, otherwise
the signature calculations simply fail with the usual obtuse S3 `403` response.

The smart constructors emitted by the generation step for all `amazonka-*` operations
now take into account streaming signature support and you're likely to encounter
the following parameters for operations utilising a streaming request body:

* `HashedBody` - A request body requiring a pre-calculated `SHA256` hash.
* `ChunkedBody` - A request body which supports streaming signature calculation.
* `RqBody` - A request body supporting any signing method.

`ToHashedBody` and `ToBody` type classes are provided to make it easy to convert
values such as `JSON`, `ByteString`, etc into the appropriate request body. `amazonka`
itself exports functions such as `hashedFile`, `chunkedFile` and others to assist
in constructing streaming request bodies.

All regular S3 `PutObject` and `UploadPart` operations now take advantage of
streaming signature calculation with the default chunk size set to `128 KB`. This seems
to be a decent trade off between streaming and the expense of incrementally performing
signature calculations, but I'd recommend profiling for your particular use-case
if performance and allocations are a concern.

> The above information is available in a more context sensitive format within the
[documentation](http://brendanhay.nz/amazonka-doc/amazonka-s3-encryption/index.html).


## Encryption and Decryption

Client-side encryption of S3 objects is used to securely and safely store sensitive
data in S3. When using client-side encryption, the data is encrypted _before_ it
is sent to S3, meaning Amazon does not receive your unencrypted object data. Unfortunately
the object metadata (headers) still leak, so any sensitive information should be
stored within the payload itself.


The procedure for encryption is as follows:

* A one-time-use symmetric key a.k.a. a data encryption key (or data key) and
initialisation vector (IV) are generated locally. This data key and IV are used
to encrypt the data of a single S3 object using an AES256 cipher in CBC mode,
with PKCS7 padding. (For each object sent, a completely separate data key and IV are generated.)

* The generated data encryption key used above is encrypted using a symmetric
AES256 cipher in ECB mode, asymmetric RSA, or KMS facilities, depending on the
client-side master key you provided.

* The encrypted data is uploaded and the encrypted data key and material description
are attached as object metadata (either headers or a separate instruction file).
If KMS is used, the material description helps determine which client-side master
key is later used for decryption, otherwise the configured client-side key at
time of decryption is used.

For decryption:

* The encrypted object is downloaded from Amazon S3 along with any metadata.
If KMS was used to encrypt the data then the master key id is taken from the
metadata material description, otherwise the client-side master key in the
current environment is used to decrypt the data key, which in turn is used
to decrypt the object data.

> If you're unsure about which key mechanism to use, I'd recommend using KMS initially
to avoid having to store and manage your own master keys.


## Instruction Files

By default, the metadata (known as an envelope) required for encryption
(except for the master key itself) is stored as S3 object metadata on the encrypted
object. Due to [user-defined S3 metadata](http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-metadata)
being limited to `8KB` when sending a `PUT` request, if you are utilising object
metadata for another purpose which exceeds this limit, an alternative method
of storing the encryption envelope in an adjacent S3 object is provided. This
method removes the metadata overhead at the expense of an additional HTTP request
to perform encryption/decryption. By default the library will store and retrieve
a `<your-object-key>.instruction` object if the related `*Instruction` suffixed
functions are used.


## Compatibility and Status

Metadata and instruction envelopes are designed to be compatible with the
official Java AWS SDK (both V1 and V2 formats), but only a limited set of the possible
encryption options are supported. Therefore assuming defaults, objects stored
with this library should be retrievable by any of the other official SDKs, and
vice versa. Support for other cryptographic configurations will be added in future,
as needed.

`amazonka-s3-encryption` can currently be considered an initial preview release.
Despite this, it's tied to the greater release process for the other `amazonka-*`
libraries and therefore life will start somewhere after version `1.3.1`.
It is separated from `amazonka-s3` proper, there are extra dependencies
not desirable within the main S3 package, such as `amazonka-kms` and
`conduit-combinators`. This way those using unencrypted S3 operations do not
inadvertantly end up with an `amazonka-kms` dependency.

The library is currently being used in a limited capacity and the release to
Hackage will be delayed until I'm confident of correctness, robustness and
compatibility aspects. If you're brave enough to experiment, it's contained within
the greater [amazonka](github.com/brendanhay/amazonka/issues) project on GitHub.
Please open an [issue](github.com/brendanhay/amazonka/issues) with any problems/suggestions
or drop into the [Amazonka Gitter chat](gitter.im/brendanhay/amazonka) if you have questions.
