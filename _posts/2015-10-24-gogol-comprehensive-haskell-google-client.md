---
layout: post
title: Gogol Preview Release
categories:
  - Google
  - Haskell
---

As a continuation of my master plan to erode legitimate reasons for management rejection of
languages like Haskell (due to say, library availability),
I'm pleased to announce the initial release of `gogol` - a Haskell suite of libraries for
interacting with the publicly available Google Service APIs.
These libraries are generated from the Google [Discovery Service](https://developers.google.com/discovery/)
and provided interface is stylistically similar to the `amazonka` [libraries](http://hackage.haskell.org/packages/#cat:AWS),
with separate libraries per API endpoint, the ability to construct a request from the
minimum required fields using a smart constructor, and a very lens oriented
interface.

Due to the namespacing of the Google API and how products are aligned in the documentation,
library names are somewhat non-intuitive. For example: The `androidpublisher`
API endpoint is branded as the `Google Play Developer API`, with the corresponding
Haskell library being named `gogol-android-publisher`. This scheme follows the [official client(s)](https://github.com/google/google-api-go-client),
and hopefully the inline Haddock links and description provided are sufficient to determine
which libraries match with what product.

The libraries can be obtained from:

* The [Google](http://hackage.haskell.org/packages/#cat:Google) category on Hackage.
* The [source repository](https://github.com/brendanhay/gogol) on GitHub.

Please keep in mind it's still a work in progress and I'll be working over the coming
weeks to iron out some of the rougher edges, as well as adding more examples and documentation.
