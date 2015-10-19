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
API endpoint is branded as the Google Play Developer API, with the Haskell library
being `gogol-android-publisher`.  After weighing trade offs when attempting to coalesce
the libraries into logical supersets, the end result is library naming simply reflects the
related endpoint and the [official clients](https://github.com/google/google-api-go-client).

The libraries can be obtained from:

* The [Google](http://hackage.haskell.org/packages/#cat:Google) category on Hackage.
* The [source repository](https://github.com/brendanhay/gogol) on GitHub.

Please bear in mind it's still a work in progress and I'll be working over the coming
weeks to iron some of the rougher edges.
