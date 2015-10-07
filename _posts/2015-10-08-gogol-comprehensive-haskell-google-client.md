---
layout: post
title: Gogol Preview Release
categories:
  - Google
  - Haskell
---

Just briefly on the topic of Cloud API Clients and auto-generated code, I'm pleased
to announce the initial release of `gogol` - a Haskell suite of libraries for
interacting with the publicly available Google Service APIs.

These libraries are generated from the Google [Discovery Service](https://developers.google.com/discovery/).
The provided interface is stylistically similar to the `amazonka` libraries, with
a separate library per API endpoint, the ability to construct a request from the
minimum required fields using a smart constructor, as well as a very lens oriented
interface.

Due to the namespacing of the Google API and how products are aligned in the documentation,
library names are somewhat non-intuitive. For example: The `androidpublisher`
API endpoint is branded as the Google Play Developer API, with the Haskell library
being `gogol-android-publisher`.  After weighing trade offs when attempting to coalesce
the libraries into logical supersets, the end result is libraries simply reflect the endpoints,
and incidentally the [other official clients](https://github.com/google/google-api-go-client).

The libraries can be obtained from:

* The [Google](http://hackage.haskell.org/packages/#cat:Google) category on Hackage.
* The [source repository](https://github.com/brendanhay/gogol) on GitHub.

It's still a work in progress and the initial `0.0.1` release version reflects
the current production readiness. Until the surface API stabilises and more testing
is done I'd very much consider this to be a preview or experimental release.
