# ig-haskell-client

Haskell implementation of IG's REST and Lightstreamer APIs

The Lightstreamer API implementation integrates against a fork of [jm4games's lightstreamer](https://github.com/peteryhwong/lightstreamer), which exposes ADT `TlsSettings` to allow TLS as well as the module `Lightstreamer.Streaming`. The fork also updates the package dependency `transformers` to `0.5.2.0`.
