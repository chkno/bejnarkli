## Bejnarkli

Reliable 'write-only' network storage.  Here, 'write-only' means that
the server's network interface only accepts writes.  To read the data,
access it from the server's filesystem.

Clients are encouraged to send data to at least two servers for
durability (eg: `bejnarkli-send --durability 2 ...`).  Servers are
typically configured (with `--peer`) to forward data amongst themselves,
so that data sent to any server shortly appears at all servers.

All network transit is authenticated by HMAC — servers and clients
must be configured with a shared secret.  Bejnarkli does not do
any encryption; encrypt your content before sending it or use
[stunnel](https://www.stunnel.org/).

"Bejnarkli" is [lujvo](https://jbovlaste.lojban.org/dict/lujvo)
of [benji](https://jbovlaste.lojban.org/dict/benji)
[na](https://jbovlaste.lojban.org/dict/na)
[klina](https://jbovlaste.lojban.org/dict/klina) .  Roughly:
_blob-sender_.
