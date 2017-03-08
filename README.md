# rss-conduit

This [Haskell][hsk] library implements a streaming parser/renderer for the [RSS 2.0 syndication format][rss], and a streaming parser for the [RSS 1.0 syndication format][rss1], based on [conduit][cdt]s.

Parsers are lenient as much as possible. E.g. unexpected tags are simply ignored.


[rss]: http://cyber.law.harvard.edu/rss/rss.html
[rss1]: http://web.resource.org/rss/1.0/spec
[cdt]: https://hackage.haskell.org/package/conduit
[hsk]: https://haskell.org
