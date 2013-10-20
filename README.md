# nock

A Clojure library that implements a Nock interpreter.

The Nock tutorial http://www.urbit.org/2013/08/22/Chapter-2-nock.html says the
best way to learn Nock is to write an interpreter for it. And here we are!

See http://urbit.org.

## Usage

```
(use 'nock.core :only (nock))
(nock [[[4 5] [6 14 15]] 0 7])
;; => (14 15)
```
