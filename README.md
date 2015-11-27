hcached
=======

hcached is a very basic and still work-in-progress network-exposed, in-memory
hashtable to be used for simple caching. The hashtable is a size-limited one
just like [memcached](http://www.memcached.org/)'s one, where the maximum size
of the table can be specified at startup. When the size limit is reached,
hcached discards the least recently used key-value-pairs first to make space
for new ones.

### Memcached-compability

- [x] `set`
- [x] `add`
- [x] `replace`
- [x] `append`
- [x] `prepend`
- [ ] `cas`
- [x] `get`/`gets`
- [x] `delete`
- [ ] `incr`
- [ ] `decr`
- [x] `touch`
- [ ] `stats`
- [x] `flush_all`
- [x] `version`
- [ ] `verbosity`
- [x] `quit`

Installing
----------

hcached is built with [stack](https://www.stackage.org/) support, so building
and installing is as easy as

```
stack install
```

Usage
-----

hcached aims to be as compatible to memcached as possible, thus implementing
the same interface. All client-libraries that use a TCP-connection should work
for the supported commands (see checklist above). UDP is currently not
supported.

