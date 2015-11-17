hcached
=======

hcached is a very basic and still work-in-progress network-exposed, in-memory
hashtable to be used for simple caching. The hashtable is a size-limited one
just like [memcached](http://www.memcached.org/)'s one, where the maximum size
of the table can be specified at startup. When the size limit is reached,
hcached discards the least recently used key-value-pairs first to make space
for new ones.

Installing
----------

hcached is built with [stack](https://www.stackage.org/) support, so building
and installing is as easy as

```
stack install
```

