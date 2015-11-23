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

Usage
-----

When started, hcached listens on a user-defined port (default: 11211) which
accepts TCP connections. It exposes a protocol that accepts commands like this:

    <command> [<args>]

Each command needs to end with a newline. Regardless of the commands, there are
two general errors that can be returned:

    CLIENT_ERROR [<message>]

indicates that the command or arguments are invalid. A human-readable message
may be appended to specify the nature of the error.

    SERVER_ERROR [<message>]

indicates that something on the server end went wrong while executing the
command. A human-readable message may be appended to specify the nature of the
error.

There are currently three implemented commands:

### set

    set <key> <flags> <ttl> <value-size> <value>

sets a key-value pair

- `key` is the key under which the value will be saved. It needs to be a single
  word, that is without spaces, and can contain all ASCII values between 33 and
  126.
- `flags` is an arbitrary integer written out in decimal for storing metadata.
  It is not utilized by hcached in any way.
- `ttl` is the maximum time to live for this value, after which it will be
  discarded. It can be relative in seconds or absolute in UNIX time (even
  seconds only, no punctuation).
- `value-size` is the size of the value in bytes. It needs to match in order
  for the command to be valid.
- `value` is the value to set. It can contain arbitrary content as long as it
  is exactly as long as specified

Possible answers are:

    STORED

### get

    get <key>

gets a value for a given key

- `key` is the key to lookup. The same restrictions apply as for `set`.

Possible answers are:

    VALUE <flags> <value>

    NOT_FOUND

### delete

    delete <key>

forces deletion of a key-value-pair

- `key` is the key to delete. The same restrictions apply as for `set`.

Possible answers are:

    DELETED

    NOT_FOUND

