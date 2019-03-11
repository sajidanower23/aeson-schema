# aeson-schema [![Build Status](https://img.shields.io/travis/Fuuzetsu/aeson-schema.svg?style=flat)](http://travis-ci.org/timjb/aeson-schema) [![Hackage version](https://img.shields.io/hackage/v/aeson-schema.svg?style=flat)](http://hackage.haskell.org/package/aeson-schema) [![Hackage dependencies](https://img.shields.io/hackage-deps/v/aeson-schema.svg?style=flat)](http://packdeps.haskellers.com/feed?needle=aeson-schema)

aeson-schema is an implementation of the [JSON Schema specification](http://json-schema.org). It can be used in two ways:

* To validate JSON value against a schema.
* To generate a parser for a schema. The generated code includes Haskell data structure definitions and FromJSON instances. This allows you to use the validated data in a type-safe and convenient way.

You can install this library using cabal:

    cabal update && cabal install aeson-schema

## Running the tests

aeson-schema utilizes the cabal sandbox.  To get up and running with the tests you'll need to use these commands:

    cd aeson-schema
    git submodule init
    git submodule update
    cabal sandbox init
    cabal install
    cabal install --enable-tests
    cabal test

## Compatibility

`aeson-schema` implements [Draft 7](https://json-schema.org/understanding-json-schema/index.html) of the spec.
It supports all core schema definitions **_except_** the following
[format values](https://json-schema.org/understanding-json-schema/reference/string.html#format):

* date
* time
* uri-reference
* iri
* iri-reference
* uri-template
* json-pointer
* relative-json-pointer
* email
* idn-email
* ipv4
* ipv6
* hostname
* idn-hostname

I would be happy to accept pull requests that implement validation of one of these formats, fix bugs or add other features.

## Other libraries

The [hjsonschema](https://github.com/seagreen/hjsonschema) library is more actively maintained and implements the newer JSON Schema v4. It doesn't do code generation, however.
