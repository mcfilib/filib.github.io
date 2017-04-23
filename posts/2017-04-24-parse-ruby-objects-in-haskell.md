---
title: Parse Ruby Objects in Haskell
author: Philip Cunningham
tags: haskell, ruby
---

In 2015 I released my first Haskell project [ruby-marshal](https://www.reddit.com/r/haskell/comments/3enysr/my_first_haskell_project_a_ruby_marshal_parser/). It's a package that uses the [binary](https://hackage.haskell.org/package/binary) package to parse Ruby objects serialised with [Marshal.dump](https://ruby-doc.org/core-2.2.2/Marshal.html#method-c-dump). I wrote it in my spare time because I was curious to know whether I could devise a strategy to incrementally migrate legacy Ruby on Rails applications over to Haskell without the risk associated with a full rewrite.

My hypothesis was that if I could decrypt and de-serialise Rails sessions then I'd be able to piggyback on the Rails application's authentication mechanism. Not long after, I had the opportunity to use this package at work, and put this theory to the test, by writing a Haskell web application that shared sessions with Rails.

It has been running in production -- without any issue -- for almost two years.

## Marshal

Ruby's [Marshal](https://ruby-doc.org/core-2.2.2/Marshal.html) library serialises Ruby objects to a bytestring e.g. dumping `true` results in `[4, 8, 84]` where `4` and `8` are the [Marshal](https://ruby-doc.org/core-2.2.2/Marshal.html) version number and `true` is represented as `84` or ASCII `T`.

``` ruby
% irb
irb(main):001:0> x = Marshal.dump(true)
=> "\x04\bT"
irb(main):002:0> x.bytes
=> [4, 8, 84]
```

Compound objects, e.g. hash maps, can also be serialised using [Marshal.dump](https://ruby-doc.org/core-2.2.2/Marshal.html). This might explain why it was used as the default cookie serialiser in Rails until version 4.1, after which [JSON serialisation became the default](https://github.com/rails/rails/issues/12881).

``` ruby
% irb
irb(main):001:0> x = Marshal.dump("session_id" => "ba0844151d")
=> "\x04\b{\x06I\"\x0Fsession_id\x06:\x06ETI\"\x0Fba0844151d\x06;\x00T"
irb(main):002:0> x.bytes
=> [4, 8, 123, 6, 73, 34, 15, 115, 101, 115, 115, 105, 111, 110, 95, 105, 100, 6, 58, 6, 69, 84, 73, 34, 15, 98, 97, 48, 56, 52, 52, 49, 53, 49, 100, 6, 59, 0, 84]
```

More information about the [Marshal.dump](https://ruby-doc.org/core-2.2.2/Marshal.html) binary format can be found in a series of blog posts by [\@jakegoulding](http://jakegoulding.com/blog/2013/01/15/a-little-dip-into-rubys-marshal-format/) or by [reviewing the ruby-marshal source code](https://github.com/filib/ruby-marshal/blob/master/src/Data/Ruby/Marshal/Get.hs).

## Design

The [ruby-marshal](https://hackage.haskell.org/package/ruby-marshal) package allows us to transform this binary format into Haskell values and follows a pattern you'll see elsewhere in the Haskell ecosystem. It consists of:

- An abstract syntax tree ([AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree)) that represents Ruby objects.
- A collection of [parser combinators](https://en.wikipedia.org/wiki/Parser_combinator) to transform the [Marshal](https://ruby-doc.org/core-2.2.2/Marshal.html) binary representation into an [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree).
- A custom [monad](https://wiki.haskell.org/Monad) to enrich the underlying [Get monad](https://downloads.haskell.org/~ghc/8.0.1/docs/html/libraries/binary-0.8.3.0/Data-Binary-Get.html#t:Get) with additional effects.

### AST

The [Ruby AST](https://hackage.haskell.org/package/ruby-marshal-0.1.2/docs/Data-Ruby-Marshal-RubyObject.html#t:RubyObject) represents a subset of values that can be encoded by [Marshal.dump](https://ruby-doc.org/core-2.2.2/Marshal.html).

``` haskell
data RubyObject
  -- Simple objects.
  = RNil
  | RBool Bool
  | RFixnum Int
  | RFloat Float
  | RString ByteString
  | RSymbol ByteString
  -- Compound objects.
  | RArray (Vector RubyObject)
  | RHash (Vector (RubyObject, RubyObject))
  | RIVar (RubyObject, RubyStringEncoding)
  -- Tag for unsupported objects e.g. Bignum.
  | Unsupported
```

This is a common pattern you'll see in other packages e.g. [msgpack:Object](https://hackage.haskell.org/package/msgpack-1.0.0/docs/Data-MessagePack-Object.html#t:Object) and [aeson:Value](https://hackage.haskell.org/package/aeson-1.2.0.0/docs/Data-Aeson.html#t:Value).

### Parsers Combinators

[Parsers](https://en.wikipedia.org/wiki/Parser_combinator) are combined to build an [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree) e.g. parsing a raw bytestring is defined as follows.

``` haskell
getString :: Marshal BS.ByteString
getString =
  -- Label a parser to ensure label will be appended if the parse fails.
  marshalLabel "RawString" $ do
    -- Get the number of bytes in the bytestring.
    n <- getFixnum
    -- Get the number of bytes in the bytestring and lift it into the Marshal monad.
    liftMarshal $ getBytes n
```

It is then used by other parsing functions e.g. parsing a [Ruby symbol](https://ruby-doc.org/core-2.2.2/Symbol.html).

``` haskell
getSymbol :: Marshal BS.ByteString
getSymbol = marshalLabel "Symbol" $ do
  -- Get bytestring.
  x <- getString
  -- Write symbol into the symbol cache.
  writeCache $ RSymbol x
  -- Return the bytestring.
  return x
```

Before being used in the top level parsing function that combines parsing functions and lifts values in to the [Ruby AST](https://hackage.haskell.org/package/ruby-marshal-0.1.2/docs/Data-Ruby-Marshal-RubyObject.html#t:RubyObject).

``` haskell
getRubyObject :: Marshal RubyObject
getRubyObject =
  -- Make sure we're using a supported Marshal version, throw away the result and recursively parse our bytestring.
  getMarshalVersion >> go
  where
    go :: Marshal RubyObject
    go = liftMarshal getWord8 >>= \case
           NilChar        -> return RNil
           TrueChar       -> return $ RBool True
           FalseChar      -> return $ RBool False
           FixnumChar     -> RFixnum <$> getFixnum
           FloatChar      -> RFloat <$> getFloat
           StringChar     -> RString <$> getString
           SymbolChar     -> RSymbol <$> getSymbol
           ObjectLinkChar -> RIVar <$> getObjectLink
           SymlinkChar    -> RSymbol <$> getSymlink
           ArrayChar      -> RArray <$> getArray go
           HashChar       -> RHash <$> getHash go go
           IVarChar       -> RIVar <$> getIVar go
           _              -> return Unsupported
```

### Marshal Monad

A quirk of the [Marshal](https://ruby-doc.org/core-2.2.2/Marshal.html) format is that it saves space by encoding repeated objects as indexes into a symbol cache and an object cache. We use `StateT` to keep track of these during de-serialisation and enrich the underlying [Get monad](https://downloads.haskell.org/~ghc/8.0.1/docs/html/libraries/binary-0.8.3.0/Data-Binary-Get.html#t:Get) by creating a custom [monad](https://wiki.haskell.org/Monad).

``` haskell
newtype Marshal a = Marshal { runMarshal :: StateT Cache Get a }
  deriving (Functor, Applicative, Monad, MonadState Cache)
```

This allows us to write to and read from our cache during parsing without having to manually thread state through our parsing functions.

## Examples

### File IO

Let's take a simple example of a Ruby string, serialise it and dump it to the file system using `irb`.

``` ruby
% irb
irb(main):001:0> x = "hello haskell"
=> "hello haskell"
irb(main):002:0> y = Marshal.dump(x)
=> "\x04\bI\"\x12hello haskell\x06:\x06ET"
irb(main):003:0> File.open("example.bin", "w") { |z| z.write(y) }
```

Switching over to Haskell we set up our imports.

``` haskell
import Data.ByteString (ByteString)
import Data.Ruby.Marshal
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
```

Define a function to read our example from the file system.

``` haskell
readExample :: IO ByteString
readExample = BS.readFile "example.bin"
```

Define a function that uses the [Rubyable](https://hackage.haskell.org/package/ruby-marshal-0.1.2/docs/Data-Ruby-Marshal-RubyObject.html#t:Rubyable) typeclass to convert a [RubyObject](https://hackage.haskell.org/package/ruby-marshal-0.1.2/docs/Data-Ruby-Marshal-RubyObject.html#t:RubyObject) to a more convenient representation.

``` haskell
toString :: RubyObject -> Maybe (ByteString, RubyStringEncoding)
toString rubyObject = fromRuby rubyObject
```

Before putting it all together to print the Ruby string to the console.

``` haskell
main :: IO ()
main = do
  -- Read example.bin.
  example <- readExample

  -- Decode using ruby-marshal and maybe convert the result to a bytestring.
  case decode example >>= toString of

    -- Handle the case when serialisation fails.
    Nothing ->
      putStrLn "Oops, something went wrong..."

    -- Throw away the encoding information and print the Ruby string to the console.
    Just (string, _) ->
      Char8.putStrLn string
```

### Memcache

Let's take another example of de-serialising Ruby objects stored in [memcache](https://memcached.org/) using the the [dalli gem](https://github.com/petergoldstein/dalli).

``` ruby
% irb
irb(main):001:0> require "dalli"
=> true
irb(main):002:0> dc = Dalli::Client.new("localhost:11211")
=> #<Dalli::Client:0x007fe4948b7358 @servers=["localhost:11211"], @options={}, @ring=nil>
irb(main):003:0> dc.set("str", "hello haskell")
```

We'll reuse our existing Haskell code but add another import.

``` haskell
import qualified Database.Memcache.Client as M
```

Define a function that creates a new [memcache](https://memcached.org/) client.

``` haskell
createMemcacheClient :: IO M.Client
createMemcacheClient =
  M.newClient [M.ServerSpec "localhost" 11211 M.NoAuth] M.def
```

Before putting it all together to pull the value out of [memcache](https://memcached.org/) and print the Ruby string to the console.

``` haskell
main :: IO ()
main = do
  -- Set up memcache client.
  mc <- createMemcacheClient

  -- Retrieve bytestring from memcache server.
  example <- M.get mc "str"

  -- Unpack result from memcache server.
  case example of
    Nothing ->
      putStrLn "Oops, key not found..."

    -- Pattern match to extract bytestring.
    Just (value, _, _) ->

      -- Decode using ruby-marshal and maybe convert the result to a bytestring.
      case decode value >>= toString of

        -- Handle the case when serialisation fails.
        Nothing ->
          putStrLn "Oops, something went wrong..."

        -- Throw away the encoding information and print the Ruby string to the console.
        Just (string, _) ->
          Char8.putStrLn string
```

## Conclusion

By writing the [ruby-marshal](https://hackage.haskell.org/package/ruby-marshal) package, I was able to create a Haskell web application that coexisted with a Rails application. This approach has been a success at work and appears to be one way in which you could gradually migrate an existing web application written in Ruby over to Haskell without the risk associated with a full rewrite.

## Links

- [https://github.com/filib/ruby-marshal](https://github.com/filib/ruby-marshal)
- [https://hackage.haskell.org/package/ruby-marshal](https://hackage.haskell.org/package/ruby-marshal)
- [https://hackage.haskell.org/package/rails-session](https://hackage.haskell.org/package/rails-session)
