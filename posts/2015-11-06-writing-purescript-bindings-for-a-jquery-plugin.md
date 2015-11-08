---
title: Writing PureScript Bindings for a jQuery Plugin
author: Philip Cunningham
tags: javascript, purescript
---

You've started a new project at work with a tight deadline. You're in charge of
picking the stack and after assessing technologies best for the project you've
decided to use [PureScript](http://www.purescript.org/) on the front-end. One
factor determining the success of the project is that it be delivered on time.

PureScript is the technology I chose for a new project at work and one of the
ways it's helping my company deliver our project on time is by allowing us to
leverage existing JavaScript libraries using its
[foreign function interface (FFI)](https://en.wikipedia.org/wiki/Foreign_function_interface).
The experience of writing FFI bindings has been so positive that I wanted to
share with you how you might write your own bindings for an existing
[jQuery](https://github.com/jquery/jquery) plugin.

In this post, together we'll be writing bindings for a subset of the
functionality of [jQuery-steps](https://github.com/rstaib/jquery-steps); a
lightweight wizard UI component written for jQuery. This wont be a deep dive
into PureScript's FFI but will give you an insight into some of the practical
considerations involved in using JavaScript code from PureScript. It assumes
familiarity with JavaScript, PureScript and jQuery.

## FFI

[PureScript's FFI](https://leanpub.com/purescript/read#leanpub-auto-the-foreign-function-interface)
allows communication from PureScript code to JavaScript code. It's
straight-forward and flexible, making it possible to write bindings for even the
most involved JavaScript code. Provided, that is, that you understand the
underlying runtime representation of what you're dealing with.

## Configuration

As is typical for jQuery plugins, jQuery-steps takes a configuration object to
tell it which DOM elements it should bind to. We begin by defining the types
we'll need to safely represent the fields of this configuration object in
PureScript.

``` haskell
module JQuery.Steps where

import Control.Monad.Eff
import Control.Monad.Eff.JQuery
import Control.Monad.Eff.Unsafe
import DOM
import Data.Foreign.Callback
import Data.Function
import Prelude

newtype HeaderTag = HeaderTag String
newtype BodyTag = BodyTag String
newtype CssClass = CssClass String
```

We use [newtype](https://leanpub.com/purescript/read#leanpub-auto-newtypes)
wrappers to represent the values of the fields in our configuration object. We
do this because newtypes have the same runtime representation as the underlying
type, whilst being distinct from the perspective of the type checker. This makes
newtypes an easy win for providing an extra layer of type safety when defining
FFI bindings.

``` haskell
type Config = { headerTag :: HeaderTag
              , bodyTag   :: BodyTag
              , cssClass  :: CssClass
              }
```

We use a
[type synonym](https://leanpub.com/purescript/read#leanpub-auto-defining-our-types)
to represent our configuration rather an an
[algebraic data type](https://leanpub.com/purescript/read#leanpub-auto-algebraic-data-types)
(ADT). Much like a newtype, the type synonym's runtime representation
will have a direct correspondence with what jQuery-steps expects, whereas an
ADT's constructors would present an extra level of indirection that we'd have to
handle in our JavaScript wrapper.

``` haskell
defaultConfig :: Config
defaultConfig = { headerTag: HeaderTag "h1"
                , bodyTag: BodyTag "div"
                , cssClass: CssClass "wizard"
                }
```

We define a default configuration object to let consumers of our bindings use
[record update syntax](https://leanpub.com/purescript/read#leanpub-auto-putting-row-polymorphism-to-work)
to override the defaults we've set.

``` javascript
var defaultConfig = {
      headerTag: "h1"
    , bodyTag: "div"
    , cssClass: "wizard"
}
```

Here's what **defaultConfig** will look like at runtime. Isn't it rather
lovely that we have that extra layer of type safety when working with our data
in PureScript, only for it to compile down to a representation that can be
passed directly to jQuery-steps?

``` javascript
/*global exports, jQuery*/

"use strict";

// module JQuery.Steps

exports.steps = function (object) {
    return function (config) {
        return function () {
            return jQuery(object).steps(config);
        };
    };
};
```

We define our export function that wraps jQuery-steps by providing a
[curried](https://leanpub.com/purescript/read#leanpub-auto-curried-functions)
interface.

``` haskell
foreign import steps :: forall eff. JQuery -> Config -> Eff (dom :: DOM | eff) Unit
```

And, finally, we write a foreign import declaration and we're done. It's really
as simple as that. Well, almost.

## Callbacks and Dishonesty

Binding to DOM elements is all well and good but very soon you'll want to add
dynamic behaviour such as client-side validation. This means delving into into
the unprincipled world of JavaScript callbacks and exposing the plugin's event
handler API.

``` haskell
type Config = { headerTag   :: HeaderTag
              , bodyTag     :: BodyTag
              , cssClass    :: CssClass
              , onFinished  :: Callback2 JQueryEvent Int Unit
              , onFinishing :: Fn3 JQueryEvent Int Int Boolean
              }
```

We begin by extending **Config** to include two new fields: **onFinished** and
**onFinishing**. The type constructor **Callback2** comes from
[purescript-foreign-callbacks](https://github.com/fluffynukeit/purescript-foreign-callbacks)
and represents an effectful computation that takes two arguments, whilst the
type constructor **Fn3** comes from
[purescript-functions](https://github.com/purescript/purescript-functions) and
represents a pure function that takes three arguments.

Something seems a bit odd here, why is **onFinished** effectful, whilst
**onFinishing** is pure? Well, the truth is that they are both effectful, but
since JavaScript offers no means of distinguishing between effectful and pure
computations, we need to circumvent the type checker to ensure functions we pass
to **onFinishing** have the correct runtime representation.

``` haskell
mkFinishing :: forall a. (JQueryEvent -> Int -> Int -> Eff a Boolean)
             -> Fn3 JQueryEvent Int Int Boolean
mkFinishing f = mkFn3 \x y z -> runPure <<< unsafeInterleaveEff $ f x y z
```

**mkFinishing** transforms our function into an appropriate runtime
representation by uncurrying it and by unsafely pulling the inner value out of
the **Eff** constructor. We uncurry our function because JavaScript functions
aren't curried by default and we pull the inner value out of **Eff** because
jQuery-steps expects a callback that returns a boolean value.

If at this point you're feeling a little uncomfortable, that's OK. Once a value
is inside **Eff** you shouldn't really be pulling it back out without a very
good reason. But we're dealing with JavaScript code here, and managing the
impedance mismatch so consumers of our bindings don't have to is probably reason
enough. In any case, the callback function wouldn't be very useful if it was
pure!

``` haskell
mkFinished :: forall a. (JQueryEvent -> Int -> Eff a Unit)
              -> Callback2 JQueryEvent Int Unit
mkFinished f = callback2 f
```

**mkFinished**, on the other hand, is much simpler. It uses
[callback2](https://github.com/fluffynukeit/purescript-foreign-callbacks/blob/5c6502fcb0b53e51b69c76e437cfa3e9e177ddf5/src/Data/Foreign/Callback.js#L15-L17)
to take our effectful computation of two arguments and transforms it into an
appropriate runtime representation for jQuery-steps.

``` haskell
defaultConfig :: Config
defaultConfig = { headerTag: HeaderTag "h1"
                , bodyTag: BodyTag "div"
                , cssClass: CssClass "wizard"
                , onFinished: mkFinished \_ _ -> return unit
                , onFinishing: mkFinishing \_ _ _ -> return true
                }
```

And, finally, Here's what the resulting **defaultConfig** looks like. Now we're
really done. That wasn't so bad, was it?

## Conclusion

PureScript lets us easily reuse existing JavaScript libraries with help from its
its FFI. Being able to reuse code can help us prototype more rapidly by allowing
us to stand on the shoulders of giants. Combining this property with a strongly
typed language can help us when that rapid prototype needs to make its way into
a production setting.
