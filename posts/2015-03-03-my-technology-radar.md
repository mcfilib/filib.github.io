---
title: My Technology Radar
author: Philip Cunningham
tags: radar
---

My decisions to learn different technologies have, up until very recently, been
fairly ad-hoc. I'd hear about some interesting or novel technology and bookmark
it to come back to later. Unsurprisingly this approach is not without its flaws.
The main one being that it lacks a clear strategy or structure, making it
difficult assess successes and shortcomings in any larger context.

I've decided to steal [Neal Ford's](http://nealford.com/) idea of building a
[technology radar](http://devchat.tv/ruby-rogues/195-rr-building-your-technology-radar-with-neal-ford)
in order to better structure my learning in the next six months. Without further
ado here are the technologies that are currently on my radar.

## Ansible

*trial*

I've used Chef and found the experience frustrating. I'm trialling Ansible to
see if turns out to be a smaller and simpler automation solution by piloting it
at work. I'm doing this by writing a playbook to manage deployment and
provisioning of a small Haskell web service that I wrote.

<http://www.ansible.com>

## Apache Kafka

*trial*

Kafka is [publish-subscribe](https://en.wikipedia.org/wiki/Publish%E2%80%93subscribe_pattern)
messaging rethought as a distributed [commit log](https://stackoverflow.com/questions/2582889/what-is-a-commit-log).
It looks like a mature and well-designed project and I hope to pilot it at work
by designing and implementing a real-time notification engine.

<https://kafka.apache.org>

## Haskell

*adopt*

I'm already totally sold on Haskell. The type system offers a powerful way to
capture invariants and explore solution space in a very abstract way, whilst the
type checker provides fast feedback about whether our programs adhere to these
invariants. I hope to write a lot more Haskell this year.

<https://www.haskell.org>

## Purescript

*assess*

I have to write a lot of JavaScript and I really don't like that I have to write
a lot of JavaScript. I'm hoping that Purescript's type system can help alleviate
some of this pain.

<http://www.purescript.org>

## Rust

*assess*

Rust's type system provides hope that someone like me could do systems
programming without shooting themselves in the foot. I suspect it's a language
worth learning before it hits version `1.0` in a few months' time.

<http://www.rust-lang.org>

## Snap

*assess*

Snap is web development framework written in Haskell. It occupies an
exciting position in relation to other Haskell web frameworks in that it's
larger than [Scotty](http://hackage.haskell.org/package/scotty) but smaller than
[Yesod](http://hackage.haskell.org/package/yesod). It has a few interesting
ideas, including an abstraction called [snaplets](http://snapframework.com/docs/tutorials/snaplets-tutorial)
that enable self-contained pieces of functionality to be easily composed.

<http://snapframework.com>

## Socket.IO

*trial*

As part of the real-time notification engine I'll be working on, I'm planning to
use Socket.IO on the client side. It looks like a fairly mature project and
there's a Haskell implementation of the protocol [available on Hackage](https://hackage.haskell.org/package/socket-io)
making it a nice fit with other technologies on my radar.

<http://socket.io>
