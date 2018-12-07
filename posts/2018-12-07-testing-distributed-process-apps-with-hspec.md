---
title: Testing distributed-process Apps Using Hspec
author: Philip Cunningham
tags: distributed-process, haskell, hspec, testing
---

`distributed-process` is a Haskell library that brings Erlang-style concurrency to Haskell. Whilst developing an application at work that uses it, I've been exploring novel ways to develop and test applications using approaches that are more common in object-oriented programming languages.

## Application

- two processes, which have state
- processes talk to one another
- messages

## Testing

- setup
- testing process registration
- test doubles
- expecting messages

## Conclusion

- lets us spec out the behaviour of our processes before we implement them
- lets us mock external resources
- lets us avoid setting up the whole system
- lets us keep tests fast

## Links
