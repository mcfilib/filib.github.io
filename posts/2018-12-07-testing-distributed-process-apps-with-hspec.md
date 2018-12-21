---
title: Testing distributed-process Apps Using Hspec
author: Philip Cunningham
tags: distributed-process, haskell, hspec, testing
---

[`distributed-process`](https://github.com/haskell-distributed/distributed-process) is a Haskell library that brings Erlang-style concurrency to Haskell. Whilst developing an application at work that uses it, I found that there wasn't much material online describing how to test [`distributed-process`](https://github.com/haskell-distributed/distributed-process) applications. I used some techniques from object oriented programming that allowed me to test the behaviour of my application whilst I was learning how it was supposed to fit together. This post documents some techniques I found useful.

## Application

Our example revolves around a fairly simple [client-server](https://en.wikipedia.org/wiki/Client%E2%80%93server_model) application. The client process can send data to the server and output responses to the console, whilst the server performs calculations and sends the results back to clients.

``` haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}


module Main where


import Control.Distributed.Process hiding (onException)
import Control.Distributed.Process.Node
import Data.Binary
import GHC.Generics
import Network.Transport (Transport(..))
import Network.Transport.TCP
import Prelude (String)
import Protolude
import Test.Hspec


-- APP


app :: LocalNode -> Process ()
app node = do
  _ <- newProcess node "client" clientProcess
  _ <- newProcess node "server" serverProcess
  pure ()


-- PROCESSES


data ClientMsg =
      Ask [Int]
    | Result Int
    deriving (Eq, Generic, Show)

instance Binary ClientMsg


clientProcess :: Process ()
clientProcess = forever $ do
  msg <- expect

  case msg of
    Ask ints -> do
      self <- getSelfPid
      namedSend "server" $ Calc self ints

    Result n ->
      say $ "received: " <> show n


data ServerMsg =
    Calc ProcessId [Int]
  deriving (Eq, Generic, Show)


instance Binary ServerMsg


serverProcess :: Process ()
serverProcess = forever $ do
  msg <- expect

  case msg of
    Calc sender ints -> do
      send sender $ Result (sum ints)
```

Our example uses some helper functions that aren't present in [`distributed-process`](https://github.com/haskell-distributed/distributed-process) and are included in the code snippet below.

``` haskell
-- PROCESS HELPERS


-- | Fork process and register it
newProcess :: LocalNode -> String -> Process () -> Process ProcessId
newProcess node name process = do
  pid <- liftIO $ forkProcess node process
  _ <- register name pid
  pure pid


-- | Create a new transport
newTransport :: IO (Either IOException Transport)
newTransport =
  let
    host =
      "localhost"
    port =
      "3000"
  in
    createTransport host port (host,) defaultTCPParameters


-- | Spins up an application
run :: (LocalNode -> Process ()) -> IO (LocalNode, Transport)
run app = do
  eitherTrans <- newTransport

  case eitherTrans of
    Left err ->
      panic $ show err

    Right transport -> do
      node <- newLocalNode transport initRemoteTable
      _    <- runProcess node (app node)

      pure (node, transport)


-- | Sends to a named process
namedSend :: (Binary a, Typeable a) => String -> a -> Process ()
namedSend name msg = do
  mbPid <- whereis name

  case mbPid of
    Nothing ->
      say "process not registered"

    Just pid ->
       send pid msg
```

## Testing

We'll start off by writing a custom [HSpec](https://hspec.github.io/writing-specs.html) hook to communicate between our application and our tests. Our hook will spin up an application and thread around a shared [`MVar`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-MVar.html#t:MVar) and a [`LocalNode`](http://hackage.haskell.org/package/distributed-process-0.7.4/docs/Control-Distributed-Process-Node.html#t:LocalNode).

The [`MVar`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-MVar.html#t:MVar) serves two purposes; it will us to communicate state and act as locking mechanism ([`takeMVar`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-MVar.html#v:takeMVar) blocks until it's full). Whilst the [`LocalNode`](http://hackage.haskell.org/package/distributed-process-0.7.4/docs/Control-Distributed-Process-Node.html#t:LocalNode) will allow us to spin up adhoc processes when we need them.

The functions `aroundApp` and `withApp` are the first step in bridging these two worlds.

``` haskell
-- SPEC HELPERS


-- | Spins up and tears down app and passing along an mvar
aroundApp :: (MVar a -> LocalNode -> Process ())
          -> SpecWith (MVar a, LocalNode)
          -> Spec
aroundApp app =
  around $ withApp app


-- | Spins up application, closes it cleanly and passes along an mvar
withApp :: (MVar a -> LocalNode -> Process ())
        -> (((MVar a, LocalNode) -> IO ()) -> IO ())
withApp app action = do
  mvar              <- newEmptyMVar
  (node, transport) <- run $ app mvar
  _                 <- action (mvar, node) `onException` closeTransport transport
  closeTransport transport
```

The second is by defining a function that'll listen for messages that are sent to a process and put them in our [`MVar`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-MVar.html#t:MVar).

``` haskell
-- | Listens for messages and writes msg to an mvar
writer :: (Binary a, Show a, Typeable a) => MVar a -> Process ()
writer mvar = do
  msg <- expect
  liftIO $ putMVar mvar msg
```

Using these functions we'll write our first test. It's important that we're confident our application spins up all of the relevant processes it needs to function correctly. We'll do this by starting our application, checking whether the process is registered and putting the result in our [`MVar`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-MVar.html#t:MVar).

``` haskell
-- SPECS


main :: IO ()
main = hspec $ do
  let
    double mvar node = do
      _ <- app node
      x <- whereis "client"
      y <- whereis "server"
      liftIO $ putMVar mvar [x, y]

  aroundApp double $
    describe "app" $ do
      it "should spin up every process" $ \(mvar, _) -> do
        mbPids <- takeMVar mvar
        any isNothing mbPids `shouldBe` False
```

From here we'll want to test that our processes communicate with one another as we expect. We do this by starting a client process and registering a server [test double](https://martinfowler.com/bliki/TestDouble.html) that'll listen for messages sent to it using the `writer` function.

``` haskell
  let
    double mvar node = do
      _ <- newProcess node "client" clientProcess
      _ <- newProcess node "server" $ writer mvar
      pure ()

  aroundApp double $
    describe "Ask ints" $ do
      it "should call the server process" $ \(mvar, node) -> do
        _ <- forkProcess node $
          namedSend "client" (Ask [1, 2, 3, 4])

        Calc _ ints <- takeMVar mvar
        ints `shouldBe` [1, 2, 3, 4]
```

Finally we'll want to test our server process calculates results correctly and sends them back to clients. We do this by starting our server process and sending a message to it from a [test double](https://martinfowler.com/bliki/TestDouble.html) client process.

``` haskell
  let
    double mvar node = do
      void $ newProcess node "server" serverProcess

  aroundApp double $
    describe "Result i" $ do
      it "should call the client process with the result" $ \(mvar, node) -> do
        _ <- forkProcess node $ do
          pid <- newProcess node "client" $ writer mvar
          namedSend "server" $ Calc pid [1, 2, 3, 4]

        Result i <- takeMVar mvar
        i `shouldBe` 10
```

## Conclusion

The approach described in this post reflects some of my background in object oriented programming. After all, spinning up processes and testing messages passed between them feels very similar to instantiating objects and doing the same thing.

There are obviously some shortcomings to the techniques described — the big one being that the type checker doesn't complain when you send an unknown message to a process. That said, the approach [`distributed-process`](https://github.com/haskell-distributed/distributed-process) makes you to take is very consistent and makes it pleasant to write asynchronous applications.

Hopefully what I've written here offers some insight into how you might begin testing your [`distributed-process`](https://github.com/haskell-distributed/distributed-process) applications.
