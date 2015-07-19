---
title: List, Bag and Stack
author: Philip Cunningham
tags: haskell
---

``` haskell
import Prelude hiding (map, head, tail, length)

-- Simple Linked List

data List a = Empty | Cons a (List a) deriving Show

head :: List a -> a
head (Cons x xs) = x

tail :: List a -> List a
tail (Cons x xs) = xs

cons :: a -> List a -> List a
cons x xs = (Cons x xs)

map :: (a -> b) -> List a -> List b
map _ Empty       = Empty
map f (Cons x xs) = cons (f x) (map f xs)

length :: List a -> Int
length Empty       = 0
length (Cons x xs) = 1 + length xs

instance Functor List where
  fmap = map

-- Simple Bag

newtype Bag a = Bag (List a) deriving Show

add :: a -> Bag a -> Bag a
add x (Bag xs) = Bag (cons x xs)

bagSize :: Bag a -> Int
bagSize (Bag xs) = length xs

instance Functor Bag where
  fmap f (Bag xs) = Bag (map f xs)

-- Simple Stack

newtype Stack a = Stack (List a) deriving Show

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (cons x xs)

pop :: Stack a -> (a, Stack a)
pop (Stack xs) = ((head xs), Stack (tail xs))

peek :: Stack a -> a
peek (Stack xs) = head xs

stackSize :: Stack a -> Int
stackSize (Stack xs) = length xs

instance Functor Stack where
  fmap f (Stack xs) = Stack (map f xs)
```
