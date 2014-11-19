---
title: Euclidean Distance in Ruby
author: Philip Cunningham
tags: ruby
---

Short and sweet euclidean distance lambda in Ruby (1.9+).

```ruby
euclidean_distance = ->(p1, p2) do
  Math.sqrt(p1.zip(p2).map{ |a, b| a - b }.map{ |d| d * d }.reduce(:+))
end
```
