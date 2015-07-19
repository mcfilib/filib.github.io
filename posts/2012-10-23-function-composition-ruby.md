---
title: Function Composition in Ruby
author: Philip Cunningham
tags: ruby
---

We can monkey patch `Proc` to add an operator for function composition.

``` ruby
class Proc
  def comp(f, g)
    -> (*args) { f.(g.(*args)) }
  end

  def *(g)
    comp(self, g)
  end
end

multiply = -> (x, y) { x * y }
negative = -> (x) { -x }
composed = negative * multiply

composed.(9,9) # => -81

[2,4,6].zip([1,2,3]).map{ |x, y| composed.(x, y) } # => [-2, -8, -18]
```
