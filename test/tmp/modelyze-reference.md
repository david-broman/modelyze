# Modelyze Reference Manual

## Functions



## Data Types
Modelyze has four primitive data types: `Int`, `Bool`, `Real`, and `String`. 

There are four built in compound types: `Set`, `Map`, `Seq`, and `Tup`. 

### Set
A `Set` is an unordered collection of values. A set can be constructed directly using curly brackets. For example:

* `{3}` is the singleton set containing one integer element.
* `{"my","bag", "of", "words"}` is a set with four string elements.

All elements of a set must have the same type. The type of the element is given as an argument to the set type. For instance, `Set(Int)` is set of integers.

### Map
`Map(Int,String)` is a map type from integers to strings. Such a map can be constructed using curly brackets:

```
{7->"Anders", 88->"Sven", 324->"Olle"}
```
 
### Seq
A sequence may be viewed as either a list, an immutable array, or a vector. It is an ordered sequence of elements  of the same type. 

```
[1,4,1,4,1,5,773,1]
``` 

### Tup
`Tup` defines a tuple. For instance, `Tup(Int,Int,Bool)` defines a 3-tuple.


**TODO:** Explain the meaning `()`, `{}` and `[]`, that is, an empty set, map, sequence, and tuple. 

#Other
 
* Define sequences of  
  